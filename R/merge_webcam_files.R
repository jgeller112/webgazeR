#' Merge and Process Webcam Eye-Tracking Files
#'
#' Read, merge, and standardize webcam eye-tracking data from multiple platforms
#' into a single **long-format** data frame with columns:
#' `subject`, `trial`, `time`, `x`, `y` (+ any retained trial-level metadata).
#'
#' The function supports:
#'
#' - **Gorilla** (`kind = "gorilla"`): expects already-long data where `type == "prediction"`,
#'   then renames columns to the standardized schema using `col_map`.
#'
#' - **jsPsych** (`kind = "jspsych"`): supports either
#'   (a) jsPsych **JSON trials exports** that contain a top-level `data` object
#'   (e.g., `subject-<id>.json`), or
#'   (b) tabular files (.csv/.tsv/.xlsx) where each row is a trial and `array_col`
#'   contains per-trial eye samples (as a JSON string).
#'   For JSON files, if the subject column is missing, the function will infer it
#'   from the filename pattern `subject-<id>.json`.
#'
#' - **PsychoPy** (`kind = "psychopy"`): expects tabular trial-level data where `array_col`
#'   contains per-trial eye samples (list-column or JSON string).
#'
#' ## Eye sample formats in `array_col`
#' The per-trial sample array can be any of:
#' - **Unkeyed triplets**: `[[t, x, y], ...]` (or a 3-column matrix/data.frame)
#' - **Keyed objects**: `[{t=..., x=..., y=...}, ...]` or `[{time=..., x=..., y=...}, ...]`
#'
#' If `array_key = FALSE`, samples are treated as positional `(t, x, y)` and field
#' names are ignored. If `array_key = TRUE`, the parser uses keys (preferring
#' `col_map$time/x/y`, then falling back to `"time"/"t"`, `"x"`, `"y"`).
#'
#' @param file_paths Character vector of paths to files to read.
#' @param screen_index Optional. If provided, filters Gorilla data by one or more screen indices
#'   (requires a `screen_index` column in the data).
#' @param kind Data collection platform. One of `"gorilla"`, `"jspsych"`, `"psychopy"`.
#' @param col_map Named list mapping your file's column names to standardized names:
#'   `subject`, `trial`, `time`, `x`, `y`. For jsPsych/PsychoPy you usually only need
#'   `subject` and `trial` because `time/x/y` are read from the sample arrays.
#' @param array_col For `kind = "jspsych"` or `"psychopy"`: name of the column containing
#'   per-trial eye samples (list-column or JSON string). Required for these kinds.
#' @param array_key Logical. If `FALSE`, assume unkeyed triplets `(t,x,y)`. If `TRUE`, use keys
#'   when present.
#' @param trial_filter Optional function `f(df) -> df` to filter trial rows before parsing
#'   (useful for jsPsych; e.g., keep only trials that contain WebGazer samples).
#' @param out_dir Optional. If not `NULL`, writes one CSV per participant (subject) into this folder.
#' @param overwrite Logical. If `TRUE`, overwrite existing per-participant files in `out_dir`.
#' @param file_prefix Character. Prefix for per-participant output files written to `out_dir`.
#'
#' @return A data frame containing aggregated long-format eye data across all files.
#'
#' @examples
#' \dontrun{
#' # Gorilla (already-long predictions)
#' df <- merge_webcam_files(
#'   file_paths = "gorilla_export.csv",
#'   kind = "gorilla",
#'   col_map = list(
#'     subject = "participant_id", trial = "spreadsheet_row",
#'     time = "time_elapsed", x = "x", y = "y"
#'   )
#' )
#'
#' # jsPsych JSON trials export (subject inferred from filename if needed)
#' df <- merge_webcam_files(
#'   file_paths = "subject-6085bd39a5358.json",
#'   kind = "jspsych",
#'   col_map = list(subject = "subject_id", trial = "trial_index"),
#'   array_col = "webgazer_data",
#'   array_key = TRUE
#' )
#'
#' # jsPsych CSV (requires subject_id column in the CSV)
#' df <- merge_webcam_files(
#'   file_paths = "jspsych_export.csv",
#'   kind = "jspsych",
#'   col_map = list(subject = "subject_id", trial = "trial_index"),
#'   array_col = "webgazer_data",
#'   array_key = TRUE
#' )
#' }
#'
#' @export


merge_webcam_files <- function(
    file_paths,
    screen_index = NULL,
    kind = c("gorilla", "jspsych", "psychopy"),
    col_map = list(
      subject = "participant_id",
      trial   = "spreadsheet_row",
      time    = "time_elapsed",
      x       = "x",
      y       = "y"
    ),
    array_col = NULL,
    array_key = TRUE,
    trial_filter = NULL,
    out_dir = NULL,
    overwrite = FALSE,
    file_prefix = "eye_long") {
  kind <- match.arg(kind)

  # ---- make col_map robust (allow named vector or list) ----
  if (is.atomic(col_map) && !is.list(col_map)) col_map <- as.list(col_map)
  if (!is.list(col_map) || is.null(names(col_map))) {
    stop("col_map must be a *named* list (or named character vector) with names: subject, trial, time, x, y.")
  }
  col_map <- purrr::imap(col_map, ~ janitor::make_clean_names(.x))

  # ---------- helpers ----------
  is_blankish <- function(x) {
    if (is.null(x)) {
      return(TRUE)
    }
    if (length(x) == 0) {
      return(TRUE)
    }
    if (is.character(x) && length(x) == 1) {
      return(is.na(x) || x == "" || x %in% c("null", "[]"))
    }
    FALSE
  }

  pick_col <- function(nms, candidates) {
    candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
    if (length(candidates) == 0) {
      return(NULL)
    }
    hit <- intersect(candidates, nms)
    if (length(hit) == 0) NULL else hit[[1]]
  }

  # ---------- file readers (KIND-AWARE) ----------
  read_any <- function(path, kind, col_map) {
    ext <- tolower(tools::file_ext(path))

    if (ext == "csv") {
      return(readr::read_csv(path, show_col_types = FALSE))
    } else if (ext %in% c("tsv", "tab")) {
      return(readr::read_tsv(path, show_col_types = FALSE))
    } else if (ext %in% c("xlsx", "xls")) {
      return(readxl::read_excel(path))
    } else if (ext == "json") {
      x <- jsonlite::fromJSON(path, simplifyVector = TRUE)

      # --- jsPsych "subject-*.json" format: list(data=..., metadata=...) ---
      if (identical(kind, "jspsych") && is.list(x) && "data" %in% names(x)) {
        df <- tibble::as_tibble(x$data)

        # Add subject id from filename if not present
        subj_col_clean <- janitor::make_clean_names(col_map$subject)
        if (!(subj_col_clean %in% names(df))) {
          # subject-6085bd39a5358.json -> 6085bd39a5358
          subj_from_name <- sub("^subject-", "", tools::file_path_sans_ext(basename(path)))
          df[[subj_col_clean]] <- subj_from_name
        }

        return(df)
      }

      # If JSON is already a data.frame-like object
      if (is.data.frame(x)) {
        return(tibble::as_tibble(x))
      }

      stop("Unsupported JSON structure for kind='", kind, "': ", path)
    } else if (ext == "jsonl") {
      return(tibble::as_tibble(jsonlite::stream_in(file(path), verbose = FALSE)))
    } else {
      stop("Unsupported file type: ", path)
    }
  }

  # Normalize per-trial samples into tibble(time, x, y)
  parse_eye_samples <- function(samples, time_nm = NULL, x_nm = NULL, y_nm = NULL, array_key = TRUE) {
    if (is_blankish(samples)) {
      return(NULL)
    }

    # JSON string -> parse
    if (is.character(samples) && length(samples) == 1) {
      samples <- jsonlite::fromJSON(samples, simplifyVector = TRUE)
      if (is_blankish(samples)) {
        return(NULL)
      }
    }

    time_candidates <- unique(c(time_nm, "time", "t"))
    x_candidates <- unique(c(x_nm, "x"))
    y_candidates <- unique(c(y_nm, "y"))

    # matrix/data.frame
    if (is.matrix(samples) || is.data.frame(samples)) {
      df <- as.data.frame(samples)

      if (!array_key) {
        if (ncol(df) < 3) stop("array_key = FALSE requires at least 3 columns (t, x, y).")
        out <- df[, 1:3, drop = FALSE]
        names(out) <- c("time", "x", "y")
        return(tibble::as_tibble(out))
      }

      nms <- names(df)
      if (!is.null(nms) && length(nms) > 0) {
        time_key <- pick_col(nms, time_candidates)
        x_key <- pick_col(nms, x_candidates)
        y_key <- pick_col(nms, y_candidates)
        if (!is.null(time_key) && !is.null(x_key) && !is.null(y_key)) {
          out <- df[, c(time_key, x_key, y_key), drop = FALSE]
          names(out) <- c("time", "x", "y")
          return(tibble::as_tibble(out))
        }
      }

      # fallback: assume t,x,y
      if (ncol(df) < 3) stop("Eye samples must have at least 3 columns.")
      out <- df[, 1:3, drop = FALSE]
      names(out) <- c("time", "x", "y")
      return(tibble::as_tibble(out))
    }

    # list of numeric triplets -> assume t,x,y
    if (is.list(samples) && length(samples) > 0 && is.atomic(samples[[1]]) && length(samples[[1]]) >= 3) {
      mat <- do.call(rbind, lapply(samples, function(v) v[1:3]))
      df <- as.data.frame(mat)
      names(df) <- c("time", "x", "y")
      return(tibble::as_tibble(df))
    }

    # list of keyed objects
    if (is.list(samples) && length(samples) > 0 && is.list(samples[[1]])) {
      if (!array_key) {
        stop("array_key = FALSE but samples are keyed objects; cannot assume positional order.")
      }

      df <- tibble::as_tibble(samples)
      nms <- names(df)

      time_key <- pick_col(nms, time_candidates)
      x_key <- pick_col(nms, x_candidates)
      y_key <- pick_col(nms, y_candidates)

      if (is.null(time_key) || is.null(x_key) || is.null(y_key)) {
        stop("Keyed eye samples missing required time/x/y fields.")
      }

      out <- df[, c(time_key, x_key, y_key), drop = FALSE]
      names(out) <- c("time", "x", "y")
      return(out)
    }

    stop("Unrecognized eye samples format.")
  }

  # ---------- read + merge (now always returns a data frame per file) ----------
  merged_data <- purrr::map_dfr(
    file_paths,
    read_any,
    kind = kind,
    col_map = col_map
  ) %>%
    janitor::clean_names()

  # Optional pre-filter
  if (!is.null(trial_filter)) {
    merged_data <- trial_filter(merged_data)
  }

  # ============================================================
  # GORILLA
  # ============================================================
  if (kind == "gorilla") {
    if (!"type" %in% names(merged_data)) stop("Gorilla data require a 'type' column.")

    out <- merged_data %>%
      dplyr::filter(.data$type == "prediction") %>%
      dplyr::rename(
        subject = dplyr::all_of(col_map$subject),
        trial   = dplyr::all_of(col_map$trial),
        time    = dplyr::all_of(col_map$time),
        x       = dplyr::all_of(col_map$x),
        y       = dplyr::all_of(col_map$y)
      ) %>%
      dplyr::mutate(
        subject = as.character(.data$subject),
        trial   = as.character(.data$trial),
        time    = as.numeric(.data$time),
        x       = suppressWarnings(as.numeric(.data$x)),
        y       = suppressWarnings(as.numeric(.data$y))
      ) %>%
      dplyr::group_by(.data$subject, .data$trial, .data$time) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    if (!is.null(screen_index) && "screen_index" %in% names(out)) {
      out <- out %>% dplyr::filter(.data$screen_index %in% screen_index)
    }

    out <- out %>%
      dplyr::select(
        subject, trial, time, x, y,
        dplyr::any_of(c("screen_index", "type", "face_conf", "convergence"))
      )
  } else {
    # ============================================================
    # jsPsych / PsychoPy
    # ============================================================
    if (is.null(array_col) || is.na(array_col) || array_col == "") {
      stop("For kind = '", kind, "', you must provide array_col (column containing eye samples).")
    }
    array_col <- janitor::make_clean_names(array_col)

    required <- c(array_col, col_map$subject, col_map$trial)
    missing <- setdiff(required, names(merged_data))
    if (length(missing) > 0) {
      stop("Missing required columns for ", kind, ": ", paste(missing, collapse = ", "))
    }

    trial_cols <- setdiff(names(merged_data), array_col)

    out <- merged_data %>%
      dplyr::filter(!purrr::map_lgl(.data[[array_col]], is_blankish)) %>%
      dplyr::mutate(
        eyedata = purrr::map(
          .data[[array_col]],
          parse_eye_samples,
          time_nm = col_map$time,
          x_nm = col_map$x,
          y_nm = col_map$y,
          array_key = array_key
        )
      ) %>%
      tidyr::unnest(eyedata) %>%
      dplyr::rename(
        subject = dplyr::all_of(col_map$subject),
        trial   = dplyr::all_of(col_map$trial)
      ) %>%
      dplyr::mutate(
        subject = as.character(.data$subject),
        trial   = as.character(.data$trial),
        time    = as.numeric(.data$time),
        x       = suppressWarnings(as.numeric(.data$x)),
        y       = suppressWarnings(as.numeric(.data$y))
      ) %>%
      dplyr::select(
        subject, trial, time, x, y,
        dplyr::any_of(setdiff(trial_cols, c(col_map$subject, col_map$trial)))
      )
  }

  # ============================================================
  # Optional: write one file per participant
  # ============================================================
  if (!is.null(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    safe_subject <- function(x) {
      x <- as.character(x)
      x <- stringr::str_replace_all(x, "[^A-Za-z0-9_-]+", "_")
      x <- stringr::str_trim(x)
      x
    }

    subs <- sort(unique(out$subject))
    purrr::walk(subs, function(s) {
      one <- dplyr::filter(out, .data$subject == s)
      fn <- paste0(file_prefix, "_", safe_subject(s), ".csv")
      fp <- file.path(out_dir, fn)

      if (!overwrite && file.exists(fp)) {
        return(invisible(NULL))
      }

      readr::write_csv(one, fp)
      invisible(NULL)
    })
  }

  out
}
