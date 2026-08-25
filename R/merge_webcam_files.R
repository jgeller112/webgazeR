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
#'   contains per-trial eye samples (list-column or JSON string). `time` is always converted
#'   from seconds to milliseconds, since PsychoPy's clocks report elapsed time in seconds.
#'
#' - **Labvanced** (`kind = "labvanced"`): combines Labvanced's per-experiment
#'   **timeseries export** (one row per logged event, long-format, with columns
#'   including `Trial_Nr`, `Trial_Id`, `Task_Name`, `Exp_Subject_Id`,
#'   `variable_name`, `timestamp`, `value`) with its **trials export** (one row
#'   per trial, containing trial-level metadata) and, if present, its
#'   **sessions export** (one row per recording session, with screen/window
#'   size in pixels). Files are told apart automatically by their columns, so
#'   `file_paths` can list them in any order, the trials and sessions files
#'   are each optional (with no trials file, only the standardized
#'   `subject`/`trial`/`time`/`x`/`y`/`confidence` columns are returned), and
#'   any file that's none of the three is ignored with a message rather than
#'   silently merged in. You can pass every subject's files together in one
#'   call (e.g. everything under a study folder via
#'   `list.files(..., recursive = TRUE)`). Only `variable_name == "gaze_data"`
#'   rows are kept.
#'
#'   If a sessions export is supplied, `Screen_Width_In_Pixels`,
#'   `Screen_Height_In_Pixels`, `Window_Width_In_Pixels`, and
#'   `Window_Height_In_Pixels` (whichever are present -- not the rest of the
#'   file, which carries mostly browser/timing metadata unrelated to gaze
#'   data) are joined onto every matching row by `Rec_Session_Id`. Since
#'   `Rec_Session_Id` is the join key, a sessions file doesn't need to live in
#'   the same folder as the participant it describes.
#'
#'   Labvanced's `Trial_Nr` is a *per-task* counter, not a session-global one:
#'   a `sentence` task and a later `sentence_DC` task each have their own
#'   `Trial_Nr` sequence starting back at 1, so the same `Trial_Nr` (and even
#'   the same `Trial_Id`, which turns out to be a per-task randomized
#'   item-order id rather than a stable identity) can land on unrelated trials
#'   across different tasks or different subjects' sessions -- and since a
#'   task can be reused across multiple blocks, `Trial_Nr` isn't guaranteed
#'   not to also reset per block in some study designs. Rather than bet on
#'   exactly which level of Labvanced's session > block > task > trial
#'   hierarchy resets the counter, `trial` defaults to
#'   `"<Block_Name>_<Task_Name>_<Trial_Nr>"` (using whichever of those three
#'   columns are actually present), and trial metadata is joined on
#'   `Rec_Session_Id` + `Block_Name` + `Task_Name` + `Trial_Nr` -- so a trial
#'   can never be confused with an unrelated one from another block, task,
#'   session, or subject.
#'
#'   Labvanced's timeseries CSV only names columns through `value`, but each
#'   `gaze_data` row packs three further, unnamed values onto the end of the
#'   line. This function always splits those four trailing values into
#'   `x`, `y`, `t` (a high-precision per-sample timestamp), and `c` (a gaze
#'   confidence score, kept as `confidence`). Some `gaze_data` events (e.g.
#'   right as tracking starts up for a trial) carry no sample at all -- those
#'   rows are dropped rather than kept as `NA`, so the returned data only
#'   ever contains rows with real `x`/`y`/`time`/`confidence` values.
#'
#'   `t` itself is an absolute Unix epoch timestamp in ms (confirmed in
#'   Labvanced's own docs: it's the raw camera-capture timestamp, not
#'   trial-relative on its own), so it's re-expressed as `time`: milliseconds
#'   elapsed from 0 at that trial's first *actual* gaze sample, matching how
#'   Gorilla/jsPsych report `time`. This deliberately doesn't anchor to the
#'   trials export's `trial_start` instead: `Trial_Start` isn't an official
#'   Labvanced system variable, so a `trial_start` column is whatever a given
#'   study's own condition table happened to define it as, not something
#'   guaranteed to exist or mean the same thing across studies. Anchoring to
#'   the first gaze sample only ever needs the timeseries export itself, so
#'   it works the same way for every Labvanced study.
#'
#' ## Eye sample formats in `array_col`
#' The per-trial sample array can be any of:
#' - **Unkeyed triplets**: `[[t, x, y], ...]` (or a 3-column matrix/data.frame)
#' - **Keyed objects**: \verb{[\{t=..., x=..., y=...\}, ...]} or \verb{[\{time=..., x=..., y=...\}, ...]}
#'
#' If `array_key = FALSE`, samples are treated as positional `(t, x, y)` and field
#' names are ignored. If `array_key = TRUE`, the parser uses keys (preferring
#' `col_map$time/x/y`, then falling back to `"time"/"t"`, `"x"`, `"y"`).
#'
#' @param file_paths Character vector of paths to files to read.
#' @param screen_index Optional. If provided, filters Gorilla data by one or more screen indices
#'   (requires a `screen_index` column in the data).
#' @param kind Data collection platform. One of `"gorilla"`, `"jspsych"`, `"psychopy"`, `"labvanced"`.
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
#' @param webgazer_target_names Optional named character vector for `kind = "jspsych"` JSON
#'   files that recorded per-trial target/AOI zones in a `webgazer_targets` field -- either
#'   the webgazer extension's own `targets` parameter (a nested object keyed by CSS selector,
#'   e.g. `"#scenes"`, each value itself `x`/`y`/`width`/`height`/`top`/`right`/`bottom`/`left`),
#'   or a plugin that logs one row per target with an identifying column instead (e.g.
#'   `jspsych-free-sort`'s draggable zones, keyed by `selector`; `id`, `name`, `target`, `roi`,
#'   `label`, and `aoi` are also recognized). This function always flattens `webgazer_targets`,
#'   whichever shape it's in, into flat `<selector>_<field>` columns, e.g. `scene_roi_x`,
#'   `scene_roi_y`, `scene_roi_width`, etc. -- never left as a nested object, and works the same
#'   way even if different trials use different shapes, selector sets, or omit `webgazer_targets`
#'   entirely. By default each selector's columns are named from the raw selector string via
#'   `janitor::clean_names()` (e.g. `"#scenes"` becomes `number_scenes`, giving `number_scenes_x`,
#'   `number_scenes_y`, ...). Supply a vector where names are the raw selectors and values are
#'   the desired column names, e.g. `c("#scenes" = "scene_roi")`, to control that naming instead.
#'   Selectors not present in `webgazer_target_names` keep the default cleaned name.
#' @param task For `kind = "labvanced"` only: optionally restrict to a single Labvanced
#'   `Task_Name` (e.g. `"sentence"`). Within one task, `Trial_Nr` alone is a stable,
#'   familiar numeral trial id, so supplying `task` makes `trial` just that numeral
#'   (e.g. `"1"`, `"2"`, ...) instead of the safe-but-verbose default composite id.
#'   Errors if the task spans more than one block (where `Trial_Nr` alone wouldn't be
#'   unique) or if `task` doesn't match any `Task_Name` in the data.
#' @return A data frame containing aggregated long-format eye data across all files.
#'   `webgazer_targets` is always flattened into flat columns (see
#'   `webgazer_target_names`), never left nested. Any *other* remaining nested
#'   column (e.g. jsPsych fields like `mouse_events`, `final_locations`,
#'   `raw_gaze`, `validation_points`) is collapsed into a single string per
#'   row so the result is always flat and CSV-writable: a flat, unnamed vector
#'   becomes pipe-delimited text, while a data frame or named/nested list
#'   (structured, per-record data) is serialized as JSON so its field names
#'   and row grouping survive instead of being scrambled by a plain
#'   `unlist()`.
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
#' # PsychoPy CSV (requires subject_id column in the CSV); `time` is always
#' # converted from seconds to ms, since PsychoPy's clocks log seconds
#' df <- merge_webcam_files(
#'   file_paths = "psychopy_export.csv",
#'   kind = "psychopy",
#'   col_map = list(subject = "subject_id", trial = "trial_index"),
#'   array_col = "gaze_data",
#'   array_key = TRUE
#' )
#'
#' # Labvanced: combine a timeseries export with a trials export
#' # (order doesn't matter, and the trials file is optional)
#' df <- merge_webcam_files(
#'   file_paths = c("timeseries.csv", "trials.csv"),
#'   kind = "labvanced"
#' )
#'
#' # Labvanced, restricted to one task: `trial` becomes a plain numeral
#' # ("1", "2", ...) instead of the default "<block>_<task>_<trial_nr>" id
#' df <- merge_webcam_files(
#'   file_paths = c("timeseries.csv", "trials.csv"),
#'   kind = "labvanced",
#'   task = "sentence"
#' )
#' }
#'
#' @export


merge_webcam_files <- function(
    file_paths,
    screen_index = NULL,
    kind = c("gorilla", "jspsych", "psychopy", "labvanced"),
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
    file_prefix = "eye_long",
    webgazer_target_names = NULL,
    task = NULL) {
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

  # `webgazer_targets` shows up in two different shapes depending on how a
  # trial recorded it:
  #  - "extension" shape: a named list/record keyed by CSS selector, each
  #    value itself a flat record of fields (x, y, width, height, ...) --
  #    what the webgazer extension's own `targets` parameter produces.
  #  - "long" shape: a data frame with one row per target and a `selector`
  #    column identifying it, plus flat field columns (top, bottom, left,
  #    right, ...) -- what e.g. the jspsych-free-sort plugin produces.
  # Both get normalized to the same (selector, field...) long form here, so
  # one flattening step spreads either into `<selector>_<field>` columns.
  webgazer_target_id_candidates <- c("selector", "id", "name", "target", "roi", "label", "aoi")

  webgazer_targets_to_long <- function(v) {
    if (is_blankish(v)) {
      return(NULL)
    }
    if (is.data.frame(v)) {
      id_col <- pick_col(names(v), webgazer_target_id_candidates)
      if (is.null(id_col) || nrow(v) == 0) {
        return(NULL)
      }
      names(v)[names(v) == id_col] <- ".selector"
      return(v)
    }
    if (is.list(v) && length(v) > 0 && !is.null(names(v)) && all(nzchar(names(v)))) {
      return(purrr::imap_dfr(v, function(rec, sel) {
        tibble::as_tibble(as.list(rec)) %>% dplyr::mutate(.selector = sel)
      }))
    }
    NULL
  }

  flatten_webgazer_targets_row <- function(v, webgazer_target_names) {
    long <- webgazer_targets_to_long(v)
    if (is.null(long)) {
      return(NULL)
    }

    # Apply user-provided renaming, matched against the raw selector string
    # (e.g. "#scenes"); any selector not covered keeps its raw string and
    # gets cleaned up later by the pipeline's overall janitor::clean_names()
    # pass, same as every other column.
    sel <- long$.selector
    if (!is.null(webgazer_target_names)) {
      hit <- match(sel, names(webgazer_target_names))
      sel[!is.na(hit)] <- webgazer_target_names[hit[!is.na(hit)]]
    }

    field_cols <- setdiff(names(long), ".selector")
    out <- list()
    for (i in seq_along(sel)) {
      for (fc in field_cols) {
        out[[paste0(sel[i], "_", fc)]] <- long[[fc]][i]
      }
    }
    tibble::as_tibble(out)
  }

  pick_col <- function(nms, candidates) {
    candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
    if (length(candidates) == 0) {
      return(NULL)
    }
    hit <- intersect(candidates, nms)
    if (length(hit) == 0) NULL else hit[[1]]
  }

  # ---------- Labvanced helpers ----------
  # Labvanced's per-sample "timeseries" export is a ragged CSV: the header only
  # names the file up through `value`, but rows carrying a gaze sample pack
  # THREE further, unnamed columns onto the end of the line (after `value`).
  # Together those four trailing values are (x, y, t, c): pixel x, pixel y,
  # a high-precision sample timestamp (t), and a confidence score (c). Rows
  # for other `variable_name`s (e.g. "applied_drift") don't carry all four,
  # so we always reserve x/y/t/c and leave any that are absent as NA.
  split_csv_line <- function(line) {
    # split on commas that are not inside a quoted field
    fields <- stringr::str_split(line, ",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)")[[1]]
    fields <- stringr::str_trim(fields)
    fields <- sub('^"(.*)"$', "\\1", fields)
    fields[fields == ""] <- NA_character_
    fields
  }

  is_labvanced_timeseries_file <- function(path) {
    if (tolower(tools::file_ext(path)) != "csv") {
      return(FALSE)
    }
    hdr <- janitor::make_clean_names(names(readr::read_csv(path, n_max = 0, show_col_types = FALSE)))
    all(c("variable_name", "timestamp", "value") %in% hdr)
  }

  is_labvanced_trials_file <- function(path) {
    if (tolower(tools::file_ext(path)) != "csv") {
      return(FALSE)
    }
    hdr <- janitor::make_clean_names(names(readr::read_csv(path, n_max = 0, show_col_types = FALSE)))
    "trial_id" %in% hdr
  }

  is_labvanced_sessions_file <- function(path) {
    if (tolower(tools::file_ext(path)) != "csv") {
      return(FALSE)
    }
    hdr <- janitor::make_clean_names(names(readr::read_csv(path, n_max = 0, show_col_types = FALSE)))
    "rec_session_id" %in% hdr &&
      !("trial_id" %in% hdr) &&
      !("variable_name" %in% hdr) &&
      any(c("screen_width_in_pixels", "screen_height_in_pixels") %in% hdr)
  }

  read_labvanced_timeseries <- function(path) {
    lines <- readr::read_lines(path)
    lines <- lines[nzchar(lines)]
    if (length(lines) < 2) stop("Empty labvanced timeseries file: ", path)

    header <- janitor::make_clean_names(split_csv_line(lines[1]))
    n_header <- length(header)

    rows <- lapply(lines[-1], split_csv_line)
    # always reserve 4 trailing slots (value -> x, plus y, t, c)
    n_out <- max(n_header + 3L, vapply(rows, length, integer(1)))
    rows <- lapply(rows, function(r) {
      length(r) <- n_out
      r
    })

    mat <- do.call(rbind, rows)
    df <- tibble::as_tibble(as.data.frame(mat, stringsAsFactors = FALSE), .name_repair = "minimal")
    names(df) <- c(header[seq_len(n_header - 1)], c("x", "y", "t", "c")[seq_len(n_out - n_header + 1)])

    numeric_cols <- intersect(
      c(
        "trial_nr", "trial_id", "block_nr", "task_nr", "exp_subject_id",
        "rec_session_id", "session_nr", "timestamp", "x", "y", "t", "c"
      ),
      names(df)
    )
    df %>% dplyr::mutate(dplyr::across(dplyr::all_of(numeric_cols), ~ suppressWarnings(as.numeric(.x))))
  }

  # ---------- file readers (KIND-AWARE) ----------
  read_any <- function(path, kind, col_map, webgazer_target_names = NULL) {
    ext <- tolower(tools::file_ext(path))

    if (ext == "csv") {
      return(readr::read_csv(path, show_col_types = FALSE))
    } else if (ext %in% c("tsv", "tab")) {
      return(readr::read_tsv(path, show_col_types = FALSE))
    } else if (ext %in% c("xlsx", "xls")) {
      return(readxl::read_excel(path))
    } else if (ext == "json") {
      x <- jsonlite::fromJSON(path, simplifyVector = TRUE)

      # --- jsPsych JSON export: either a bare array of trials, or
      # list(data = <array of trials>, metadata = ...) ---
      if (identical(kind, "jspsych")) {
        trials <- if (is.data.frame(x)) {
          x
        } else if (is.list(x) && "data" %in% names(x)) {
          x$data
        } else {
          stop("Unsupported JSON structure for kind='jspsych': ", path)
        }

        df <- tibble::as_tibble(trials)

        # Add subject id from filename if not present
        subj_col_clean <- janitor::make_clean_names(col_map$subject)
        if (!(subj_col_clean %in% names(df))) {
          # subject-6085bd39a5358.json -> 6085bd39a5358
          subj_from_name <- sub("^subject-", "", tools::file_path_sans_ext(basename(path)))
          df[[subj_col_clean]] <- subj_from_name
        }

        if ("webgazer_targets" %in% names(df)) {
          if (is.data.frame(df$webgazer_targets)) {
            # jsonlite's simplifyVector packs webgazer_targets into a genuine
            # nested data-frame column when its shape is uniform across every
            # trial (same selector(s), same fields) -- the "extension" case.
            # tidyr::unpack flattens that directly: first the selector-named
            # sub-columns, then each of those into flat `<selector>_<field>`
            # columns.
            df <- df %>% tidyr::unpack(webgazer_targets)

            if (!is.null(webgazer_target_names)) {
              rename_map <- webgazer_target_names[names(webgazer_target_names) %in% names(df)]
              if (length(rename_map) > 0) {
                df <- df %>% dplyr::rename(!!!stats::setNames(names(rename_map), rename_map))
              }
            }

            target_cols <- names(df)[purrr::map_lgl(df, is.data.frame)]
            if (length(target_cols) > 0) {
              df <- df %>% tidyr::unpack(dplyr::all_of(target_cols), names_sep = "_")
            }
          } else {
            # Otherwise webgazer_targets is a per-row list column -- its shape
            # varies from trial to trial (e.g. a different number of targets),
            # so jsonlite can't pack it uniformly. Flatten each row
            # individually instead.
            flat_rows <- purrr::map(df$webgazer_targets, flatten_webgazer_targets_row, webgazer_target_names = webgazer_target_names)
            if (any(!purrr::map_lgl(flat_rows, is.null))) {
              flat_rows <- purrr::map(flat_rows, function(r) if (is.null(r)) tibble::tibble(.rows = 1) else r)
              flat_df <- purrr::list_rbind(flat_rows)
              df <- df %>%
                dplyr::select(-webgazer_targets) %>%
                dplyr::bind_cols(flat_df)
            }
            # If no row's webgazer_targets matched a recognized shape, the
            # column is left as-is here; the pipeline's generic list-column
            # collapse (JSON serialization) handles it safely as a fallback.
          }
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

  # ============================================================
  # LABVANCED
  # ============================================================
  if (kind == "labvanced") {
    is_ts <- purrr::map_lgl(file_paths, is_labvanced_timeseries_file)
    is_trial <- purrr::map_lgl(file_paths, is_labvanced_trials_file)
    is_session <- purrr::map_lgl(file_paths, is_labvanced_sessions_file) & !is_ts & !is_trial

    # Anything that's neither a timeseries export (has variable_name/timestamp/
    # value), a trials export (has Trial_Id), nor a sessions export (has
    # Rec_Session_Id + screen size, no Trial_Id) is ignored rather than swept
    # into the trial metadata, where it would just add unmatched, all-NA
    # columns (and can collide with existing column names).
    ignored <- file_paths[!is_ts & !is_trial & !is_session]
    if (length(ignored) > 0) {
      message(
        "kind = 'labvanced': ignoring file(s) that look like neither a timeseries, ",
        "a trials, nor a sessions export: ",
        paste(basename(ignored), collapse = ", ")
      )
    }

    ts_paths <- file_paths[is_ts]
    trial_paths <- file_paths[is_trial & !is_ts]
    session_paths <- file_paths[is_session]

    if (length(ts_paths) == 0) {
      stop(
        "For kind = 'labvanced', at least one file must be a timeseries export ",
        "(a csv with 'variable_name'/'timestamp'/'value' columns)."
      )
    }

    # Labvanced's sessions export carries each participant's screen/window
    # size (in raw pixels, matching the gaze x/y units) -- useful for
    # normalizing coordinates or defining AOIs consistently across
    # participants with different screens. Unlike the trials export, a
    # sessions export isn't expected to live in the same folder as its
    # participant's other files (it may cover the whole study in one file),
    # so it's collected globally across all of file_paths and joined onto
    # every participant by Rec_Session_Id, rather than grouped by folder.
    # Only the screen/window size columns are pulled in -- not the rest of
    # the file (browser info, timing stats, etc.) -- so this can't reintroduce
    # the column collisions a blanket merge would risk.
    session_meta <- NULL
    if (length(session_paths) > 0) {
      session_meta <- purrr::map(session_paths, ~ readr::read_csv(.x, show_col_types = FALSE)) |>
        purrr::list_rbind() %>%
        janitor::clean_names() %>%
        dplyr::select(dplyr::any_of(c(
          "rec_session_id",
          "screen_width_in_pixels", "screen_height_in_pixels",
          "window_width_in_pixels", "window_height_in_pixels"
        ))) %>%
        dplyr::distinct(.data$rec_session_id, .keep_all = TRUE)
    }

    # Each participant's timeseries + trials export are combined together
    # *first*, one participant at a time, and only then are the participants'
    # results stacked -- e.g. data-raw/sub1/{timeseries.csv,trials.csv} and
    # data-raw/sub2/{timeseries.csv,trials.csv} are each fully processed on
    # their own before being row-bound. Participants are grouped by the
    # directory their files live in, so any folder layout works as long as
    # each participant's files share a folder; files handed in flat (no
    # subfolders) all fall into one group, same as before.
    process_one_participant <- function(dir_ts_paths, dir_trial_paths) {
      if (length(dir_ts_paths) == 0) {
        return(NULL)
      }

      ts <- purrr::map(dir_ts_paths, read_labvanced_timeseries) |> purrr::list_rbind()

      if (!is.null(trial_filter)) {
        ts <- trial_filter(ts)
      }

      # Optionally scope everything to a single Labvanced task (e.g.
      # task = "sentence"), which sidesteps the Trial_Nr-resets-per-task
      # ambiguity below entirely: within one task, Trial_Nr alone is a clean,
      # familiar numeral trial id -- as long as that task only lives in a
      # single block. If it turns out to span more than one, Trial_Nr alone
      # still wouldn't be unique, so that's checked for and reported clearly
      # rather than silently producing duplicate trial ids.
      if (!is.null(task)) {
        if (!"task_name" %in% names(ts)) {
          stop("`task` was supplied but the timeseries file has no Task_Name column.")
        }
        available_tasks <- unique(ts$task_name)
        ts <- ts[ts$task_name == task, , drop = FALSE]
        if (nrow(ts) == 0) {
          stop(
            "task = '", task, "' matched no rows. Available Task_Name values: ",
            paste(available_tasks, collapse = ", ")
          )
        }
        if (all(c("block_name", "trial_nr") %in% names(ts))) {
          n_trial_nr <- dplyr::n_distinct(ts$trial_nr)
          n_trial_nr_block <- nrow(dplyr::distinct(ts, .data$trial_nr, .data$block_name))
          if (n_trial_nr_block > n_trial_nr) {
            stop(
              "task = '", task, "' spans more than one block, so Trial_Nr alone ",
              "isn't unique within it. Leave `task` unset to get the safe composite ",
              "trial id (block + task + Trial_Nr) instead."
            )
          }
        }
      }

      subj_col <- if (!is.null(col_map$subject) && col_map$subject %in% names(ts)) col_map$subject else "exp_subject_id"
      trial_col <- if (!is.null(col_map$trial) && col_map$trial %in% names(ts)) col_map$trial else NULL

      # Labvanced's Trial_Nr is at least a *per-task* counter, not a
      # session-global one: e.g. a "sentence" task and a later "sentence_DC"
      # task each have their own Trial_Nr sequence starting back at 1, so
      # Trial_Nr=1 can legitimately refer to unrelated trials in different
      # tasks within the same session (confirmed on real Labvanced exports).
      # Trial_Id doesn't help either -- it turns out to be a per-task
      # randomized item-order id, not a stable trial identity, so the same
      # Trial_Id can land on unrelated trials in different tasks purely by
      # coincidence. Since a task can also be reused across multiple blocks,
      # and there's no guarantee Trial_Nr can't *also* reset per block in
      # some study designs, the default folds in every level of Labvanced's
      # own session > block > task > trial hierarchy that's actually present
      # in the file -- Block_Name, Task_Name, Trial_Nr -- rather than betting
      # on exactly which level resets the counter. Supplying `task` above
      # sidesteps all of this and gets a plain Trial_Nr instead.
      hierarchy_cols <- intersect(c("block_name", "task_name", "trial_nr"), names(ts))
      if (!is.null(trial_col)) {
        trial_vec <- as.character(ts[[trial_col]])
      } else if (!is.null(task) && "trial_nr" %in% names(ts)) {
        trial_vec <- as.character(ts$trial_nr)
      } else if (length(hierarchy_cols) > 0) {
        trial_vec <- purrr::pmap_chr(ts[hierarchy_cols], ~ paste(c(...), collapse = "_"))
      } else {
        trial_vec <- as.character(ts$trial_id)
      }

      gaze <- ts %>%
        dplyr::mutate(trial = trial_vec) %>%
        dplyr::filter(.data$variable_name == "gaze_data") %>%
        dplyr::rename(subject = dplyr::all_of(subj_col)) %>%
        dplyr::mutate(
          subject    = as.character(.data$subject),
          time       = as.numeric(.data$t),
          x          = suppressWarnings(as.numeric(.data$x)),
          y          = suppressWarnings(as.numeric(.data$y)),
          confidence = suppressWarnings(as.numeric(.data$c))
        ) %>%
        # Labvanced logs a `gaze_data` event with no sample attached yet
        # (e.g. right as gaze tracking starts up for a trial) before real
        # samples start arriving; those rows carry no x/y/t/c at all, so
        # drop them rather than keep them as NA rows -- both because they're
        # not real data, and so the trial-relative zero below lands on the
        # first *logged* sample rather than an empty placeholder before it.
        dplyr::filter(!is.na(.data$x) & !is.na(.data$y) & !is.na(.data$time))

      if (length(dir_trial_paths) > 0) {
        trial_meta <- purrr::map(dir_trial_paths, ~ readr::read_csv(.x, show_col_types = FALSE)) |>
          purrr::list_rbind() %>%
          janitor::clean_names()

        # Join on the same key that makes a trial unique: Rec_Session_Id (as
        # a safety net in case a participant's folder ever holds more than
        # one recording session -- it's not what keeps different
        # participants apart, that's already guaranteed by processing each
        # participant's folder on its own) + Block_Name + Task_Name +
        # Trial_Nr. Drop any leftover duplicate keys defensively so the join
        # can never multiply rows.
        join_keys <- intersect(
          c("rec_session_id", "block_name", "task_name", "trial_nr"),
          intersect(names(trial_meta), names(gaze))
        )

        if (length(join_keys) > 0) {
          trial_meta <- trial_meta %>%
            dplyr::distinct(dplyr::across(dplyr::all_of(join_keys)), .keep_all = TRUE) %>%
            dplyr::select(-dplyr::any_of(setdiff(
              c("trial_id", "rec_session_id", "trial_nr", "task_name", "block_nr", "task_nr", "block_name"),
              join_keys
            )))

          gaze <- gaze %>% dplyr::left_join(trial_meta, by = join_keys)
        }
      }

      if (!is.null(session_meta) && "rec_session_id" %in% names(gaze)) {
        gaze <- gaze %>% dplyr::left_join(session_meta, by = "rec_session_id")
      }

      # `time` starts out as the raw sample timestamp `t`, an absolute Unix
      # epoch value in ms (confirmed in Labvanced's own docs: `t` is the raw
      # camera-capture timestamp, not trial-relative on its own). Re-express
      # it as elapsed ms from 0 at that trial's first *actual* gaze sample,
      # matching how Gorilla/jsPsych report `time`. This deliberately doesn't
      # anchor to the trials export's `trial_start` instead: `Trial_Start`
      # isn't an official Labvanced system variable (it's absent from their
      # own system-variables table), so a `trial_start` column is whatever a
      # given study's own condition table happened to define it as -- not a
      # value that means the same thing, or even exists, across studies. The
      # first-gaze-sample anchor only ever needs the timeseries export
      # itself, so it works the same way for every Labvanced study.
      gaze <- gaze %>%
        dplyr::group_by(.data$subject, .data$trial) %>%
        dplyr::mutate(time = {
          anchor <- suppressWarnings(min(.data$time, na.rm = TRUE))
          if (is.infinite(anchor)) NA_real_ else .data$time - anchor
        }) %>%
        dplyr::ungroup()

      gaze
    }

    ts_by_dir <- split(ts_paths, dirname(ts_paths))
    trial_by_dir <- split(trial_paths, dirname(trial_paths))
    all_dirs <- union(names(ts_by_dir), names(trial_by_dir))

    gaze <- purrr::map(
      all_dirs,
      ~ process_one_participant(ts_by_dir[[.x]], trial_by_dir[[.x]])
    ) |> purrr::list_rbind()

    out <- gaze %>%
      dplyr::select(
        subject, trial, time, x, y,
        dplyr::any_of("confidence"),
        dplyr::any_of(setdiff(
          names(gaze),
          c(
            "subject", "trial", "time", "x", "y", "confidence",
            "t", "c", "value", "timestamp", "trial_nr", "trial_id",
            "block_nr", "task_nr", "rec_session_id", "session_nr", "exp_subject_id"
          )
        ))
      )
  } else {
    # ---------- read + merge (now always returns a data frame per file) ----------
    merged_data <- purrr::map(
      file_paths,
      read_any,
      kind = kind,
      col_map = col_map,
      webgazer_target_names = webgazer_target_names
    ) |> purrr::list_rbind() %>%
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
        # PsychoPy's clock always reports elapsed time in seconds (unlike
        # Gorilla/jsPsych/Labvanced, and the downstream sampling-rate/binning
        # functions, which all assume milliseconds), so convert.
        time = as.numeric(.data$time) * (if (kind == "psychopy") 1000 else 1),
        x       = suppressWarnings(as.numeric(.data$x)),
        y       = suppressWarnings(as.numeric(.data$y))
      ) %>%
      dplyr::select(
        subject, trial, time, x, y,
        dplyr::any_of(setdiff(trial_cols, c(col_map$subject, col_map$trial)))
      )
    }
  }

  # Collapse any remaining list-columns (e.g. jsPsych validation-trial fields
  # like raw_gaze, percent_in_roi, average_offset, validation_points, or
  # preload's failed_images/failed_audio/failed_video, or free-sort-plugin
  # fields like mouse_events, final_locations, webgazer_targets) into a
  # single, CSV-writable string per row, so nothing gets dropped.
  #
  # A flat, unnamed vector (e.g. a plain list of validation_points numbers)
  # collapses to simple pipe-delimited text. A data.frame or named/nested
  # list -- e.g. one row per mouse event, each with its own type/object/t
  # fields -- carries structure that a plain unlist()+paste() would scramble
  # (every field's values interleaved with no way to tell which belongs to
  # which record), so those are serialized as JSON instead, which keeps the
  # field names and row grouping intact and human-parseable.
  collapse_list_cell <- function(v) {
    if (is.null(v) || length(v) == 0) {
      return(NA_character_)
    }
    if (is.data.frame(v) || (is.list(v) && !is.null(names(v)))) {
      return(as.character(jsonlite::toJSON(v, auto_unbox = TRUE, na = "null")))
    }
    paste(unlist(v), collapse = "|")
  }

  list_cols <- names(out)[purrr::map_lgl(out, is.list)]
  if (length(list_cols) > 0) {
    out <- out %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(list_cols),
        ~ purrr::map_chr(.x, collapse_list_cell)
      ))
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
