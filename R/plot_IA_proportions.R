#' Plot Proportion of Looks Over Time for Interest Areas
#'
#' This function generates a plot of the proportion of looks over time for specified Interest Areas (IAs).
#' It optionally facets the plot by a condition column. The user can provide custom labels for each IA.
#'
#' @param data A data frame containing the data to be plotted.
#' @param ia_column The name of the column containing Interest Areas (IAs).
#' @param time_column The name of the column representing time (e.g., in milliseconds).
#' @param proportion_column The name of the column containing the proportion of looks to each IA.
#' @param condition_column (Optional) The name of the column representing the experimental condition.
#' If not provided, the plot will not be faceted by condition.
#' @param ... Mappings of IA names to custom labels (e.g., IA1 = "target", IA2 = "unrelated").
#'
#' @return A ggplot2 plot showing the proportion of looks over time for each IA, optionally faceted by condition.
#' @examples
#' # Example with a condition column
#' plot_IA_proportions(gaze_data, ia_column = "condition", time_column = "time_ms",
#'   proportion_column = "proportion_looks", condition_column = "condition",
#'   IA1 = "target", IA2 = "cohort", IA3 = "rhyme", IA4 = "unrelated")
#'
#' # Example without a condition column
#' plot_IA_proportions(gaze_data, ia_column = "condition", time_column = "time_ms",
#'   proportion_column = "proportion_looks", IA1 = "target", IA2 = "cohort",
#'   IA3 = "rhyme", IA4 = "unrelated")
#'
#' @export

plot_IA_proportions <- function(data, ia_column, time_column, proportion_column, condition_column = NULL, ...) {

  # Capture the mappings from the ellipsis (...) in a named list
  ia_mapping <- list(...)

  # Get the unique values in the IA column
  unique_ias <- unique(data[[ia_column]])

  # Filter out mappings where the condition does not exist in the data
  valid_ia_mapping <- ia_mapping[names(ia_mapping) %in% unique_ias]

  if (length(valid_ia_mapping) == 0) {
    stop("None of the specified IA mappings exist in the data.")
  }

  # Map the custom IA labels to the unique Interest Areas in the data
  data <- data %>%
    mutate(IA_label = recode(.data[[ia_column]], !!!valid_ia_mapping))  # Map the IAs using recode

  # Set IA_label as a factor with levels in the order specified by ia_mapping
  data$IA_label <- factor(data$IA_label, levels = unname(valid_ia_mapping))

  # Create the base plot
  p <- ggplot(data, aes_string(x = time_column, y = proportion_column, color = "IA_label")) +
    geom_line(size = 1.2) +          # Line plot with specified line width
    scale_y_continuous(limits = c(0, 1)) +  # Set y-axis from 0 to 1
    labs(
      x = "Time since word onset (ms)",   # Custom x-axis label
      y = "Proportion of Looks",          # Custom y-axis label
      color = "Interest Area"             # Legend title
    ) +
    theme_minimal(base_size = 14) +  # Set base font size to 14
    theme(
      axis.title = element_text(face = "bold"),   # Bold axis titles
      axis.text = element_text(face = "bold")     # Bold axis labels
    )

  # If a condition column is provided, facet by condition
  if (!is.null(condition_column)) {
    p <- p + facet_wrap(as.formula(paste("~", condition_column)))  # Create separate plots for each condition
  }

  # Return the plot
  return(p)
}
