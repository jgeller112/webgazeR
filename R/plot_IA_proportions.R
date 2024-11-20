#' Plot Proportion of Looks Over Time for Interest Areas (IAs)
#'
#' This function creates a time-course plot of the proportion of looks to specified Interest Areas (IAs).
#' Optionally, it can facet the plot by an experimental condition. Custom labels for each IA can be specified
#' through the `ia_mapping` argument to define the display order.
#'
#' @param data A data frame containing the data to plot.
#' @param ia_column The name of the column containing Interest Area (IA) identifiers.
#' @param time_column The name of the column representing time (e.g., milliseconds).
#' @param proportion_column The name of the column with the proportion of looks for each IA.
#' @param condition_column Optional. The name of the column representing experimental conditions.
#' If not provided, the plot will not be faceted by condition.
#' @param ia_mapping A named list specifying custom labels for each IA in the desired display order
#' (e.g., `list(IA1 = "Target", IA2 = "Cohort", IA3 = "Rhyme", IA4 = "Unrelated")`).
#'
#' @return A ggplot2 plot of the proportion of looks over time for each IA, optionally faceted by condition.
#' @examples
#' # Example with a condition column
#' plot_IA_proportions(gaze_data, ia_column = "condition", time_column = "time_ms",
#'                     proportion_column = "proportion_looks", condition_column = "condition",
#'                     ia_mapping = list(IA1 = "Target", IA2 = "Cohort", IA3 = "Rhyme", IA4 = "Unrelated"))
#'
#' # Example without a condition column
#' plot_IA_proportions(gaze_data, ia_column = "condition", time_column = "time_ms",
#'                     proportion_column = "proportion_looks",
#'                     ia_mapping = list(IA1 = "Target", IA2 = "Cohort", IA3 = "Rhyme", IA4 = "Unrelated"))
#'
#' @export
#'

plot_IA_proportions <- function(data, ia_column, time_column, proportion_column, condition_column = NULL, ia_mapping) {

  if (!requireNamespace("ggokabeito", quietly = TRUE)) {
    install.packages("ggokabeito")
  }

  # Check that the ia_mapping provided has mappings for the IAs in the data
  unique_ias <- unique(data[[ia_column]])
  valid_ia_mapping <- ia_mapping[names(ia_mapping) %in% unique_ias]

  if (length(valid_ia_mapping) == 0) {
    stop("None of the specified IA mappings exist in the data.")
  }

  # Map the custom IA labels to the unique Interest Areas in the data
  data <- data %>%
    mutate(IA_label = recode(.data[[ia_column]], !!!valid_ia_mapping))  # Map the IAs using recode

  # Set IA_label as a factor with levels in the custom order provided by the user
  data$IA_label <- factor(data$IA_label, levels = unname(valid_ia_mapping))

  # Create the base plot
  p <- ggplot(data, aes_string(x = time_column, y = proportion_column, color = "IA_label")) +
    geom_line(size = 1.2) +          # Line plot with specified line width
    scale_y_continuous(limits = c(0, 1)) +  # Set y-axis from 0 to 1
    scale_color_okabe_ito(order = "original") +  # Apply colorblind-friendly palette
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
