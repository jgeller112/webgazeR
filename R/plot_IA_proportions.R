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
#' @param use_color Logical. If `TRUE` (default), the plot will use colors to differentiate Interest Areas.
#' If `FALSE`, different line types, shapes, and line widths will be used instead.
#'
#' @return A ggplot2 plot of the proportion of looks over time for each IA, optionally faceted by condition.
#' @examples
#' # Example with a condition column and color
#' plot_IA_proportions(gaze_data, ia_column = "condition", time_column = "time_ms",
#'                     proportion_column = "proportion_looks", condition_column = "condition",
#'                     ia_mapping = list(IA1 = "Target", IA2 = "Cohort", IA3 = "Rhyme", IA4 = "Unrelated"),
#'                     use_color = TRUE)
#'
#' # Example without color (using line types and shapes instead)
#' plot_IA_proportions(gaze_data, ia_column = "condition", time_column = "time_ms",
#'                     proportion_column = "proportion_looks", condition_column = "condition",
#'                     ia_mapping = list(IA1 = "Target", IA2 = "Cohort", IA3 = "Rhyme", IA4 = "Unrelated"),
#'                     use_color = FALSE)
#'
#' @export

plot_IA_proportions <- function(data, ia_column, time_column, proportion_column, condition_column = NULL, ia_mapping, use_color=TRUE) {

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

  # Create the base plot with different aesthetics based on use_color
  if (use_color) {
    p <- ggplot(data, aes(x = .data[[time_column]], y = .data[[proportion_column]], color = IA_label)) +
      geom_line(linewidth = 1.2) +  # Line plot with specified line width
      scale_color_okabe_ito() +  # Apply colorblind-friendly palette
      labs(
        x = "Time since word onset (ms)",
        y = "Proportion of Looks",
        color = "Interest Area"
      )
  } else {
    p <- ggplot(data, aes(x = .data[[time_column]], y = .data[[proportion_column]],
                          linetype = IA_label, shape = IA_label, linewidth = IA_label)) +
      geom_line() +
      geom_point(size = 3, alpha = 0.7) +

      # Set manual aesthetics, ensuring a single legend
      scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash"), guide = "legend") +
      scale_shape_manual(values = c(16, 17, 18, 15, 8), guide = "legend") +
      scale_linewidth_manual(values = c(0.8, 1.2, 1.5, 1.8, 2.2), guide = "legend") +

      labs(
        x = "Time since word onset (ms)",
        y = "Proportion of Looks",
        linetype = "Interest Area",
        shape = "Interest Area",
        linewidth = "Interest Area"  # Ensures all mapped aesthetics are in the same legend
      )
  }

  # Common plot styling
  p <- p +
    scale_y_continuous(limits = c(0, 1)) +  # Set y-axis range
    theme_minimal(base_size = 14) +
    theme(
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "bold"),
      legend.position = "right"
    )

  # If a condition column is provided, facet by condition
  if (!is.null(condition_column)) {
    p <- p + facet_wrap(vars(.data[[condition_column]]))
  }

  return(p)
}


