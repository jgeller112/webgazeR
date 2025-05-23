% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_IA_proportions.R
\name{plot_IA_proportions}
\alias{plot_IA_proportions}
\title{Plot Proportion of Looks Over Time for Interest Areas (IAs)}
\usage{
plot_IA_proportions(
  data,
  ia_column,
  time_column,
  proportion_column,
  condition_column = NULL,
  ia_mapping,
  use_color = TRUE
)
}
\arguments{
\item{data}{A data frame containing the data to plot.}

\item{ia_column}{The name of the column containing Interest Area (IA) identifiers.}

\item{time_column}{The name of the column representing time (e.g., milliseconds).}

\item{proportion_column}{The name of the column with the proportion of looks for each IA.}

\item{condition_column}{Optional. The name of the column representing experimental conditions.
If not provided, the plot will not be faceted by condition.}

\item{ia_mapping}{A named list specifying custom labels for each IA in the desired display order
(e.g., `list(IA1 = "Target", IA2 = "Cohort", IA3 = "Rhyme", IA4 = "Unrelated")`).}

\item{use_color}{Logical. If `TRUE` (default), the plot will use colors to differentiate Interest Areas.
If `FALSE`, different line types, shapes, and line widths will be used instead.}
}
\value{
A ggplot2 plot of the proportion of looks over time for each IA, optionally faceted by condition.
}
\description{
This function creates a time-course plot of the proportion of looks to specified Interest Areas (IAs).
Optionally, it can facet the plot by an experimental condition. Custom labels for each IA can be specified
through the `ia_mapping` argument to define the display order.
}
