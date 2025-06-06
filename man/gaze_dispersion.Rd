% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gaze_dispersion.R
\name{gaze_dispersion}
\alias{gaze_dispersion}
\title{Compute Gaze Dispersion}
\usage{
gaze_dispersion(data, x, y, grouping_vars)
}
\arguments{
\item{data}{A data frame or tibble.}

\item{x}{A string: name of the column with X gaze coordinates.}

\item{y}{A string: name of the column with Y gaze coordinates.}

\item{grouping_vars}{A character vector of column names to group by (e.g., subject, condition, trial).}
}
\value{
A tibble with centroid_x, centroid_y, dispersion, and log_dispersion for each group.
}
\description{
Calculates centroid anddispersion from gaze data.
}
