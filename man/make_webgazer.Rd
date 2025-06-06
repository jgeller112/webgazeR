% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_webgazer.R
\name{make_webgazer}
\alias{make_webgazer}
\title{Standardize a Dataframe to WebGazer Conventions}
\usage{
make_webgazer(
  data,
  col_map = list(subject = "subject", trial = "trial", time = "time", x = "x", y = "y")
)
}
\arguments{
\item{data}{A dataframe containing gaze data.}

\item{col_map}{A named list mapping your current columns to WebGazer names: `subject`, `trial`, `time`, `x`, `y`.}
}
\value{
A dataframe with renamed columns but preserves all other original columns.
}
\description{
This function takes a dataframe and renames columns to match WebGazer conventions:
subject, trial, time, x, y. All other columns are preserved.
Standardize a Dataframe to WebGazer Conventions
}
\details{
This function takes a dataframe and renames columns to match WebGazer conventions:
subject, trial, time, x, y. All other columns are preserved.
}
