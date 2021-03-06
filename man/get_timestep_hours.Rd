% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataFunctions.R
\name{get_timestep_hours}
\alias{get_timestep_hours}
\title{Get the timestep in fractional hours}
\usage{
get_timestep_hours(x)
}
\arguments{
\item{x}{Vector of POSIX timestamps of at least length 2.}
}
\value{
Numeric scalar of the time difference of the first two entries
  in fraction hours.
}
\description{
Get the timestep in fractional hours
}
