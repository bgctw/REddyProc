\name{fGetYearOfEddyPeriod}
\alias{fGetYearOfEddyPeriod}
\title{fGetYearOfEddyPeriod}
\description{get the Year from a POSIXct, with new Year (1.1. 00:00) belongs to the old year.}
\usage{fGetYearOfEddyPeriod(Time.V.p, DTS.n = 48L)}\arguments{
  \item{Time.V.p}{Time stamp in POSIXct time format}
  \item{DTS.n}{Daily time steps}
}
\details{The timestamp of an Eddy record denotes the end of a half-hour period.
If the end is NewYear,e.g. 1.1.2015 00:00) the half hour period is still in the old year.}
\value{integer vector of length(Time.V.p): of calendar years}

\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





