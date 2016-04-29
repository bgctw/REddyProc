\name{fGetEndOfEddyPeriod}
\alias{fGetEndOfEddyPeriod}
\title{fGetEndOfEddyPeriod}
\description{Get the end time of a half-hour period, that is denoted by its beginning time}
\usage{fGetEndOfEddyPeriod(Time.V.p, DTS.n = 48L)}
\arguments{
  \item{Time.V.p}{Time stamp in POSIXct time format}
  \item{DTS.n}{Daily time steps}
}

\value{integer vector of length(Time.V.p): of times shifted by half an hour.}

\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





