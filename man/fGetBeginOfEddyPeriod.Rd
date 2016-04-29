\name{fGetBeginOfEddyPeriod}
\alias{fGetBeginOfEddyPeriod}
\title{fGetBeginOfEddyPeriod}
\description{Get the begin time of a half-hour period, that is denoted by its end time.}
\usage{fGetBeginOfEddyPeriod(Time.V.p, DTS.n = 48L)}
\arguments{
  \item{Time.V.p}{Time stamp in POSIXct time format}
  \item{DTS.n}{Daily time steps}
}
\details{The timestamp of an Eddy record denotes the end of a half-hour period.
This function gets the time, half an hour before}
\value{integer vector of length(Time.V.p): of times shifted by half an hour.}

\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





