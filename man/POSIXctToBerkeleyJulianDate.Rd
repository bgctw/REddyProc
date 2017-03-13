\name{POSIXctToBerkeleyJulianDate}
\alias{POSIXctToBerkeleyJulianDate}
\title{POSIXctToBerkeleyJulianDate}
\description{convert POSIXct to JulianDate format used in Berkeley release}
\usage{POSIXctToBerkeleyJulianDate(sDateTime)}
\arguments{
  \item{sDateTime}{POSIXct vector}
}
\details{In the Berkeley-Release of the fluxnet data, the time is stored as an number
with base10-digits representing YYYYMMddhhmm}


\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{BerkeleyJulianDateToPOSIXct}}}

