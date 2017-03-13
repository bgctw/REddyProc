\name{BerkeleyJulianDateToPOSIXct}
\alias{BerkeleyJulianDateToPOSIXct}
\title{BerkeleyJulianDateToPOSIXct}
\description{convert JulianDate format used in Berkeley release to POSIXct}
\usage{BerkeleyJulianDateToPOSIXct(julianDate, tz = "GMT", 
    ...)}
\arguments{
  \item{julianDate}{numeric vector representing times (see details for format)}
  \item{tz}{time zone used to represent the dates}
  \item{\dots}{further arguments to \code{\link{strptime}}, such as tz}
}
\details{In the Berkeley-Release of the fluxnet data, the time is stored as an number
with base10-digits representing YYYYMMddhhmm}


\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{POSIXctToBerkeleyJulianDate}}}

