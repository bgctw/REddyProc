\name{DEGebExample}
\docType{data}
\alias{DEGebExample}
\title{Eddy covariance data from Gebesee crop site, Germany}
\description{The data frame 'DEGebExample' contains half-hourly eddy covariance measurements from Gebesee of the years 2004 to 2006.}
\usage{data(DEGebExample)}

\format{For each column, the attributes 'varnames' for the variable names and 'units' for the variable units are provided. \cr
  \describe{
  \item{Time stamp}{ DateTime: POSIXct-time of the end of the half-hour period, Use as.POSIXlt(DateTime)$year to get hour, day of year, ...}
  \item{Flux measurements}{NEE}
  \item{Meteo measurements}{Rg, Tair, rH, VPD, Ustar}
  }
For processing of the example data see \code{\link{sEddyProc.example}}.
}

\source{The data originates from the  http://www.europe-fluxdata.eu.}
%\references{}
\keyword{dataset}
