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
For processing of the example data see \code{vignette("DEGebExample")}.
}

\details{
DISCLAIMER: This example dataset should only be used for test purposes of the REddyProc R package. 
For other uses, the data is openly available through the European Fluxes Database 
(http://www.europe-fluxdata.eu/home/site-details?id=3) 
and upon registration the current version can be downloaded there.
}


\source{The data was downloaed from \code{http://www.europe-fluxdata.eu} at date 2016-01-25.}
%\references{}
\keyword{dataset}
