\name{Example_DETha98}
\docType{data}
\alias{Example_DETha98}
\title{Eddy covariance data from Tharandt, Germany}
\description{The data frame 'EddyData.F' contains half-hourly eddy covariance measurements from Tharandt of the year 1998.}
\usage{data(Example_DETha98)}

\format{For each column, the attributes 'varnames' for the variable names and 'units' for the variable units are provided. \cr
  \describe{
  \item{Time stamp}{Year - Year provided with century 1998. \cr DoY - Day of year provided as 1 to 365 (or 1 to 366 in leap years). \cr Hour - Hour provided as decimal 0.0 to 23.5.}
  \item{Flux measurements}{NEE, LE, H}
  \item{Meteo measurements}{Rg, Tair, Tsoil, rH, VPD, Ustar}
  }
For processing of the example data see \href{../doc/useCase.html}{useCase vignette}.
}

\source{The data originates from the CARBODATA CD.}
%\references{}
\keyword{dataset}
