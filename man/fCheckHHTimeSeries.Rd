\name{fCheckHHTimeSeries}
\alias{fCheckHHTimeSeries}
\title{Check half-hourly time series data}

\usage{fCheckHHTimeSeries(Time.V.p, DTS.n, CallFunction.s = "")}
\arguments{
  \item{Time.V.p}{Time vector in POSIX format}
  \item{DTS.n}{Number of daily time steps (24 or 48)}
  \item{CallFunction.s}{Name of function called from}
}
\details{The number of steps per day can be 24 (hourly) or 48 (half-hourly).

The time stamp needs to be provided in POSIX time format,}
\value{Function stops on errors.}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





