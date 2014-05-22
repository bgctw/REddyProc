\name{fExpandToFullYear}
\alias{fExpandToFullYear}
\title{Generate vector with (half-)hourly time steps of full year, stamped in the middle of time unit}

\usage{fExpandToFullYear(Time.V.p, Data.V.n, Year.i, DTS.n, CallFunction.s = "")}
\arguments{
  \item{Time.V.p}{Time stamp in POSIX time format}
  \item{Data.V.n}{Data vector to be expanded}
  \item{Year.i}{Year (e.g. to plot)}
  \item{DTS.n}{Daily time steps}
  \item{CallFunction.s}{Name of function called from}
}
\details{Function to expand vectors to full year, e.g. to plot in correct time format}
\value{Expanded time and data vector as data frame}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





