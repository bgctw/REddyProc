\name{fFullYearTimeSteps}
\alias{fFullYearTimeSteps}
\title{fFullYearTimeSteps}
\description{Generate vector with (half-)hourly time steps of full year, stamped in the middle of time unit}
\usage{fFullYearTimeSteps(Year.i, DTS.n, CallFunction.s = "", 
    tz = "GMT")}
\arguments{
  \item{Year.i}{Data frame to be converted}
  \item{DTS.n}{Daily time steps}
  \item{CallFunction.s}{Name of function called from}
  \item{tz}{timezone used to store the data. Advisded to keep GMT to avoid daytime shifting issues}
}

\value{Vector with time steps of full year in POSIX format}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





