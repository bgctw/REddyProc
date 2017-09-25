\name{replaceMissingSdByPercentage}
\alias{replaceMissingSdByPercentage}
\title{replaceMissingSdByPercentage}
\description{replace missing standard deviation of a measure x by a percentage of x}
\usage{replaceMissingSdByPercentage(sdX, x, perc = 0.2, 
    minSdX = 0.7)}
\arguments{
  \item{sdX}{numeric vector: with missing to be repalce}
  \item{x}{numeric vector of length(sdX): value form which percentage is computed}
  \item{perc}{numeric scalar: sdX = perc*x}
  \item{minSdX}{numeric scalar: minimum of sdX to be applied for low x}
}
\details{If either perc or inSdX is NA then only the other criterion is applied.
If both are NA then all missings are set to NA.
\code{sdX[iToFill] <- pmax(minSdX, abs(x[iToFill]*perc), na.rm=TRUE)}}
\value{sdX with non-finite values replaced.}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





