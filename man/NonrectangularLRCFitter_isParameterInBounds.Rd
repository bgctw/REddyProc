\name{NonrectangularLRCFitter_isParameterInBounds}
\alias{NonrectangularLRCFitter_isParameterInBounds}
\title{NonrectangularLRCFitter isParameterInBounds}
\description{Check if estimated parameter vector is within reasonable bounds}
\usage{NonrectangularLRCFitter_isParameterInBounds(theta, 
    sdTheta, RRefNight, ctrl)}
\arguments{
  \item{theta}{numeric vector of estimated parameters}
  \item{sdTheta}{numeric vector of estimated standard deviation of the parameters}
  \item{RRefNight}{numeric scalar: night-time based estimate of basal respiration}
  \item{ctrl}{list of further controls}
}

\value{FALSE if parameters are outisde reasonable bounds, TRUE otherwise }

\author{TW, MM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





