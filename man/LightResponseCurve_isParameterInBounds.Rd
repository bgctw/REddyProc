\name{LightResponseCurve_isParameterInBounds}
\alias{LightResponseCurve_isParameterInBounds}
\title{LightResponseCurve isParameterInBounds}
\description{Check if estimated parameter vector is within reasonable bounds}
\usage{LightResponseCurve_isParameterInBounds(theta, sdTheta, 
    RRefNight, ctrl)}
\arguments{
  \item{theta}{estimate of parameter}
  \item{sdTheta}{estimate of uncertainty of the parameter}
  \item{RRefNight}{numeric scalar: night-time based estimate of basal respiration}
  \item{ctrl}{list of further controls}
}

\value{TRUE if parameters are within reasonable bounds, FALSE otherwise }

\author{TW, MM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





