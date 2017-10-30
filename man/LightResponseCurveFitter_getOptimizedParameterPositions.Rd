\name{LightResponseCurveFitter_getOptimizedParameterPositions}
\alias{LightResponseCurveFitter_getOptimizedParameterPositions}
\title{LightResponseCurveFitter getOptimizedParameterPositions}
\description{get the positions of the parameters to optimize for given conditions on fixing alpha or VPD effect}
\usage{LightResponseCurveFitter_getOptimizedParameterPositions(isUsingFixedVPD, 
    isUsingFixedAlpha)}
\arguments{
  \item{isUsingFixedVPD}{boolean scalar: if TRUE, VPD effect set to zero and is not optimized}
  \item{isUsingFixedAlpha}{boolean scalar: if TRUE, initial slope is fixed and is not optimized}
}
\details{If subclasses extend the parameter vector, they need to overide this method.}
\value{integer vector of positions in parameter vector}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





