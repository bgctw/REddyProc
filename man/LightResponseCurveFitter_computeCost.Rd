\name{LightResponseCurveFitter_computeCost}
\alias{LightResponseCurveFitter_computeCost}
\title{LightResponseCurveFitter computeCost}
\description{Computing residual sum of sqares for predictions vs. data of NEE}
\usage{LightResponseCurveFitter_computeCost(thetaOpt, theta, 
    iOpt, flux = NA, sdFlux = NA, parameterPrior, sdParameterPrior, 
    ...)}
\arguments{
  \item{thetaOpt}{parameter vecotr with components of theta0 that are optimized}
  \item{theta}{parameter vector with positions as in argument of \code{\link{LightResponseCurveFitter_getParameterNames}}}
  \item{iOpt}{position in theta that are optimized}
  \item{flux}{numeric: NEP (-NEE) or GPP time series [umolCO2/m2/s], should not contain NA}
  \item{sdFlux}{numeric: standard deviation of Flux [umolCO2/m2/s], should not contain NA}
  \item{parameterPrior}{numeric vector along theta: prior estimate of parameter (range of values)}
  \item{sdParameterPrior}{standard deviation of parameterPrior}
  \item{\dots}{other arguments to \code{\link{LightResponseCurveFitter_predictLRC}}, such as VPD0, fixVPD}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




