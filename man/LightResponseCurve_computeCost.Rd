\name{LightResponseCurve_computeCost}
\alias{LightResponseCurve_computeCost}
\title{LightResponseCurve computeCost}
\description{Computing residual sum of sqares for predictions vs. data of NEE}
\usage{LightResponseCurve_computeCost(thetaOpt, theta, iOpt, 
    flux = NA, sdFlux = NA, parameterPrior, sdParameterPrior, 
    ..., VPD0 = 10, fixVPD = FALSE)}
\arguments{
  \item{thetaOpt}{parameter vecotr with components of theta0 that are optimized}
  \item{theta}{parameter vector with positions as in argument of \code{\link{partGL_RHLightResponse}}}
  \item{iOpt}{position in theta that are optimized}
  \item{flux}{numeric: NEP (-NEE) or GPP time series [umolCO2/m2/s], should not contain NA}
  \item{sdFlux}{numeric: standard deviation of Flux [umolCO2/m2/s], should not contain NA}
  \item{parameterPrior}{numeric vector along theta: prior estimate of parameter (range of values)}
  \item{sdParameterPrior}{standard deviation of parameterPrior}
  \item{\dots}{other arguments to \code{\link{partGL_RHLightResponse}}}
  \item{VPD0}{VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010}
  \item{fixVPD}{fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





