\name{RectangularLRCFitterCVersion_computeCost}
\alias{RectangularLRCFitterCVersion_computeCost}
\title{RectangularLRCFitterCVersion computeCost}
\description{Computing residual sum of sqares for predictions vs. data of NEE}
\usage{RectangularLRCFitterCVersion_computeCost(thetaOpt, 
    theta, iOpt, flux = NA, sdFlux = NA, parameterPrior, 
    sdParameterPrior, ..., VPD0 = 10, fixVPD = FALSE)}
\arguments{
  \item{thetaOpt}{parameter vecotr with components of theta0 that are optimized}
  \item{theta}{parameter vector with positions as in argument of \code{\link{LightResponseCurveFitter_getParameterNames}}}
  \item{iOpt}{position in theta that are optimized}
  \item{flux}{numeric: NEP (-NEE) or GPP time series [mumolCO2/m2/s], should not contain NA}
  \item{sdFlux}{numeric: standard deviation of Flux [mumolCO2/m2/s], should not contain NA}
  \item{parameterPrior}{numeric vector along theta: prior estimate of parameter (range of values)}
  \item{sdParameterPrior}{standard deviation of parameterPrior}
  \item{\dots}{other arguments to \code{\link{LightResponseCurveFitter_predictLRC}}}
  \item{VPD0}{VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010}
  \item{fixVPD}{fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





