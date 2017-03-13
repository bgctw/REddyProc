\name{LightResponseCurveFitter_computeLRCGradient}
\alias{LightResponseCurveFitter_computeLRCGradient}
\title{LightResponseCurveFitter computeLRCGradient}
\description{Gradient of \code{\link{LightResponseCurveFitter_predictLRC}}}
\usage{LightResponseCurveFitter_computeLRCGradient(theta, 
    Rg, VPD, Temp, VPD0 = 10, fixVPD = FALSE, TRef = 15)}
\arguments{
  \item{theta}{theta [numeric] -> parameter vector (theta[1]=k (k), theta[2]=beta (beta), theta[3]=alpha, theta[4]=RRef (rb), theta[4]=E0)}
  \item{Rg}{ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation}
  \item{VPD}{VPD [numeric] -> Vapor Pressure Deficit [hPa]}
  \item{Temp}{Temp [degC] -> Temperature [degC]}
  \item{VPD0}{VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010}
  \item{fixVPD}{fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered}
  \item{TRef}{numeric scalar of Temperature (degree Celsius) for reference respiration RRef}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





