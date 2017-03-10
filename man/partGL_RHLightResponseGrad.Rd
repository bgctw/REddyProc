\name{partGL_RHLightResponseGrad}
\alias{partGL_RHLightResponseGrad}
\title{partGL RHLightResponseGrad}
\description{Gradient of \code{\link{partGL_RHLightResponse}}}
\usage{partGL_RHLightResponseGrad(theta, Rg, VPD, Temp, VPD0 = 10, 
    fixVPD = FALSE)}
\arguments{
  \item{theta}{theta [numeric] -> parameter vector (theta[1]=kVPD (k), theta[2]=beta0 (beta), theta[3]=alfa, theta[4]=Rref (rb), theta[4]=E0)
<< E0: Temperature sensitivity ("activation energy") in Kelvin (degK)}
  \item{Rg}{ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation}
  \item{VPD}{VPD [numeric] -> Vapor Pressure Deficit [hPa]}
  \item{Temp}{Temp [degC] -> Temperature [degC]}
  \item{VPD0}{VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010}
  \item{fixVPD}{fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





