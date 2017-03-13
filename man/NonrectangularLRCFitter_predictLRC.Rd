\name{NonrectangularLRCFitter_predictLRC}
\alias{NonrectangularLRCFitter_predictLRC}
\title{NonrectangularLRCFitter predictLRC}
\description{Nonrectangular Hyperbolic Light Response function: (Gilmanov et al., 2003)}
\usage{NonrectangularLRCFitter_predictLRC(theta, Rg, VPD, 
    Temp, VPD0 = 10, fixVPD = FALSE)}
\arguments{
  \item{theta}{theta [numeric] see \code{\link{NonrectangularLRCFitter_getParameterNames}}}
  \item{Rg}{ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation}
  \item{VPD}{VPD [numeric] -> Vapor Pressure Deficit [hPa]}
  \item{Temp}{Temp [degC] -> Temperature [degC]}
  \item{VPD0}{VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010}
  \item{fixVPD}{fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





