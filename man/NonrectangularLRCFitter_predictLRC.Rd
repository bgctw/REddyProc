\name{NonrectangularLRCFitter_predictLRC}
\alias{NonrectangularLRCFitter_predictLRC}
\title{NonrectangularLRCFitter predictLRC}
\description{Nonrectangular Hyperbolic Light Response function: (Gilmanov et al., 2003)}
\usage{NonrectangularLRCFitter_predictLRC(theta, 
    Rg, VPD, Temp, VPD0 = 10, fixVPD = (k == 
        0), TRef = 15)}
\arguments{
  \item{theta}{theta [numeric] see \code{\link{NonrectangularLRCFitter_getParameterNames}}}
  \item{Rg}{ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation}
  \item{VPD}{VPD [numeric] -> Vapor Pressure Deficit [hPa]}
  \item{Temp}{Temp [degC] -> Temperature [degC]}
  \item{VPD0}{VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010}
  \item{fixVPD}{boolean scalar or vector of nrow theta:fixVPD if TRUE the VPD effect is not considered and VPD is not part of the computation}
  \item{TRef}{numeric scalar of Temperature (degree Celsius) for reference respiration RRef}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





