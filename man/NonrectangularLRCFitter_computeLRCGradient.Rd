\name{NonrectangularLRCFitter_computeLRCGradient}
\alias{NonrectangularLRCFitter_computeLRCGradient}
\title{NonrectangularLRCFitter computeLRCGradient}
\description{Gradient of \code{\link{NonrectangularLRCFitter_predictLRC}}}
\usage{NonrectangularLRCFitter_computeLRCGradient(theta, Rg, 
    VPD, Temp, VPD0 = 10, fixVPD = (k == 0), TRef = 15)}
\arguments{
  \item{theta}{theta [numeric] -> parameter vector (theta[1]=kVPD (k), theta[2]=beta0 (beta), theta[3]=alpha, theta[4]=RRef (rb), theta[4]=E0, theta[5]=logitconv)}
  \item{Rg}{ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation}
  \item{VPD}{VPD [numeric] -> Vapor Pressure Deficit [hPa]}
  \item{Temp}{Temp [degC] -> Temperature [degC]}
  \item{VPD0}{VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010}
  \item{fixVPD}{boolean scalar or vector of nrow(theta): fixVPD if TRUE the VPD effect is not considered and VPD is not part of the computation}
  \item{TRef}{numeric scalar of Temperature (degree Celsius) for reference respiration RRef}
}
\details{differs from base by extracting conv parameter from theta 
and adding gradient to logitconv (3rd parameter from computeGPPGradient)}


\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





