\name{LightResponseCurveFitter_computeLRCGradient}
\alias{LightResponseCurveFitter_computeLRCGradient}
\title{LightResponseCurveFitter computeLRCGradient}
\description{Gradient of \code{\link{LightResponseCurveFitter_predictLRC}}}
\usage{LightResponseCurveFitter_computeLRCGradient(theta, 
    Rg, VPD, Temp, VPD0 = 10, fixVPD = (k == 
        0), TRef = 15)}
\arguments{
  \item{theta}{theta [numeric] -> parameter vector
(theta[1] = k (k), theta[2] = beta (beta), theta[3] = alpha,
theta[4] = RRef (rb), theta[4] = E0)}
  \item{Rg}{ppfd [numeric] -> photosynthetic flux density
[umol / m2 / s] or Global Radiation}
  \item{VPD}{VPD [numeric] -> Vapor Pressure Deficit [hPa]}
  \item{Temp}{Temp [degC] -> Temperature [degC]}
  \item{VPD0}{VPD0 [hPa] -> Parameters VPD0 fixed to 10
hPa according to Lasslop et al 2010}
  \item{fixVPD}{boolean scalar or vector of nrow(theta):
fixVPD if TRUE the VPD effect is not considered and VPD is not part
of the computation}
  \item{TRef}{numeric scalar of Temperature (degree Celsius) for
reference respiration RRef}
}



\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}





