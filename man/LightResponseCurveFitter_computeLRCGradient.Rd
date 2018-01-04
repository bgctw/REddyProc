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
  \item{VPD0}{VPDQ0 [hPa] -> Parameters VPD0 fixed to 10
hPa according to Lasslop et al 2010}
  \item{fixVPD}{boolean scalar or vector of nrow(theta):
fixVPD if TRUE the VPD effect is not considered and VPD is not part
of the computation}
  \item{TRef}{numeric scalar of Temperature (degree Celsius) for
reference respiration RRef}
}



\author{Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}





