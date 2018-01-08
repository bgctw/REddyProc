\name{LightResponseCurveFitter_predictLRC}
\alias{LightResponseCurveFitter_predictLRC}
\title{LightResponseCurveFitter predictLRC}
\description{Light Response Function}
\usage{LightResponseCurveFitter_predictLRC(theta, 
    Rg, VPD, Temp, VPD0 = 10, fixVPD = (k == 
        0), TRef = 15)}
\arguments{
  \item{theta}{numeric vector of parameters}
  \item{Rg}{ppfd [numeric] -> photosynthetic flux density
[umol / m2 / s] or Global Radiation}
  \item{VPD}{VPD [numeric] -> Vapor Pressure Deficit [hPa]}
  \item{Temp}{Temp [degC] -> Temperature [degC]}
  \item{VPD0}{VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa
according to Lasslop et al 2010}
  \item{fixVPD}{boolean scalar or vector of nrow theta:
fixVPD if TRUE the VPD effect is not considered and VPD is not part
of the computation}
  \item{TRef}{numeric scalar of Temperature (degree Celsius) for
reference respiration RRef}
}
\details{Predict ecosystem fluxes (Reco, GPP, NEP = GPP-Reco) for given parameters
and environmental conditions.

The VPD effect is included according to Lasslop et al., 2010.

If theta is a matrix, a different row of parameters is used for different
entries of other inputs}


\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





