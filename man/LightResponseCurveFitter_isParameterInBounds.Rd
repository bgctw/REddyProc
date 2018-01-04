\name{LightResponseCurveFitter_isParameterInBounds}
\alias{LightResponseCurveFitter_isParameterInBounds}
\title{LightResponseCurveFitter isParameterInBounds}
\description{Check if estimated parameter vector is within reasonable bounds}
\usage{LightResponseCurveFitter_isParameterInBounds(theta, 
    sdTheta, RRefNight, ctrl)}
\arguments{
  \item{theta}{estimate of parameter}
  \item{sdTheta}{estimate of uncertainty of the parameter}
  \item{RRefNight}{numeric scalar: night-time based estimate of
basal respiration}
  \item{ctrl}{list of further controls}
}
\details{check the Beta bounds that depend on uncertainty:
outside if (beta > 100 and sdBeta >= beta)}
\value{FALSE if parameters are outisde reasonable bounds, TRUE otherwise}

\author{TW, MM
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}





