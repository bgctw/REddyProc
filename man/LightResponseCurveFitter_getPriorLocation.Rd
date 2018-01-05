\name{LightResponseCurveFitter_getPriorLocation}
\alias{LightResponseCurveFitter_getPriorLocation}
\title{LightResponseCurveFitter getPriorLocation}
\description{return the prior distribution of parameters}
\usage{LightResponseCurveFitter_getPriorLocation(NEEDay, 
    RRefNight, E0)}
\arguments{
  \item{NEEDay}{numeric vector of daytime NEE}
  \item{RRefNight}{numeric scalar of basal respiration estimated
from night-time data}
  \item{E0}{numeric scalar of night-time estimate of temperature sensitivity}
}

\value{a numeric vector with prior estimates of the parameters}

\author{Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





