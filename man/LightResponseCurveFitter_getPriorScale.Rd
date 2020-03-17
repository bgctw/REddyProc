\name{LightResponseCurveFitter_getPriorScale}
\alias{LightResponseCurveFitter_getPriorScale}
\title{LightResponseCurveFitter getPriorScale}
\description{return the prior distribution of parameters}
\usage{LightResponseCurveFitter_getPriorScale(thetaPrior, 
    medianRelFluxUncertainty, nRec, ctrl)}
\arguments{
  \item{thetaPrior}{numeric vector of location of priors}
  \item{medianRelFluxUncertainty}{numeric scalar: median across the
relative uncertainty of the flux values, i.e. sdNEE / NEE}
  \item{nRec}{integer scalar: number of finite observations}
  \item{ctrl}{list of further controls, with entry
\code{isLasslopPriorsApplied}}
}
\details{The beta parameter is quite well defined. Hence use a prior with
a standard deviation. The specific results are sometimes a bit sensitive
to the uncertainty of the beta prior. This uncertainty is set
corresponding to 20 times the median relative flux uncertainty. The prior
is weighted n times the observations in the cost. Hence, overall it is
using a weight of 1 / 20 of the weight of all observations.

However, its not well defined if PAR does not reach saturation. Need to
check before applying this prior}
\value{a numeric vector with prior estimates of the parameters}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





