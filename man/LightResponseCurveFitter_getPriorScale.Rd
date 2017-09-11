\name{LightResponseCurveFitter_getPriorScale}
\alias{LightResponseCurveFitter_getPriorScale}
\title{LightResponseCurveFitter getPriorScale}
\description{return the prior distribution of parameters}
\usage{LightResponseCurveFitter_getPriorScale(thetaPrior, 
    medianRelFluxUncertainty, nRec, ctrl)}
\arguments{
  \item{thetaPrior}{numeric vector of location of priors}
  \item{medianRelFluxUncertainty}{numeric scalar: median across the relative uncertainty of the flux values, i.e. sdNEE/NEE}
  \item{nRec}{integer scalar: number of finite observations}
  \item{ctrl}{list of further controls, with entry \code{isLasslopPriorsApplied}}
}
\details{The beta parameter is quite well defined. Hence use a prior with a standard deviation.
The specific results are sometimes a bit sensitive to the uncertainty of the beta prior. 
This uncertainty is set corresponding to 10 times the median relative flux uncertainty.
The prior is weighted n times the observations in the cost.
Hence, overall it is using a weight of 1/10 of the weight of all observations.}
\value{a numeric vector with prior estimates of the parameters}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




