\name{LightResponseCurve_getPriorScale}
\alias{LightResponseCurve_getPriorScale}
\title{LightResponseCurve getPriorScale}
\description{return the prior distribution of parameters}
\usage{LightResponseCurve_getPriorScale(thetaPrior, medianRelFluxUncertainty, 
    nRec, ctrl)}
\arguments{
  \item{thetaPrior}{numeric vector of location of priors}
  \item{medianRelFluxUncertainty}{numeric scalar: median across the relative uncertainty of the flux values, i.e. sdNEE/NEE}
  \item{nRec}{integer scalar: number of finite observations}
  \item{ctrl}{list of further controls, with entry \code{isLasslopPriorsApplied}}
}

\value{a numeric vector with prior estimates of the parameters}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





