\name{LightResponseCurve_optimLRCBounds}
\alias{LightResponseCurve_optimLRCBounds}
\title{LightResponseCurve optimLRCBounds}
\description{Optimize parameters of light response curve and refit with some fixed parameters if fitted parameters are outside bounds}
\usage{LightResponseCurve_optimLRCBounds(theta0, parameterPrior, 
    ..., lastGoodParameters.V.n, ctrl)}
\arguments{
  \item{theta0}{initial parameter estimate}
  \item{parameterPrior}{prior estimate of model parameters}
  \item{\dots}{further parameters to \code{.optimLRC}, such as \code{dsDay}}
  \item{lastGoodParameters.V.n}{parameters vector of last successful fit}
  \item{ctrl}{list of further controls}
}

\value{list result of optimization as of \code{.optimLRC} with entries 
\item{theta}{ numeric parameter vector that includes the fixed components}
\item{iOpt}{ integer vector of indices of the vector that have been optimized}}

\author{TW, MM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





