\name{NonrectangularLRCFitter_optimLRC}
\alias{NonrectangularLRCFitter_optimLRC}
\title{NonrectangularLRCFitter optimLRC}
\description{<< calling the optimization function}
\usage{NonrectangularLRCFitter_optimLRC(theta, iOpt, sdParameterPrior, 
    ..., ctrl, isUsingHessian)}
\arguments{
  \item{theta}{numeric vector of starting values}
  \item{iOpt}{integer vector of positions of parameters being optimized}
  \item{sdParameterPrior}{numeric vector of scale of parameter priors}
  \item{\dots}{further arguments to the cost function}
  \item{ctrl}{list of further controls}
  \item{isUsingHessian}{scalar boolean: set to TRUE to compute Hessian at optimum}
}

\value{list of restult of \code{\link{optim}} amended with list
\item{theta}{numeric vector: optimized parameter vector including the fixed components}
\item{iOpt}{integer vector: position of parameters that have been optimized
}}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





