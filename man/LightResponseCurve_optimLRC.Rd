\name{LightResponseCurve_optimLRC}
\alias{LightResponseCurve_optimLRC}
\title{LightResponseCurve optimLRC}
\description{call the optimization function}
\usage{LightResponseCurve_optimLRC(theta, iOpt, sdParameterPrior, 
    isUsingHessian = isUsingHessian, ...)}
\arguments{
  \item{theta}{numeric vector: starting parameters

numeric vector: optimized parameter vector including the fixed components}
  \item{iOpt}{integer vector: positions of paramters to optimize

integer vector: position of parameters that have been optimized}
  \item{sdParameterPrior}{numeric vector: prior uncertainty}
  \item{isUsingHessian}{boolean scalar, whether to compute Hessian at the optimum}
  \item{\dots}{further arguments to \code{\link{computeCost}}}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





