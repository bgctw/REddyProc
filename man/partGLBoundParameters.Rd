\name{partGLBoundParameters}
\alias{partGLBoundParameters}
\title{partGLBoundParameters}
\description{Check if parameters are in the range and correct to bounds}
\usage{partGLBoundParameters(resOpt, last_good)}
\arguments{
  \item{resOpt}{list with first entry vector of parameters, second entry its uncertainty}
  \item{last_good}{vector of windows most recent good fit, make sure to set it to reasonable values, e.g. initial guess before optimiztion}
}
\details{adjustments according to TAble A1 in Lasslop 2010
vector positions: 1: k, 2: beta, 3: alpha, 4: rb}
\value{list with entries
\item{opt.parms.V}{numeric vector of bounded optimized parameters}
\item{se.parms.V}{numeric vector of uncertainties of parameters}
\item{isGoodParameterSet}{boolean scalar that is FALSE if original parameters were outside bounds
}}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





