\name{NonrectangularLRCFitter_predictGPP}
\alias{NonrectangularLRCFitter_predictGPP}
\title{NonrectangularLRCFitter predictGPP}
\description{Nonrectangluar hyperbolic Light Response function for GPP}
\usage{NonrectangularLRCFitter_predictGPP(Rg, Amax, 
    alpha, conv)}
\arguments{
  \item{Rg}{ppfd [numeric] -> photosynthetic flux density
[mumol / m2 / s] or Global Radiation}
  \item{Amax}{numeric scalar or vector of length(Rg):
beta parameter adjusted for VPD effect}
  \item{alpha}{numeric scalar or vector of length(Rg):
alpha parameter: initial slope}
  \item{conv}{numeric scalar or vector of length(Rg):
convexity parameter (see details)}
}
\details{This function generalizes the \code{\link{RectangularLRCFitter_predictGPP}}
by adding the convexity parameter \code{conv}.
For conv -> 0 (logitconv -> -Inf): approaches the rectangluar hyperbolic.
For conv -> 1 (logitconv -> + Inf): approaches a step function.
Expected values of conv are about 0.7-0.9 (Moffat 2012).}
\value{numeric vector of length(Rg) of GPP}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{LightResponseCurveFitter_predictGPP}}}

