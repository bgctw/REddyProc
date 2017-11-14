\name{RectangularLRCFitter_predictGPP}
\alias{RectangularLRCFitter_predictGPP}
\title{RectangularLRCFitter predictGPP}
\description{Retangular hyperbolic Light Response function for GPP}
\usage{RectangularLRCFitter_predictGPP(Rg, Amax, 
    alpha)}
\arguments{
  \item{Rg}{ppfd [numeric] -> photosynthetic flux density
[mumol / m2 / s] or Global Radiation}
  \item{Amax}{vector of length(Rg): saturation (beta parameter) adjusted
for effect of VPD for each line of Rg}
  \item{alpha}{numeric scalar or vector of length(Rg): alpha parameter:
slope at Rg = 0}
}

\value{numeric vector of length(Rg) of GPP}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{LightResponseCurveFitter_predictGPP}}}

