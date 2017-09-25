\name{LogisticSigmoidLRCFitter_predictGPP}
\alias{LogisticSigmoidLRCFitter_predictGPP}
\title{LogisticSigmoidLRCFitter predictGPP}
\description{Logistic Sigmoid Light Response function for GPP}
\usage{LogisticSigmoidLRCFitter_predictGPP(Rg, Amax, 
    alpha)}
\arguments{
  \item{Rg}{ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation}
  \item{Amax}{numeric scalar or vector of length(Rg): beta parameter adjusted for VPD effect}
  \item{alpha}{numeric scalar or vector of length(Rg): alpha parameter: initial slope}
}
\details{\code{GPP <- Amax * tanh(alpha*Rg/Amax)}}
\value{numeric vector of length(Rg) of GPP }

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{LightResponseCurveFitter_predictGPP}}}

