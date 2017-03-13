\name{LogisticSigmoidLRCFitter_computeGPPGradient}
\alias{LogisticSigmoidLRCFitter_computeGPPGradient}
\title{LogisticSigmoidLRCFitter computeGPPGradient}
\description{Logistic Sigmoid Light Response function for GPP}
\usage{LogisticSigmoidLRCFitter_computeGPPGradient(Rg, Amax, 
    alpha)}
\arguments{
  \item{Rg}{ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation}
  \item{Amax}{numeric scalar or vector of length(Rg): beta parameter adjusted for VPD effect}
  \item{alpha}{numeric scalar or vector of length(Rg): alpha parameter: initial slope}
}

\value{numeric matrix (length(Rg),2) of gradients of predicted GPP to Amax and alpha }

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





