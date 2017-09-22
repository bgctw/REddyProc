\name{NonrectangularLRCFitter_computeGPPGradient}
\alias{NonrectangularLRCFitter_computeGPPGradient}
\title{NonrectangularLRCFitter computeGPPGradient}
\description{Logistic Sigmoid Light Response function for GPP}
\usage{NonrectangularLRCFitter_computeGPPGradient(Rg, 
    Amax, alpha, logitconv)}
\arguments{
  \item{Rg}{ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation}
  \item{Amax}{numeric scalar or vector of length(Rg): beta parameter adjusted for VPD effect}
  \item{alpha}{numeric scalar or vector of length(Rg): alpha parameter: initial slope}
  \item{logitconv}{numeric scalar or vector of length(Rg): logit of convexity paramter}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





