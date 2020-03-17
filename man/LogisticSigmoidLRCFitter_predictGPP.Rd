\name{LogisticSigmoidLRCFitter_predictGPP}
\alias{LogisticSigmoidLRCFitter_predictGPP}
\title{LogisticSigmoidLRCFitter predictGPP}
\description{Logistic Sigmoid Light Response function for GPP}
\usage{LogisticSigmoidLRCFitter_predictGPP(Rg, Amax, 
    alpha)}
\arguments{
  \item{Rg}{ppfd [numeric] -> photosynthetic flux density
[mumol / m2 / s] or Global Radiation}
  \item{Amax}{vector of length(Rg): saturation (beta parameter) adjusted
for effect of VPD for each line of Rg}
  \item{alpha}{numeric scalar or vector of length(Rg): alpha parameter:
slope at Rg = 0}
}
\details{\code{GPP <- Amax * tanh(alpha * Rg / Amax)}}
\value{numeric vector of length(Rg) of GPP}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{LightResponseCurveFitter_predictGPP}}}

