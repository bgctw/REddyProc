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

\author{Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{LightResponseCurveFitter_predictGPP}}}

