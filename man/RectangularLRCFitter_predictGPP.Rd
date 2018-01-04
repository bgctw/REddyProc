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

\author{Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{LightResponseCurveFitter_predictGPP}}}

