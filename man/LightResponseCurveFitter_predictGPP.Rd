\name{LightResponseCurveFitter_predictGPP}
\alias{LightResponseCurveFitter_predictGPP}
\title{LightResponseCurveFitter predictGPP}
\description{Light Response function for GPP}
\usage{LightResponseCurveFitter_predictGPP(Rg, ...)}
\arguments{
  \item{Rg}{ppfd [numeric] -> photosynthetic flux density [mumol / m2 / s]
or Global Radiation}
  \item{\dots}{further parameters to the LRC}
}
\details{This method must be be implemented by a specific subclass.
Currently there are several alternatives: \cr
\itemize{
\item Rectangular: \code{\link{RectangularLRCFitter_predictGPP}}
\item Nonrectangular: \code{\link{NonrectangularLRCFitter_predictGPP}}
\item Rectangular: \code{\link{LogisticSigmoidLRCFitter_predictGPP}}
}}
\value{numeric vector of length(Rg) of GPP}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{partitionNEEGL}}}

