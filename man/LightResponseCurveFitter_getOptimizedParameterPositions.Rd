\name{LightResponseCurveFitter_getOptimizedParameterPositions}
\alias{LightResponseCurveFitter_getOptimizedParameterPositions}
\title{LightResponseCurveFitter getOptimizedParameterPositions}
\description{get the positions of the parameters to optimize for given Fixed}
\usage{LightResponseCurveFitter_getOptimizedParameterPositions(isUsingFixedVPD, 
    isUsingFixedAlpha)}
\arguments{
  \item{isUsingFixedVPD}{boolean scalar: if TRUE,
VPD effect set to zero and is not optimized}
  \item{isUsingFixedAlpha}{boolean scalar: if TRUE,
initial slope is fixed and is not optimized}
}
\details{If subclasses extend the parameter vector, they need
to overide this method.}
\value{integer vector of positions in parameter vector}

\author{Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





