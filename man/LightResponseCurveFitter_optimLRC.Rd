\name{LightResponseCurveFitter_optimLRC}
\alias{LightResponseCurveFitter_optimLRC}
\title{LightResponseCurveFitter optimLRC}
\description{call the optimization function}
\usage{LightResponseCurveFitter_optimLRC(theta, 
    iOpt, sdParameterPrior, ..., ctrl, isUsingHessian)}
\arguments{
  \item{theta}{numeric vector: starting parameters}
  \item{iOpt}{integer vector: positions of paramters to optimize}
  \item{sdParameterPrior}{numeric vector: prior uncertainty}
  \item{\dots}{further arguments to the cost function}
  \item{ctrl}{list of further controls}
  \item{isUsingHessian}{scalar boolean: set to TRUE to compute
Hessian at optimum}
}

\value{list of restult of \code{\link{optim}} amended with list
\item{theta}{numeric vector: optimized parameter
vector including the fixed components}
\item{iOpt}{integer vector: position of
parameters that have been optimized
}}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





