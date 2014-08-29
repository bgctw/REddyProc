\name{sRegrE0fromShortTerm}
\alias{sRegrE0fromShortTerm}
\title{sEddyProc$sRegrE0fromShortTerm - Estimation of the temperature sensitivity E_0}
\description{Estimation of the temperature sensitivity E_0 from regression of \code{\link{fLloydTaylor}} for short periods}
\usage{sRegrE0fromShortTerm(NightFlux.s, TempVar.s, ..., CallFunction.s = "", 
    debug.l = list(fixedE0 = NA))}
\arguments{
  \item{NightFlux.s}{(original) nighttime ecosystem carbon flux, i.e. respiration variable}
  \item{TempVar.s}{(original) air or soil temperature (degC) variable}
  \item{\dots}{parameters passed to \code{\link{fRegrE0fromShortTerm}}}
  \item{CallFunction.s}{Name of function called from}
  \item{debug.l}{list with controls for debugging, see details}
}
\details{\describe{ \item{Debugging control}{
When supplying a finite scalar value \code{debug.l$fixedE0}, then this value 
is used instead of the temperature sensitivity E_0 from short term data.
}}}
\value{Data vector with (constant) temperature sensitivity (E_0, degK)}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





