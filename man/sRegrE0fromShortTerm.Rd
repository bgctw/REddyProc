\name{sRegrE0fromShortTerm}
\alias{sRegrE0fromShortTerm}
\title{sEddyProc$sRegrE0fromShortTerm - Estimation of the temperature sensitivity E_0}
\description{Estimation of the temperature sensitivity E_0 from regression of \code{\link{fLloydTaylor}} 
for short periods by calling \code{\link{fRegrE0fromShortTerm}}}
\usage{sRegrE0fromShortTerm(NightFlux.s, TempVar.s, ..., CallFunction.s = "", 
    debug.l = list(fixedE0 = NA))}\arguments{
  \item{NightFlux.s}{Variable with (original) nighttime ecosystem carbon flux, i.e. respiration}
  \item{TempVar.s}{Variable with (original) air or soil temperature (degC)}
  \item{\dots}{Parameters passed to \code{\link{fRegrE0fromShortTerm}}}
  \item{CallFunction.s}{Name of function called from}
  \item{debug.l}{List with controls for debugging, see details}
}
\details{For further details see \code{\link{fRegrE0fromShortTerm}}.

\describe{ \item{Debugging control}{
When supplying a finite scalar value \code{debug.l$fixedE0}, then this value 
is used instead of the temperature sensitivity E_0 from short term data.
}}}
\value{Data vector with (constant) temperature sensitivity (E_0, degK) 
On failure, all entries are set to -111}

\author{AMM, TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





