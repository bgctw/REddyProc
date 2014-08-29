\name{sRegrRref}
\alias{sRegrRref}
\title{sEddyProc$sRegrRref - Estimation of the reference respiration Rref}
\description{Estimation of the reference respiration Rref of \code{\link{fLloydTaylor}} for successive periods}
\usage{sRegrRref(NightFlux.s, TempVar.s, E_0.s, WinDays.i = 3, DayStep.i = 4, 
    CallFunction.s = "")}
\arguments{
  \item{NightFlux.s}{Variable with (original) nighttime ecosystem carbon flux, i.e. respiration}
  \item{TempVar.s}{Variable with (original) air or soil temperature (degC)}
  \item{E_0.s}{Temperature sensitivity E_0 estimated with \code{\link{sRegrE0fromShortTerm}}}
  \item{WinDays.i}{Window size for \code{\link{fLloydTaylor}} regression in days}
  \item{DayStep.i}{Window step for \code{\link{fLloydTaylor}} regression in days}
  \item{CallFunction.s}{Name of function called from}
}
\details{Estimates a }
\value{Data vector with reference respiration (R_ref, flux units)}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





