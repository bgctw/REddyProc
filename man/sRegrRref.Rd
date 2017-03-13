\name{sRegrRref}
\alias{sRegrRref}
\title{sEddyProc$sRegrRref - Estimation of the time series of reference respiration Rref }
\description{Estimation of the reference respiration Rref of \code{\link{fLloydTaylor}} for successive periods}
\usage{sRegrRref(NightFlux.s, TempVar.s, E_0.s, T_ref.n, WinDays.i = 3, 
    DayStep.i = 4, CallFunction.s = "")}
\arguments{
  \item{NightFlux.s}{Variable with (original) nighttime ecosystem carbon flux, i.e. respiration}
  \item{TempVar.s}{Variable with (original) air or soil temperature (degC)}
  \item{E_0.s}{Temperature sensitivity E_0 estimated with \code{\link{sRegrE0fromShortTerm}}}
  \item{T_ref.n}{
}
  \item{WinDays.i}{Window size for \code{\link{fLloydTaylor}} regression in days}
  \item{DayStep.i}{Window step for \code{\link{fLloydTaylor}} regression in days}
  \item{CallFunction.s}{Name of function called from}
}
\details{While parameter E0 in the Temperature-Respiration relationship (\code{\link{fLloydTaylor}} is kept konstant,
parameter Rref is allowed to change with time. 
This method estimates Rref for a series of time windows of length 2*\code{WinDays.i}+1 days 
shifted by \code{DayStep.i} days.

For some of the windows, it maybe not be possible to estimate Rref. These missing values are filled by linear
interpolation by function \code{\link{fInterpolateGaps}}.}
\value{Data vector (length number of windows) with reference respiration (R_ref, flux units)}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





