\name{sRegrE0fromShortTerm}
\alias{sRegrE0fromShortTerm}
\title{sEddyProc$sRegrE0fromShortTerm - Estimation of the temperature sensitivity E_0}
\description{Estimation of the temperature sensitivity E_0 from regression of \code{\link{fLloydTaylor()}} for short periods}
\usage{sRegrE0fromShortTerm(NightFlux.s, TempVar.s, WinDays.i = 7, DayStep.i = 5, 
    TempRange.n = 5, NumE_0.n = 3, Trim.n = 5, CallFunction.s = "", 
    debug.l = list(fixedE0 = NA))}
\arguments{
  \item{NightFlux.s}{Variable with (original) nighttime ecosystem carbon flux, i.e. respiration}
  \item{TempVar.s}{Variable with (original) air or soil temperature (degC)}
  \item{WinDays.i}{Window size for \code{\link{fLloydTaylor()}} regression in days}
  \item{DayStep.i}{Window step for \code{\link{fLloydTaylor()}} regression in days}
  \item{TempRange.n}{Threshold temperature range to start regression (#! Could be larger for Tair)}
  \item{NumE_0.n}{Number of best E_0's to average over}
  \item{Trim.n}{Percentile to trim residual (\%)}
  \item{CallFunction.s}{Name of function called from}
  \item{debug.l}{list with controls for debugging, see details}
}
\details{Take average of the three E_0 with lowest standard deviation

\describe{ \item{Debugging control}{
When supplying a finite scalar value \code{debug.l$fixedE0}, then this value is used instead of the temperature sensitivity E_0 from short term data.
In order to test, whether differences between approaches are due to determination of E0.
}}}
\value{Data vector with (constant) temperature sensitivity (E_0, degK)}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





