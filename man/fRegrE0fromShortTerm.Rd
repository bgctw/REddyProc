\name{fRegrE0fromShortTerm}
\alias{fRegrE0fromShortTerm}
\title{Estimation of the temperature sensitivity E_0}
\description{Estimation of the temperature sensitivity E_0 from regression of \code{\link{fLloydTaylor}} for short periods}
\usage{fRegrE0fromShortTerm(NightFlux.V.n, TempVar.V.n, DayCounter.V.i, 
    WinDays.i = 7, DayStep.i = 5, TempRange.n = 5, 
    Trim.n = 5, NumE_0.n = 3, MinE_0.n = 30, MaxE_0.n = 450, 
    T_ref.n, CallFunction.s = "", optimAlgorithm = "default")}
\arguments{
  \item{NightFlux.V.n}{(Original) nighttime ecosystem carbon flux, i.e. respiration vector}
  \item{TempVar.V.n}{(Original) air or soil temperature vector (degC)}
  \item{DayCounter.V.i}{Integer specifying the day of each record}
  \item{WinDays.i}{Window size for \code{\link{fLloydTaylor}} regression in days}
  \item{DayStep.i}{Window step for \code{\link{fLloydTaylor}} regression in days}
  \item{TempRange.n}{Threshold temperature range to start regression (#! Could be larger for Tair)}
  \item{Trim.n}{Percentile to trim residual (\%)}
  \item{NumE_0.n}{Number of best E_0's to average over}
  \item{MinE_0.n}{Minimum E0 for validity check}
  \item{MaxE_0.n}{Maximum E0 for validity check}
  \item{T_ref.n}{
}
  \item{CallFunction.s}{Name of function called from}
  \item{optimAlgorithm}{optimization algorithm used (see \code{\link{nls}} ) or 'LM' for Levenberg-Marquard (see nlsLM from package minpack.lm)}
}
\details{The coefficient E0 is estimated for windows with a length of \code{WinDays.i} days, 
for successive periods in steps of \code{DayStep.i} days.
Only those windows with a sufficient number or records and with a sufficient temperature range \code{TempRange.n}
are used for the \code{\link{fLloydTaylor}} regression of E0 using the optimization \code{\link{fOptimSingleE0}}.
Unreasonable estimates are discarded (95\% confidence interval inside \code{MinE_0.n} and \code{MaxE_0.n}) and
the others are ordered by their standard deviations.
The mean across the best (=lowest standard deviation) E0 estimates is reported 
with \code{NumE_0.n} defining the number of best estimates to use.

Take average of the three E_0 with lowest standard deviation}
\value{Estimated scalar temperature sensitivity (E_0, degK)}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





