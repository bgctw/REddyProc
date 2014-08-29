\name{fRegrE0fromShortTerm}
\alias{fRegrE0fromShortTerm}
\title{sEddyProc$sRegrE0fromShortTerm - Estimation of the temperature sensitivity E_0}
\description{Estimation of the temperature sensitivity E_0 from regression of \code{\link{fLloydTaylor}} for short periods}
\usage{fRegrE0fromShortTerm(NightFlux.V.n, TempVar.V.n, DayCounter.V.i, 
    WinDays.i = 7, DayStep.i = 5, TempRange.n = 5, NumE_0.n = 3, 
    Trim.n = 5, CallFunction.s = "", MinE_0.n = 30, MaxE_0.n = 450)}
\arguments{
  \item{NightFlux.V.n}{(original) nighttime ecosystem carbon flux, i.e. respiration vector}
  \item{TempVar.V.n}{(original) air or soil temperature (degC) vector}
  \item{DayCounter.V.i}{integer specifying the day of each record}
  \item{WinDays.i}{Window size for \code{\link{fLloydTaylor}} regression in days}
  \item{DayStep.i}{Window step for \code{\link{fLloydTaylor}} regression in days}
  \item{TempRange.n}{Threshold temperature range to start regression (#! Could be larger for Tair)}
  \item{NumE_0.n}{Number of best E_0's to average over}
  \item{Trim.n}{Percentile to trim residual (\%)}
  \item{CallFunction.s}{Name of function called from}
  \item{MinE_0.n}{minimum E0 for validity check}
  \item{MaxE_0.n}{maximum E0 for validity check}
}
\details{For aech window of length of days \code{WinDays.i}, being shifted by \code{DayStep.i} days to the previous one, 
coefficient E0 is estimated.
Only for those windows with a sufficient number or records and with a sufficient temperature range, \code{TempRange.n}
E0 is estimated by \code{\link{fOptimSingleE0}}.
Unreasonable estimates are discarded (95\% confidence interval inside \code{MinE_0.n} and \code{MaxE_0.n}.
Then the mean across the three best \code{NumE_0.n} estimates (ordered by uncertainty) is reported.

Take average of the three E_0 with lowest standard deviation}
\value{Estimated scalar temperature sensitivity (E_0, degK)}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





