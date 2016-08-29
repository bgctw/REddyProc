\name{partGLFitLRCWindows}
\alias{partGLFitLRCWindows}
\title{partGLFitLRCWindows}
\description{Estimate parameters of the Rectangular Hyperbolic Light Response Curve function (a,b,R_ref, k) for successive periods
Estimation as in Lasslop et al., 2010 for successive periods, i.e. windows.}
\usage{partGLFitLRCWindows(ds, WinSizeDays.i = 4L, WinSizeNight.i = 3L * 
    WinSizeDays.i, DayStep.i = floor(WinSizeDays.i/2L), 
    isVerbose = TRUE, nRecInDay.i = 48L, controlGLPart.l = partGLControl())}
\arguments{
  \item{ds}{data.frame with numeric columns NEE, sdNEE, Temp (degC), VPD, Rg, and logical columns isNight and isDay}
  \item{WinSizeDays.i}{Window size in days for daytime fits}
  \item{WinSizeNight.i}{Window size in days for nighttime fits}
  \item{DayStep.i}{step in days for shifting the windows}
  \item{isVerbose}{set to FALSE to suppress messages}
  \item{nRecInDay.i}{number of records within one day (for half-hourly data its 48)}
  \item{controlGLPart.l}{list of further default parameters}
}
\details{All the vectors must have the same length and consist of entire days, with each day having the same number of records

Each window estimate is associated with a time or equivalently with a record.
The first record, i.e. row number, of the day-window is reported.
Moreover, the mean of all valid records numbers in the daytime window is reported for interpolation.}
\value{a list with first entry a list with all optimization results, 
and a second entry a summary data.frame with a row for each window with columns
\item{Start}{the starting day of the day-window}
\item{End}{the endding day of the day-window}
\item{Num}{the number of records flux records in the window}
\item{iMeanRec}{the mean across all valid record numbers in the day-window}
\item{iCentralRec}{the record in the centre of the day-window (including NA-records)}
\item{iFirstRec}{the first record number in the day-window}
\item{E_0}{temperature sensitivty}
\item{E_0_SD}{standard deviation of temperature sensitivity}
\item{R_ref12}{respiration at reference temperature estimated from night-time data}
\item{R_ref}{respiration at reference temperature estimated from day-time data}
\item{a}{coefficients and associated standard deviation of the light respons curve: a,b,k}
\item{parms_out_range}{zero if it was a good parameter estimate, 1 if could not fit or fell outside ranges
}}

\author{MM, TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partitionNEEGL}}

\code{\link{partGLEstimateTempSensInBounds}}

\code{\link{partGLFitLRC}}}

