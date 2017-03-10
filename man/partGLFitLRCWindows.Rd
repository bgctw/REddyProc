\name{partGLFitLRCWindows}
\alias{partGLFitLRCWindows}
\title{partGLFitLRCWindows}
\description{Estimate temperature sensitivity and parameters of Rectangular Hyperbolic Light Response Curve function (a,b,R_ref, k) for successive periods}
\usage{partGLFitLRCWindows(ds, WinSizeDays.i = 4L, WinSizeNight.i = 3L * 
    WinSizeDays.i, DayStep.i = floor(WinSizeDays.i/2L), 
    isVerbose = TRUE, nRecInDay = 48L, winExtendSizes = WinSizeNight.i * 
        c(2L, 4L), controlGLPart = partGLControl(), 
    lrcFitter)}
\arguments{
  \item{ds}{data.frame with numeric columns NEE, sdNEE, Temp (degC), VPD, Rg, and logical columns isNight and isDay}
  \item{WinSizeDays.i}{Window size in days for daytime fits}
  \item{WinSizeNight.i}{Window size in days for nighttime fits}
  \item{DayStep.i}{step in days for shifting the windows}
  \item{isVerbose}{set to FALSE to suppress messages}
  \item{nRecInDay}{number of records within one day (for half-hourly data its 48)}
  \item{winExtendSizes}{successively increased nighttime windows, to obtain a night-time fit}
  \item{controlGLPart}{list of further default parameters}
  \item{lrcFitter}{R5 class instance responsible for fitting the light response curve}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partGLFitNightTempSensOneWindow}}

\code{\link{partGLSmoothTempSens}}

\code{\link{partGLFitNightRespRefOneWindow}}

\code{\link{partGLFitLRCOneWindow}}}

