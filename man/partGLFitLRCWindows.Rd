\name{partGLFitLRCWindows}
\alias{partGLFitLRCWindows}
\title{partGLFitLRCWindows}
\description{Estimate temperature sensitivity and parameters of Rectangular Hyperbolic Light Response Curve function (a,b,R_ref, k) for successive periods}
\usage{partGLFitLRCWindows(ds, winSizeDays = 4L, strideInDays = 2L, 
    nRecInDay = 48L, dsTempSens, isVerbose = TRUE, 
    controlGLPart = partGLControl(), lrcFitter)}
\arguments{
  \item{ds}{data.frame with numeric columns NEE, sdNEE, Temp (degC), VPD, Rg, and logical columns isNight and isDay}
  \item{winSizeDays}{Window size in days for daytime fits}
  \item{strideInDays}{step in days for shifting the windows}
  \item{nRecInDay}{number of records within one day (for half-hourly data its 48)}
  \item{dsTempSens}{data.frame that reports for each window temperature sensitivity parameters E0 and RRef}
  \item{isVerbose}{set to FALSE to suppress messages}
  \item{controlGLPart}{list of further default parameters}
  \item{lrcFitter}{R5 class instance responsible for fitting the light response curve}
}

\value{a list with item \code{resOptList} with all optimization results 
and a item \code{summary} listing winInfo, LRC parameters, and their standard deviation  
and estimated from night-time data after smoothing and forward-filling: 
the uncertainty of temperature sensitivity \code{E0_night_sd} 
and the respiration at reference temperature \code{RRef_night}.}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partGLFitLRCOneWindow}}}

