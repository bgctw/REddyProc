\name{partGLFitNightTempSensOneWindow}
\alias{partGLFitNightTempSensOneWindow}
\title{partGLFitNightTempSensOneWindow}
\description{Estimate temperature sensitivity E0 within one window
Estimation of respiration at reference temperature (RRef) and temperature (E0) for one window.}
\usage{partGLFitNightTempSensOneWindow(dss, winInfo, 
    prevRes, isVerbose = TRUE, nRecInDay = 48L, 
    controlGLPart = partGLControl())}
\arguments{
  \item{dss}{data.frame with numeric columns NEE, sdNEE, Temp (degC), VPD, Rg, and logical columns isNight and isDay}
  \item{winInfo}{one-row data.frame with window information, including iWindow}
  \item{prevRes}{component prevRes from previous result, here with item prevE0}
  \item{isVerbose}{set to FALSE to suppress messages}
  \item{nRecInDay}{number of records within one day (for half-hourly data its 48)}
  \item{controlGLPart}{list of further default parameters}
}
\details{The reference temperature is taken as the median of the temperatures of valid records,
unless a fixed reference temperature (in degree Celsius) is specified 
by argument \code{controlGLPart$fixedTRefAtNightTime}.}


\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partitionNEEGL}}

\code{\link{partGLEstimateTempSensInBoundsE0Only}}}

