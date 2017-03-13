\name{partGLFitNightTempSensOneWindow}
\alias{partGLFitNightTempSensOneWindow}
\title{partGLFitNightTempSensOneWindow}
\description{Estimate parameters of the Rectangular Hyperbolic Light Response Curve function (a,b,R_ref, k) for successive periods
Estimation of respiration at reference temperature (R_Ref) and temperature (E_0) for one window.}
\usage{partGLFitNightTempSensOneWindow(dss, winInfo, prevRes, 
    isVerbose = TRUE, nRecInDay = 48L, controlGLPart = partGLControl())}
\arguments{
  \item{dss}{data.frame with numeric columns NEE, sdNEE, Temp (degC), VPD, Rg, and logical columns isNight and isDay}
  \item{winInfo}{one-row data.frame with window information, including iWindow}
  \item{prevRes}{component prevRes from previous result, here with item prevE0}
  \item{isVerbose}{set to FALSE to suppress messages}
  \item{nRecInDay}{number of records within one day (for half-hourly data its 48)}
  \item{controlGLPart}{list of further default parameters}
}



\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partitionNEEGL}}

\code{\link{partGLEstimateTempSensInBoundsE0Only}}}

