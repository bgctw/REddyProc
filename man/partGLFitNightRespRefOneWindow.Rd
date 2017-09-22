\name{partGLFitNightRespRefOneWindow}
\alias{partGLFitNightRespRefOneWindow}
\title{partGLFitNightRespRefOneWindow}
\description{Estimate Reference temperature from nicht-time and given temperature sensitivity E0  
Estimation of respiration at reference temperature (RRef) and temperature (E0) for one window.}
\usage{partGLFitNightRespRefOneWindow(dss, winInfo, 
    prevRes = list(), E0Win, controlGLPart = partGLControl(), 
    TRef = 15)}
\arguments{
  \item{dss}{data.frame with numeric columns NEE, isNight, Temp, Rg}
  \item{winInfo}{one-row data.frame with window information, including iWindow}
  \item{prevRes}{component prevRes from previous result, here not used.}
  \item{E0Win}{data.frame with columns E0 and sdE0, RRefFit, and TRefFit with one row for each window}
  \item{controlGLPart}{list of further default parameters}
  \item{TRef}{numeric scalar of Temperature (degree Celsius) for reference respiration RRef}
}
\details{If there are too few night-time records (< controlGLPart$minNRecInDayWindow) then return NA}
\value{named numeric vector with single entry RRef}

\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partitionNEEGL}}

\code{\link{partGLEstimateTempSensInBoundsE0Only}}}

