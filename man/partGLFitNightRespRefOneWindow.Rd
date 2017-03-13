\name{partGLFitNightRespRefOneWindow}
\alias{partGLFitNightRespRefOneWindow}
\title{partGLFitNightRespRefOneWindow}
\description{Estimate Reference temperature from nicht-time and given temperature sensitivity E0  
Estimation of respiration at reference temperature (R_Ref) and temperature (E_0) for one window.}
\usage{partGLFitNightRespRefOneWindow(dss, winInfo, prevRes = list(), 
    E0Win, isVerbose = TRUE, nRecInDay = 48L, controlGLPart = partGLControl(), 
    TRef = 15)}
\arguments{
  \item{dss}{data.frame with numeric columns NEE, isNight, Temp, Rg}
  \item{winInfo}{one-row data.frame with window information, including iWindow}
  \item{prevRes}{component prevRes from previous result, here not used.}
  \item{E0Win}{data.frame with columns E0 and sdE0, RRefFit, and TRefFit with one row for each window}
  \item{isVerbose}{set to FALSE to suppress messages}
  \item{nRecInDay}{number of records within one day (for half-hourly data its 48)}
  \item{controlGLPart}{list of further default parameters}
  \item{TRef}{numeric scalar of Temperature (degree Celsius) for reference respiration RRef}
}

\value{list with entries}

\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partitionNEEGL}}

\code{\link{partGLEstimateTempSensInBoundsE0Only}}}

