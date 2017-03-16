\name{partGLFitNightTimeTRespSens}
\alias{partGLFitNightTimeTRespSens}
\title{partGLFitNightTimeTRespSens}
\description{Estimate temperature sensitivity parameters for successive periods}
\usage{partGLFitNightTimeTRespSens(ds, winSizeRefInDays = 4L, 
    winSizeNight = 12L, winExtendSizes = winSizeNight * 
        c(2L, 4L), strideInDays = 2L, isVerbose = TRUE, 
    nRecInDay = 48L, controlGLPart = partGLControl())}
\arguments{
  \item{ds}{data.frame with numeric columns NEE, sdNEE, Temp (degC), VPD, Rg, and logical columns isNight and isDay}
  \item{winSizeRefInDays}{Window size in days for daytime for referencing windows to the same base}
  \item{winSizeNight}{Window size in days for nighttime fits}
  \item{winExtendSizes}{successively increased nighttime windows, to obtain a night-time fit}
  \item{strideInDays}{step in days for shifting the windows}
  \item{isVerbose}{set to FALSE to suppress messages}
  \item{nRecInDay}{number of records within one day (for half-hourly data its 48)}
  \item{controlGLPart}{list of further default parameters}
}

\value{data.frame with columns of \code{winInfo} from applyWindows, E0, sdE0, RRef}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partGLFitNightTempSensOneWindow}}

\code{\link{partGLSmoothTempSens}}

\code{\link{partGLFitNightRespRefOneWindow}}}

