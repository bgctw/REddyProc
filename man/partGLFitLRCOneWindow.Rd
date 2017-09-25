\name{partGLFitLRCOneWindow}
\alias{partGLFitLRCOneWindow}
\title{partGLFitLRCOneWindow}
\description{Estimate parameters of the Rectangular Hyperbolic Light Response Curve function (a,b,R_ref, k) for a single window
Estimation as in Lasslop et al., 2010 for successive periods, i.e. windows.}
\usage{partGLFitLRCOneWindow(ds, winInfo, prevRes, 
    E0Win, controlGLPart = partGLControl(), 
    lrcFitter = RectangularLRCFitter())}
\arguments{
  \item{ds}{data.frame with numeric columns NEE, sdNEE, Temp (degC), VPD, Rg, and logical columns isNight and isDay}
  \item{winInfo}{one-row data.frame with window information, including iWindow}
  \item{prevRes}{component prevRes from previous result, here with item prevE0}
  \item{E0Win}{data.frame with columns E0, sdE0, RRef from nighttime, one row for each window}
  \item{controlGLPart}{list of further default parameters}
  \item{lrcFitter}{R5 class instance responsible for fitting the light response curve}
}
\details{Fitting is done on a subset of the data where isDay, NEE, sdNEE, Temp, Rg, and VPD are all non-NA 
and isDay is TRUE 

With option \code{controlGLPart$neglectNEEUncertaintyOnMissing=TRUE} all sdNEE are set to 1 if there is any missing sdNEE at otherwise valid records

If  \code{controlGLPart$isNeglectVPDEffect=TRUE}, also records with VPD=NA maybe valid

If there are too few records (< \code{controlGLPart$minNRecInDayWindow}), then
the constraint on non-NA VPD is neglected and \code{controlGLPart$isNeglectVPDEffect} is set to TRUE
If there are still too few records, an NA-result with convergence code 1011L is returned.

Each window estimate is associated with a time or equivalently with a record.
The first record, i.e. row number, of the day-window is reported.
Moreover, the mean of all valid records numbers in the daytime window is reported for interpolation.}


\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partitionNEEGL}}

\code{\link{partGLEstimateTempSensInBoundsE0Only}}

\code{\link{LightResponseCurveFitter_fitLRC}}}

