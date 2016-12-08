\name{partGLFitLRCOneWindow}
\alias{partGLFitLRCOneWindow}
\title{partGLFitLRCOneWindow}
\description{Estimate parameters of the Rectangular Hyperbolic Light Response Curve function (a,b,R_ref, k) for successive periods
Estimation as in Lasslop et al., 2010 for successive periods, i.e. windows.}
\usage{partGLFitLRCOneWindow(ds, winInfo, prevRes, E0Win, 
    controlGLPart = partGLControl())}
\arguments{
  \item{ds}{data.frame with numeric columns NEE, sdNEE, Temp (degC), VPD, Rg, and logical columns isNight and isDay}
  \item{winInfo}{one-row data.frame with window information, including iWindow}
  \item{prevRes}{component prevRes from previous result, here with item prevE0}
  \item{E0Win}{data.frame with columns E0, sdE0, RRef from nighttime, one row for each window}
  \item{controlGLPart}{list of further default parameters}
}
\details{Each window estimate is associated with a time or equivalently with a record.
The first record, i.e. row number, of the day-window is reported.
Moreover, the mean of all valid records numbers in the daytime window is reported for interpolation.}


\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partitionNEEGL}}

\code{\link{partGLEstimateTempSensInBoundsE0Only}}

\code{\link{partGLFitLRC}}}

