\name{partGLControl}
\alias{partGLControl}
\title{partGLControl}
\description{Default list of parameters for Lasslop 2010 daytime flux partitioning
For highest compatibility to Lasslop10 use 
\code{nBootUncertainty=0L, isAssociateParmsToMeanOfValids=FALSE
, isLasslopPriorsApplied=TRUE, isBoundLowerNEEUncertainty=FALSE
, smoothTempSensEstimateAcrossTime=FALSE
}}
\usage{partGLControl(LRCFitConvergenceTolerance = 0.001, nBootUncertainty = 30L, 
    minNRecInDayWindow = 10L, isAssociateParmsToMeanOfValids = TRUE, 
    isLasslopPriorsApplied = FALSE, isSdPredComputed = TRUE, 
    isFilterMeteoQualityFlag = FALSE, isBoundLowerNEEUncertainty = TRUE, 
    smoothTempSensEstimateAcrossTime = TRUE)}
\arguments{
  \item{LRCFitConvergenceTolerance}{convergence criterion for LRC fit.
If relative improvement of reducing residual sum of squares between predictions and 
observations is less than this criterion, assume convergence.
Decrease to get more precise parameter estimates, Increase for speedup.}
  \item{nBootUncertainty}{number of bootstrap samples for estimating uncertainty.
Set to zero to derive uncertainty from curvature of a single fit}
  \item{minNRecInDayWindow}{Minimum number of data points for regression}
  \item{isAssociateParmsToMeanOfValids}{set to FALSE to associate parameters to
the first record of the window for interpolation 
instead of mean across valid records inside a window}
  \item{isLasslopPriorsApplied}{set to TRUE to apply strong fixed priors on LRC fitting.
Returned parameter estimates claimed valid for some case where not enough data was available }
  \item{isSdPredComputed}{set to FALSE to avoid computing standard errors
of Reco and GPP for small performance increase 	}
  \item{isFilterMeteoQualityFlag}{set to TRUE to use only records where quality flag
of meteo drivers (Radation, Temperatrue, VPD) is zero, i.e. non-gapfilled for parameter estimation.
For prediction, the gap-filled value is used always, to produce predictions also for gaps.}
  \item{isBoundLowerNEEUncertainty}{set to FALSE to avoid adjustment of very low uncertainties before
day-Time fitting that avoids the high leverage those records with unreasonable low uncertainty.}
  \item{smoothTempSensEstimateAcrossTime}{set to FALSE to use independent estimates of temperature
sensitivity on each windows instead of a vector of E0 that is smoothed over time}
}

\value{list with entries of given arguments.}

\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partitionNEEGL}}}
\examples{
partGLControl(nBootUncertainty=40L)
}
