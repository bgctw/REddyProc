\name{partGLControl}
\alias{partGLControl}
\title{partGLControl}
\description{Default list of parameters for Lasslop 2010 daytime flux partitioning
For highest compatibility to Lasslop10 use 
\code{nBootUncertainty=0L, isAssociateParmsToMeanOfValids=FALSE, isLasslopPriorsApplied=TRUE}}
\usage{partGLControl(LRCFitConvergenceTolerance = 0.001, nBootUncertainty = 30L, 
    minNRecInDayWindow = 12L, isAssociateParmsToMeanOfValids = TRUE, 
    isLasslopPriorsApplied = FALSE, isSdPredComputed = TRUE)}
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
}

\value{list with entries of given arguments.}

\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partitionNEEGL}}}
\examples{
partGLControl(nBootUncertainty=40L)
}
