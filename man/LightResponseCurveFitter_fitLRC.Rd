\name{LightResponseCurveFitter_fitLRC}
\alias{LightResponseCurveFitter_fitLRC}
\title{LightResponseCurveFitter fitLRC}
\description{Optimize rectangular hyperbolic light response curve against data in one window and estimate uncertainty}
\usage{LightResponseCurveFitter_fitLRC(dsDay, E0, sdE0, RRefNight, 
    controlGLPart = partGLControl(), lastGoodParameters = rep(NA_real_, 
        7L))}
\arguments{
  \item{dsDay}{data.frame with columns NEE, Rg, Temp_C, VPD, and no NAs in NEE}
  \item{E0}{temperature sensitivity of respiration}
  \item{sdE0}{standard deviation of E_0.n}
  \item{RRefNight}{basal respiration estimated from night time data}
  \item{controlGLPart}{further default parameters (see \code{\link{partGLControl}})}
  \item{lastGoodParameters}{numeric vector returned by last reasonable fit}
}
\details{Optimization is performed for three initial parameter sets that differ by beta0 (*1.3, *0.8).
From those three, the optimization result is selected that yielded the lowest misfit.
Starting values are: k=0, beta=interpercentileRange(0.03,0.97) of respiration, alpha=0.1, R_ref 
from nightTime estimate.
E0 is fixed to the night-time estimate, but varied for estimating parameter uncertainty.

If \code{controlGLPart.l$nBootUncertainty == 0L} then the covariance matrix of the 
parameters is estimated by the Hessian of the LRC curve at optimum.
Then, the additional uncertainty and covariance with uncertaint E0 is neglected.

If \code{controlGLPart.l$nBootUncertainty > 0L} then the covariance matrix of the 
parameters is estimated by a bootstrap of the data.
In each draw, E0 is drawn from N ~ (E_0, sdE_0).}
\value{a list, If none of the optimizations from different starting conditions converged,
the parameters are NA
\item{thetaOpt}{numeric vector of optimized parameters including
the fixed ones and including E0}
\item{iOpt}{index of parameters that have been optimized
, here including E0, which has been optimized prior to this function.}
\item{thetaInitialGuess}{the initial guess from data}
\item{covParms}{numeric matrix of the covariance matrix of parameters, including E0}
\item{convergence}{integer code specifying convergence problems
<< 0: good convergence
<< ,1-1000: see \code{\link{optim}}
<< ,1001: too few bootstraps converged
<< ,1002: fitted parameters were outside reasonable bounds
<< ,1003: too few valid records in window
<< ,1004: near zero covariance in bootstrap indicating bad fit
<< ,1005: covariance from curvature of fit yieled negative variances indicating bad fit
<< ,1010: no temperature-respiration relationship found
<< ,1011: too few valid records in window (from different location: partGLFitLRCOneWindow)
}}

\author{TW, MM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partGLFitLRCWindows}}

\code{\link{LightResponseCurveFitter_optimLRCBounds}}}

