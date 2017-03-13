\name{partGLFitLRC}
\alias{partGLFitLRC}
\title{partGLFitLRC}
\description{Optimize rectangular hyperbolic light response curve against data in one window and estimate uncertainty}
\usage{partGLFitLRC(dsDay, E0, sdE0, RRefNight, controlGLPart = partGLControl(), 
    lastGoodParameters)}
\arguments{
  \item{dsDay}{data.frame with columns NEE, Rg, Temp_C, VPD, and no NAs in NEE}
  \item{E0}{temperature sensitivity of respiration}
  \item{sdE0}{standard deviation of E_0.n}
  \item{RRefNight}{basal respiration estimated from night time data, used as starring value and prior estimate}
  \item{controlGLPart}{further default parameters (see \code{\link{partGLControl}})}
  \item{lastGoodParameters}{numeric vector of last good theta}
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
\item{opt.parms.V}{numeric vector of optimized parameters including
the fixed ones and including E0}
\item{iOpt}{index of parameters that have been optimized
, here including E0, which has been optimized prior to this function.}
\item{initialGuess.parms.V.n}{the initial guess from data}
\item{covParms}{numeric matrix of the covariance matrix of parameters, including E0
}}

\author{TW, MM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partGLFitLRCWindows}}

\code{\link{parmGLOptimLRCBounds}}}

