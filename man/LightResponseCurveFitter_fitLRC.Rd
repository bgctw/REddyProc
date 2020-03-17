\name{LightResponseCurveFitter_fitLRC}
\alias{LightResponseCurveFitter_fitLRC}
\title{LightResponseCurveFitter fitLRC}
\description{Optimize rectangular hyperbolic light response curve in one window}
\usage{LightResponseCurveFitter_fitLRC(dsDay, E0, 
    sdE0, RRefNight, controlGLPart = partGLControl(), 
    lastGoodParameters = rep(NA_real_, 7L))}
\arguments{
  \item{dsDay}{data.frame with columns NEE, Rg, Temp_C, VPD, and no NAs in NEE}
  \item{E0}{temperature sensitivity of respiration}
  \item{sdE0}{standard deviation of E_0.n}
  \item{RRefNight}{basal respiration estimated from night time data}
  \item{controlGLPart}{further default parameters
(see \code{\link{partGLControl}})}
  \item{lastGoodParameters}{numeric vector returned
by last reasonable fit}
}
\details{Optimization is performed for three initial parameter sets that differ
by \code{beta0 (* 1.3, * 0.8)}.
From those three, the optimization result is selected that yielded
the lowest misfit.
Starting values are: \code{k = 0},
\code{beta = interpercentileRange(0.03, 0.97)} of
respiration, \code{alpha = 0.1}, \code{R_ref}
from nightTime estimate.
\code{E0} is fixed to the night-time estimate, but varies for estimating
parameter uncertainty.

If \code{controlGLPart.l$nBootUncertainty == 0L} then the
covariance matrix of the
parameters is estimated by the Hessian of the LRC curve at optimum.
Then, the additional uncertainty and covariance with uncertainty E0
is neglected.

If \code{controlGLPart.l$nBootUncertainty > 0L} then the
covariance matrix of the
parameters is estimated by a bootstrap of the data.
In each draw, E0 is drawn from N ~ (E_0, sdE_0).

If there are no estimates for more than 20\% of the
bootstrapped samples
The an NA-result with convergence code 1001L is returned.}
\value{a list, If none of the optimizations from different starting
conditions converged, the parameters are NA.
\item{thetaOpt}{numeric vector of optimized parameters
including the fixed ones and E0}
\item{iOpt}{index of parameters that have been
optimized, here including E0, which has been optimized prior to
this function.}
\item{thetaInitialGuess}{the initial guess from data}
\item{covParms}{numeric matrix of the covariance matrix
of parameters, including E0}
\item{convergence}{integer code specifying
convergence problems: \\
0: good convergence \\
, 1-1000: see \code{\link{optim}} \\
, 1001: too few bootstraps converged \\
, 1002: fitted parameters were outside reasonable bounds \\
, 1003: too few valid records in window \\
, 1004: near zero covariance in bootstrap indicating bad fit \\
, 1005: covariance from curvature of fit yielded negative variances
indicating bad fit \\
, 1006: prediction of highest PAR in window was far from saturation
indicating insufficient data to constrain LRC \\
, 1010: no temperature-respiration relationship found \\
, 1011: too few valid records in window
(from different location: partGLFitLRCOneWindow) \\
}}

\author{TW, MM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{partGLFitLRCWindows}

\code{\link{LightResponseCurveFitter_optimLRCBounds}}}

