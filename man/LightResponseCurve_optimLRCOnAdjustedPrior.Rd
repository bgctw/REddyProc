\name{LightResponseCurve_optimLRCOnAdjustedPrior}
\alias{LightResponseCurve_optimLRCOnAdjustedPrior}
\title{LightResponseCurve optimLRCOnAdjustedPrior}
\description{<< Lower bound flux uncertainty and adjust prior uncertainty before calling optimLRC }
\usage{LightResponseCurve_optimLRCOnAdjustedPrior(theta, iOpt, 
    dsDay, parameterPrior, ctrl, ...)}
\arguments{
  \item{theta}{numeric vector of starting values}
  \item{iOpt}{integer vector: positions of subset of parameters that are optimized}
  \item{dsDay}{dataframe of NEE, sdNEE and predictors Rg, VPD and Temp}
  \item{parameterPrior}{numeric vector of prior parameter estimates (corresponding to theta) # TODO rename to thetaPrior}
  \item{ctrl}{list of further controls}
  \item{\dots}{further arguments to \code{\link{optimLRC}} (passed to \code{\link{computeCost}})}
}
\details{Only those records are used for optimization where both NEE and sdNEE are finite.

Optimization of LRC parameters takes into account the uncertainty of the flux values.
In order to avoid very strong leverage, values with a very low uncertainty (< median) are assigned
the median of the uncertainty.
This procedure downweighs records with a high uncertainty, but does not apply a large leverage for
records with a very low uncertainty. Avoid this correction by suppyling setting \code{ctrl$isBoundLowerNEEUncertainty = FALSE}

The uncertainty of the prior, that maybe derived from fluxes)  is allowed to 
adapt to the uncertainty of the fluxes.
This is done in \code{link{LightResponseCurve_getPriorScale}}}
\value{result of \code{\link{optimLRC}}}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





