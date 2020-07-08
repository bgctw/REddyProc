\name{LightResponseCurveFitter_optimLRCOnAdjustedPrior}
\alias{LightResponseCurveFitter_optimLRCOnAdjustedPrior}
\title{LightResponseCurveFitter optimLRCOnAdjustedPrior}
\description{Lower bound flux uncertainty and adjust prior uncertainty before calling optimLRC}
\usage{LightResponseCurveFitter_optimLRCOnAdjustedPrior(theta, 
    iOpt, dsDay, parameterPrior, ctrl, ...)}
\arguments{
  \item{theta}{numeric vector of starting values}
  \item{iOpt}{integer vector: positions of subset of parameters
that are optimized}
  \item{dsDay}{dataframe of NEE, sdNEE and predictors Rg, VPD and Temp}
  \item{parameterPrior}{numeric vector of prior parameter estimates
(corresponding to theta) # TODO rename to thetaPrior}
  \item{ctrl}{list of further controls}
  \item{\dots}{further arguments to
\code{\link{LightResponseCurveFitter_optimLRC}}
(passed to \code{\link{LightResponseCurveFitter_computeCost}})}
}
\details{Only those records are used for optimization where both NEE
and sdNEE are finite.
In larger settings, already filtered at

Optimization of LRC parameters takes into account the uncertainty
of the flux values. In order to avoid very strong leverage, values with a
very low uncertainty (< a lower quantile) are assigned the lower quantile is
assigned. This procedure downweighs records with a high uncertainty, but does
not apply a large leverage for records with a very low uncertainty. Avoid
this correction by setting \code{ctrl$isBoundLowerNEEUncertainty =
FALSE}

The uncertainty of the prior, that maybe derived from fluxes)  is allowed to
adapt to the uncertainty of the fluxes.
This is done in \code{link{LightResponseCurveFitter_getPriorScale}}}
\value{result of \code{\link{LightResponseCurveFitter_optimLRC}} with
items theta, iOpt and convergence}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}





