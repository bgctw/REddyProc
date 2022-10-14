\name{sEddyProc_sGLFluxPartitionUStarScens}
\alias{sEddyProc_sGLFluxPartitionUStarScens}
\title{sEddyProc sGLFluxPartitionUStarScens}
\description{Flux partitioning after Lasslop et al. (2010)}
\usage{sEddyProc_sGLFluxPartitionUStarScens(..., 
    isWarnReplaceColumns = FALSE, warnOnOtherErrors = FALSE, 
    controlGLPart = partGLControl())}
\arguments{
  \item{\dots}{arguments to \code{\link{sEddyProc_sGLFluxPartition}}}
  \item{isWarnReplaceColumns}{overriding default to avoid
the warning on replacing output columns, because this is intended when
processing several uStar scenarios.}
  \item{warnOnOtherErrors}{Set to TRUE to only display a warning on
errors in uStarScenarios other than uStarScenKeep instead of stopping.}
  \item{controlGLPart}{further default parameters}
}
\details{Daytime-based partitioning of measured net ecosystem fluxes into
gross primary production (GPP) and ecosystem respiration (Reco)
for all u* threshold scenarios.

argument \code{uStarScenKeep} in ... is a scalar string specifying the scenario
for which to keep parameters (see \code{\link{sEddyProc_sApplyUStarScen}}.
Defaults to the first scenario,
which is usually the uStar without bootstrap: "uStar".
For the uStarScenKeep, a full set of output columns is returned.
For the other scenarios, the bootstrap of GPP uncertainty is omitted
and columns "FP_<x>" are overridden.}


\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}





