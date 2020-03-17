\name{sEddyProc_sMRFluxPartitionUStarScens}
\alias{sEddyProc_sMRFluxPartitionUStarScens}
\title{sEddyProc sMRFluxPartitionUStarScens}
\description{Flux partitioning after Reichstein et al. (2005)}
\usage{sEddyProc_sMRFluxPartitionUStarScens(..., 
    uStarScenKeep = character(0))}
\arguments{
  \item{\dots}{arguments to \code{\link{sEddyProc_sMRFluxPartition}}}
  \item{uStarScenKeep}{Scalar string specifying the scenario
for which to keep parameters (see \code{\link{sEddyProc_sApplyUStarScen}}.
Defaults to the first scenario.}
}
\details{Nighttime-based partitioning of measured net ecosystem fluxes into
gross primary production (GPP) and ecosystem respiration (Reco)
for all u* threshold scenarios.}
\value{NULL, it adds output columns in the class}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





