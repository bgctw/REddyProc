\name{sEddyProc_sTKFluxPartitionUStarScens}
\alias{sEddyProc_sTKFluxPartitionUStarScens}
\title{sEddyProc sTKFluxPartitionUStarScens}
\description{Flux partitioning after Keenan et al., 2019}
\usage{sEddyProc_sTKFluxPartitionUStarScens(..., 
    uStarScenKeep = character(0))}
\arguments{
  \item{\dots}{arguments to \code{\link{sEddyProc_sTKFluxPartition}}}
  \item{uStarScenKeep}{Scalar string specifying the scenario
for which to keep parameters (see \code{\link{sEddyProc_sApplyUStarScen}}.
Defaults to the first scenario.}
}
\details{Daytime-based partitioning of measured net ecosystem fluxes into
gross primary production (GPP) and ecosystem respiration (Reco)
for all u* threshold scenarios.}


\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}
\note{Currently only experimental.}




