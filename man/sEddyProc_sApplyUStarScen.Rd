\name{sEddyProc_sApplyUStarScen}
\alias{sEddyProc_sApplyUStarScen}
\title{sEddyProc sApplyUStarScen}
\description{apply a function with changing the suffix argument}
\usage{sEddyProc_sApplyUStarScen(FUN, ..., uStarScenKeep = character(0))}
\arguments{
  \item{FUN}{function to be applied}
  \item{\dots}{further arguments to FUN}
  \item{uStarScenKeep}{Scalar string specifying the scenario
for which to keep parameters. If not specified defaults to the first
entry in \code{uStarSuffixes}.}
}
\details{When repeating computations, some of the
output variables maybe replaced. Argument \code{uStarKeep}
allows to select the scenario which is computed last,
and hence to which output columns refer to.}


\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





