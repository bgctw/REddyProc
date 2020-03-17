\name{usGetSeasonalSeasonUStarMap}
\alias{usGetSeasonalSeasonUStarMap}
\title{usGetSeasonalSeasonUStarMap}
\description{extract mapping season -> uStar columns from Distribution result}
\usage{usGetSeasonalSeasonUStarMap(uStarTh)}
\arguments{
  \item{uStarTh}{result of \code{\link{sEddyProc_sEstUstarThresholdDistribution}}
or \code{\link{sEddyProc_sEstUstarThreshold}}$uStarTh}
}
\details{from result of \code{\link{sEddyProc_sEstUstarThresholdDistribution}}}
\value{a data frame with first column the season, and other columns
different uStar threshold estimates}

\author{TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





