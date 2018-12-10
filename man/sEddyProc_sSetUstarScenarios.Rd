\name{sEddyProc_sSetUstarScenarios}
\alias{sEddyProc_sSetUstarScenarios}
\title{sEddyProc sSetUstarScenarios}
\description{set uStar processing scenarios}
\usage{sEddyProc_sSetUstarScenarios(uStarTh, uStarSuffixes = colnames(uStarTh)[-1])}
\arguments{
  \item{uStarTh}{data.frame as returned by
\code{\link{usGetAnnualSeasonUStarMap}} or
\code{\link{usGetSeasonalSeasonUStarMap}}:
First column, season names, and remaining columns different estimates of
uStar Threshold.
If \code{uStarTh} has only one row, then each uStar threshold estimate is
applied to the entire dataset.
Entries in first column must match levels in argument \code{seasonFactor}
of \code{\link{sEddyProc_sEstUstarThresholdDistribution}}}
  \item{uStarSuffixes}{the suffixes appended to result column names
by default the column names of uStarTh unless its first season column}
}



\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{sEddyProc_sGetUstarScenarios}}}

