\name{sEddyProc_sGetEstimatedUstarThresholdDistribution}
\alias{sEddyProc_sGetEstimatedUstarThresholdDistribution}
\title{sEddyProc sGetEstimatedUstarThresholdDistribution}
\description{return the results of \code{\link{sEddyProc_sEstimateUstarScenarios}}}
\usage{sEddyProc_sGetEstimatedUstarThresholdDistribution()}

\value{A data.frame with columns \code{aggregationMode}, \code{year},
and \code{UStar} estimate based on the non-resampled data.
The other columns correspond to the quantiles of Ustar estimate
for given probabilities (argument \code{probs}) based on the distribution
of estimates using resampled the data.}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{sEddyProc_sSetUstarScenarios}}}

