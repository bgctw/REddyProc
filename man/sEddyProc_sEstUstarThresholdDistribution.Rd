\name{sEddyProc_sEstUstarThresholdDistribution}
\alias{sEddyProc_sEstUstarThresholdDistribution}
\title{sEddyProc sEstUstarThresholdDistribution}
\description{Estimate the distribution of u* threshold by bootstrapping over data}
\usage{sEddyProc_sEstUstarThresholdDistribution(...)}
\arguments{
  \item{\dots}{further parameters to
\code{\link{sEddyProc_sEstimateUstarScenarios}}}
}
\details{This method returns the results directly, without modifying
the class. It is there for portability reasons. Recommended is
using method \code{\link{sEddyProc_sEstimateUstarScenarios}} to
update the class and then getting the results from the class by
\code{\link{sEddyProc_sGetEstimatedUstarThresholdDistribution}}.}
\value{result of
\code{\link{sEddyProc_sGetEstimatedUstarThresholdDistribution}}}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}





