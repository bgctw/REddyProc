\name{usControlUstarSubsetting}
\alias{usControlUstarSubsetting}
\title{usControlUstarSubsetting}
\description{Default list of parameters for subsetting the data for uStarThreshold estimation}
\usage{usControlUstarSubsetting(taClasses = 7, UstarClasses = 20, 
    swThr = 10, minRecordsWithinTemp = 100, 
    minRecordsWithinSeason = 160, minRecordsWithinYear = 3000, 
    isUsingOneBigSeasonOnFewRecords = TRUE)}
\arguments{
  \item{taClasses}{set number of air temperature classes}
  \item{UstarClasses}{set number of Ustar classes}
  \item{swThr}{nighttime data threshold for solar radion [Wm-2]}
  \item{minRecordsWithinTemp}{integer scalar: the minimum number of
Records within one Temperature-class}
  \item{minRecordsWithinSeason}{integer scalar: the minimum number of
Records within one season}
  \item{minRecordsWithinYear}{integer scalar: the minimum number of
Records within one year}
  \item{isUsingOneBigSeasonOnFewRecords}{boolean scalar: set to FALSE to
avoid aggregating all seasons on too few records}
}



\author{TW
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{usEstUstarThresholdSingleFw2Binned}}
, \code{\link{usControlUstarSubsetting}}}
\examples{
usControlUstarSubsetting()
}
