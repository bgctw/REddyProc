\name{usControlUstarSubsetting}
\alias{usControlUstarSubsetting}
\title{usControlUstarSubsetting}
\description{Default list of parameters for subsetting the data for uStarThreshold estimation into classes with similar environmental conditions}
\usage{usControlUstarSubsetting(taClasses = 7, UstarClasses = 20, 
    swThr = 10, minRecordsWithinTemp = 100, 
    minRecordsWithinSeason = 160, minRecordsWithinYear = 3000, 
    isUsingOneBigSeasonOnFewRecords = TRUE)}
\arguments{
  \item{taClasses}{set number of air temperature classes}
  \item{UstarClasses}{set number of Ustar classes}
  \item{swThr}{nighttime data threshold for solar radion [Wm-2]}
  \item{minRecordsWithinTemp}{integer scalar: the minimum number of Records within one Temperature-class}
  \item{minRecordsWithinSeason}{integer scalar: the minimum number of Records within one season}
  \item{minRecordsWithinYear}{integer scalar: the minimum number of Records within one year}
  \item{isUsingOneBigSeasonOnFewRecords}{boolean scalar: set to FALSE to avoid aggregating all seasons on too few records}
}



\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{usEstUstarThresholdSingleFw2Binned}}, \code{\link{usControlUstarSubsetting}}}
\examples{
usControlUstarSubsetting()
}
