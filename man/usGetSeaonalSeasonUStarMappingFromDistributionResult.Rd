\name{usGetSeaonalSeasonUStarMappingFromDistributionResult}
\alias{usGetSeaonalSeasonUStarMappingFromDistributionResult}
\title{usGetSeaonalSeasonUStarMappingFromDistributionResult}
\description{extract mapping season -> uStar columns from Distribution result (\code{\link{sEstUstarThresholdDistribution}}}
\usage{usGetSeaonalSeasonUStarMappingFromDistributionResult(uStarTh)}
\arguments{
  \item{uStarTh}{result of \code{\link{sEstUstarThresholdDistribution}} or \code{\link{sEstUstarThreshold}}$uStarTh}
}
\details{If there are any NA values for some seasons (maybe due to many invalid bootstrap samples), 
then the thresholds for those seasons are replaced by the corresponding annual estimates.}
\value{a data frame with first column the season, and other columns different uStar threshold estimates}

\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





