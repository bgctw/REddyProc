\name{getSeaonalSeasonUStarMappingFromDistributionResult}
\alias{getSeaonalSeasonUStarMappingFromDistributionResult}
\title{getSeaonalSeasonUStarMappingFromDistributionResult}
\description{extract mapping season -> uStar columns from Distribution result (\code{\link{sEstUstarThresholdDistribution}}}
\usage{getSeaonalSeasonUStarMappingFromDistributionResult(uStarTh)}
\arguments{
  \item{uStarTh}{
}
}
\details{missing thresholds are replace by corresponding estimates based on annually aggregated estimates
(\code{\link{getAnnualSeasonUStarMappingFromDistributionResult}}}
\value{a data frame with first column the season, and other columns different uStar threshold estimates}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





