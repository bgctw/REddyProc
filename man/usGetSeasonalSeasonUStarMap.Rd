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
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





