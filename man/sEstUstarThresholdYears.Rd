\name{sEstUstarThresholdYears}
\alias{sEstUstarThresholdYears}
\title{sEstUstarThresholdYears}
\description{apply sEstUstarThresholdYear for each year in ds}
\usage{sEstUstarThresholdYears(ds = sDATA, boot = 1:nrow(ds), ..., seasonFactor.v = createSeasonFactorMonth(ds$sDateTime), 
    yearFactor.v = as.POSIXlt(ds$sDateTime)$year + 1900, UstarColName = "Ustar", 
    NEEColName = "NEE", TempColName = "Tair", RgColName = "Rg", 
    ctrlUstarSub.l = controlUstarSubsetting(), isCleaned = FALSE)}
\arguments{
  \item{ds}{data.frame with columns}
  \item{boot}{indices for calling by \code{link{boot}}}
  \item{\dots}{further arguments to \code{\link{sEstUstarThreshold}}}
  \item{seasonFactor.v}{factor of seasons to split}
  \item{yearFactor.v}{factor vector (nrow(dsYear) of seasons to split}
  \item{UstarColName}{
}
  \item{NEEColName}{
}
  \item{TempColName}{
}
  \item{RgColName}{
}
  \item{ctrlUstarSub.l}{
}
  \item{isCleaned}{set to TRUE to avoid call to \code{\link{cleanUStarSeries}}.}
}

\value{a vector with Ustar Threshold estimates. Names correspond to the year of the estimate }
\references{inspired by Papale 2006}
\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





