\name{sEstUstarThresholdYears}
\alias{sEstUstarThresholdYears}
\title{sEstUstarThresholdYears}
\description{apply sEstUstarThresholdYear for each year in dataset}
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
  \item{UstarColName}{collumn name for UStar}
  \item{NEEColName}{collumn name for NEE}
  \item{TempColName}{collumn name for air temperature}
  \item{RgColName}{collumn name for solar radiation for omitting night time data}
  \item{ctrlUstarSub.l}{control parameters for subsetting time series (number of temperature and Ustar classes \ldots), see \code{\link{controlUstarSubsetting}}}
  \item{isCleaned}{set to TRUE to avoid call to \code{\link{cleanUStarSeries}}.}
}
\details{Due to change of surface roughness, the threshold of u* can change over years.
This methods estimates u* threshold for each subset by year.}
\value{a vector with Ustar Threshold estimates. Names correspond to the year of the estimate }
\references{inspired by Papale 2006}
\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{sEstUstarThreshold}}}

