\name{usCreateSeasonFactorYday}
\alias{usCreateSeasonFactorYday}
\title{usCreateSeasonFactorYday}
\description{calculate factors to denote the season for uStar-Filtering by specifying starting day of years}
\usage{usCreateSeasonFactorYday(dates, yday = as.POSIXlt(dates)$yday + 
    1L, year = as.POSIXlt(dates)$year + 1900L, startYday = c(335, 
    60, 152, 244))}
\arguments{
  \item{dates}{POSIXct vector of length of the data set to be filled}
  \item{yday}{integer (1-366) vector of length of the data set to be filled, specifying the day of the year (1..366) for each record}
  \item{year}{integer vector of length of the data set to be filled, specifying the year}
  \item{startYday}{integer vector (1-366) specifying the starting yearDay for each season in increasing order}
}
\details{With default parameterization, dates are assumed to denote begin or center of the eddy time period.
If working with dates that denote the end of the period, use \code{yday=as.POSIXlt(fGetBeginOfEddyPeriod(dates))$yday}}
\value{Integer vector of nrow ds, each unique class representing one season}

\author{TW
seealso \code{\link{usCreateSeasonFactorMonth}}
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





