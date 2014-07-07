\name{createSeasonFactorYday}
\alias{createSeasonFactorYday}
\title{createSeasonFactorYday}
\description{calculate factors to denote the season for uStar-Filtering by specifying starting day of years}
\usage{createSeasonFactorYday(dates, yday = as.POSIXlt(dates)$yday, 
    startYday = c(335, 60, 152, 244) - 1)}
\arguments{
  \item{dates}{POSIXct vector of length of the data set to be filled}
  \item{yday}{integer (0-11) vector of length of the data set to be filled, specifying the month for each record}
  \item{startYday}{integer vector (0-366) specifying the starting month for each season}
}

\value{Integer vector of nrow ds, each unique class representing one season}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





