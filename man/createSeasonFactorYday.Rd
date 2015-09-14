\name{createSeasonFactorYday}
\alias{createSeasonFactorYday}
\title{createSeasonFactorYday}
\description{calculate factors to denote the season for uStar-Filtering by specifying starting day of years}
\usage{createSeasonFactorYday(dates, yday = as.POSIXlt(dates)$yday, 
    year = as.POSIXlt(dates)$year + 1900, startYday = c(335, 
        60, 152, 244) - 1)}
\arguments{
  \item{dates}{POSIXct vector of length of the data set to be filled}
  \item{yday}{integer (0-11) vector of length of the data set to be filled, specifying the month for each record}
  \item{year}{integer vector of length of the data set to be filled, specifying the year}
  \item{startYday}{integer vector (0-366) specifying the starting yearDay for each season}
}
\details{With default parameterization, dates are assumed to denote begin or center of the eddy time period.
If working with dates that denote the end of the period, use \code{yday=as.POSIXlt(fGetBeginOfEddyPeriod(dates))$yday}}
\value{Integer vector of nrow ds, each unique class representing one season}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
EddyData.F <- dss <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
EddyDataWithPosix.F <- ds <- fConvertTimeToPosix(dss, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
(res <- createSeasonFactorYday(ds$DateTime))
plot( res ~ ds$DateTime)
}
