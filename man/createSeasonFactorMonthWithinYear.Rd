\name{createSeasonFactorMonthWithinYear}
\alias{createSeasonFactorMonthWithinYear}
\title{createSeasonFactorMonthWithinYear}
\description{calculate factors to denote the season for uStar-Filtering by specifying starting months, with seasons not spanning year boundaries}
\usage{createSeasonFactorMonthWithinYear(dates, month = as.POSIXlt(dates)$mon, 
    year = as.POSIXlt(dates)$year + 1900, startMonth = c(3, 6, 
        9, 12) - 1)}\arguments{
  \item{dates}{POSIXct vector of length of the data set to be filled}
  \item{month}{integer (0-11) vector of length of the data set to be filled, specifying the month for each record}
  \item{year}{integer vector of length of the data set to be filled, specifying the year}
  \item{startMonth}{integer vector specifying the starting month for each season, counting from zero, default is (Dez,Jan,Feb)(Mar,April,May)(June,July,August),(Sept,Okt,Nov)}
}
\details{If Jan is not a starting month, then the first months of each year will be 
part of the last period in the year.
E.g. with the default the fourth period of the first year consists of Jan,Feb,Dec.}
\value{Integer vector length(dates), with each unique value representing one season}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{createSeasonFactorYday}}}
\examples{
Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
EddyData.F <- dss <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
EddyDataWithPosix.F <- ds <- fConvertTimeToPosix(dss, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
(res <- createSeasonFactorMonthWithinYear(ds$DateTime))
(res2 <- createSeasonFactorYday(ds$DateTime)) # default days are chosen to correspond to start of Febr, June, Sept, and Dec
}
