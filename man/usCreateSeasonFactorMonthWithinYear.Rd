\name{usCreateSeasonFactorMonthWithinYear}
\alias{usCreateSeasonFactorMonthWithinYear}
\title{usCreateSeasonFactorMonthWithinYear}
\description{Compute year-bounded Seasonfactor by starting month}
\usage{usCreateSeasonFactorMonthWithinYear(dates, 
    month = as.POSIXlt(dates)$mon + 1, year = as.POSIXlt(dates)$year + 
        1900, startMonth = c(3, 6, 9, 12))}
\arguments{
  \item{dates}{POSIXct vector of length of the data set to be filled,
specifying the center-time of each record}
  \item{month}{integer (1-12) vector of length
of the data set to be filled, specifying the month for each record}
  \item{year}{integer vector of length of
the data set to be filled, specifying the year}
  \item{startMonth}{integer vector specifying the starting
month for each season, counting from one. Default is
(Dez, Jan, Feb)(Mar, April, May)(June, July, August), (Sept, Okt, Nov)}
}
\details{Calculate factors to denote the season for uStar-Filtering by specifying
starting months, with seasons not spanning year boundaries
If Jan is not a starting month, then the first months of each year will be
part of the last period in the year.
E.g. with the default the fourth period of the first year consists of
Jan, Feb, Dec.}
\value{Integer vector length(dates), with each unique value representing one season}

\author{TW
seealso \code{\link{usCreateSeasonFactorMonth}}
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}





