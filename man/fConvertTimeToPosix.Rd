\name{fConvertTimeToPosix}
\alias{fConvertTimeToPosix}
\title{fConvertTimeToPosix}
\description{Convert different time formats to POSIX}
\usage{fConvertTimeToPosix(Data.F, TFormat.s, Year.s = "none", 
    Month.s = "none", Day.s = "none", Hour.s = "none", 
    Min.s = "none", TName.s = "DateTime", 
    tz = "GMT")}
\arguments{
  \item{Data.F}{Data frame with time columns to be converted}
  \item{TFormat.s}{Abbreviation for implemented time formats}
  \item{Year.s}{Column name of year}
  \item{Month.s}{Column name of month}
  \item{Day.s}{Column name of day}
  \item{Hour.s}{Column name of hour}
  \item{Min.s}{Column name of min}
  \item{TName.s}{Column name of new column}
  \item{tz}{timezone used to store the data. Advisded to keep
GMT to avoid daytime shifting issues}
}
\details{The different time formats are converted to POSIX (GMT) and a 'TimeDate'
column is prefixed to the data frame

Implemented time formats:}
\value{Data frame with prefixed POSIX time column.}

\author{AMM
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}




\examples{
# See unit test in test_fConvertTimeToPosix for example
}
