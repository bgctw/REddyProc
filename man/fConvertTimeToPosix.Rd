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
  \item{TFormat.s}{Abbreviation for implemented time formats,
see details}
  \item{Year.s}{Column name of year}
  \item{Month.s}{Column name of month}
  \item{Day.s}{Column name of day}
  \item{Hour.s}{Column name of hour}
  \item{Min.s}{Column name of min}
  \item{TName.s}{Column name of new column}
  \item{tz}{timezone used to store the data. Advised to keep
GMT to avoid daytime shifting issues}
}
\details{The different time formats are converted to POSIX (GMT) and a 'TimeDate'
column is prefixed to the data frame

Implemented time formats:
\describe{
\item{YDH}{ year, day of year, hour in decimal
(e.g. 1998, 1, 10.5).
The day (of year) format is (1-365 or 1-366 in leap years).
The hour format is decimal time (0.0-23.5).
}
\item{YMDH}{year, month, day of month, hour in decimal
(e.g. 1998, 1, 1, 10.5)
The month format is (1-12)
The day (of month) format is (1-31).
}
\item{YMDHM}{year, month, day of month, integer hour, minute
(e.g. 1998, 1, 1, 10, 30)
The hour format is (0-23)
The minute format is (0-59)
}
}}
\value{Data frame with prefixed POSIX time column.}

\author{AMM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{BerkeleyJulianDateToPOSIXct}}}
\examples{
# See unit test in test_fConvertTimeToPosix for example
}
