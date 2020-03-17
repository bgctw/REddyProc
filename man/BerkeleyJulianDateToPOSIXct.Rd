\name{BerkeleyJulianDateToPOSIXct}
\alias{BerkeleyJulianDateToPOSIXct}
\title{BerkeleyJulianDateToPOSIXct}
\description{convert JulianDate format used in Berkeley release to POSIXct}
\usage{BerkeleyJulianDateToPOSIXct(julianDate, tz = "GMT", 
    ...)}
\arguments{
  \item{julianDate}{numeric vector representing times (see details for format)}
  \item{tz}{time zone used to represent the dates}
  \item{\dots}{further arguments to \code{\link{strptime}}, such as tz}
}
\details{In the Berkeley-Release of the fluxnet data, the time is stored as an number
with base10-digits representing YYYYMMddhhmm}


\author{TW,
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{POSIXctToBerkeleyJulianDate}}
\code{\link{fConvertTimeToPosix}}}

