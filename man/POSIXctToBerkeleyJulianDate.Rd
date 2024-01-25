\name{POSIXctToBerkeleyJulianDate}
\alias{POSIXctToBerkeleyJulianDate}
\title{POSIXctToBerkeleyJulianDate}
\description{convert POSIXct to JulianDate format used in Berkeley release}
\usage{POSIXctToBerkeleyJulianDate(sDateTime, tz = getTZone(sDateTime))}
\arguments{
  \item{sDateTime}{POSIXct vector}
  \item{tz}{time zone attribute, such as "etc/GMT-1"}
}
\details{In the Berkeley-Release of the Fluxnet data, the time is stored as an number
with base10-digits representing YYYYMMddhhmm}


\author{TW,
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{BerkeleyJulianDateToPOSIXct}},}

