\name{POSIXctToBerkeleyJulianDate}
\alias{POSIXctToBerkeleyJulianDate}
\title{POSIXctToBerkeleyJulianDate}
\description{convert POSIXct to JulianDate format used in Berkeley release}
\usage{POSIXctToBerkeleyJulianDate(sDateTime)}
\arguments{
  \item{sDateTime}{POSIXct vector}
}
\details{In the Berkeley-Release of the fluxnet data, the time is stored as an number
with base10-digits representing YYYYMMddhhmm}


\author{TW,
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{BerkeleyJulianDateToPOSIXct}}}

