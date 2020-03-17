\name{POSIXctToBerkeleyJulianDate}
\alias{POSIXctToBerkeleyJulianDate}
\title{POSIXctToBerkeleyJulianDate}
\description{convert POSIXct to JulianDate format used in Berkeley release}
\usage{POSIXctToBerkeleyJulianDate(sDateTime)}
\arguments{
  \item{sDateTime}{POSIXct vector}
}
\details{In the Berkeley-Release of the Fluxnet data, the time is stored as an number
with base10-digits representing YYYYMMddhhmm}


\author{TW,
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{BerkeleyJulianDateToPOSIXct}},}

