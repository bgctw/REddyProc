\name{getTZone}
\alias{getTZone}
\title{getTZone}
\description{extracts the timezone attribute from POSIXct with default on missing}
\usage{getTZone(x, default = "GMT")}
\arguments{
  \item{x}{POSIXct vector}
  \item{default}{time zone returned,
if x has not timezone associated or attribute is the zero string}
}



\author{Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}




\examples{
getTZone(as.POSIXct("2010-07-01 16:00:00", tz = "etc/GMT-1") )
getTZone(as.POSIXct("2010-07-01 16:00:00") )
# printed with local time zone, but actually has no tz attribute
getTZone(Sys.time())
}
