\name{fCalcSunPosition}
\alias{fCalcSunPosition}
\title{fCalcSunPosition}
\description{Calculate the position of the sun}
\usage{fCalcSunPosition(DoY.V.n, Hour.V.n, Lat_deg.n, 
    Long_deg.n, TimeZone_h.n, useSolartime.b = TRUE)}
\arguments{
  \item{DoY.V.n}{Data vector with day of year (DoY)}
  \item{Hour.V.n}{Data vector with time as decimal hour}
  \item{Lat_deg.n}{Latitude in (decimal) degrees}
  \item{Long_deg.n}{Longitude in (decimal) degrees}
  \item{TimeZone_h.n}{Time zone (in hours)}
  \item{useSolartime.b}{by default corrects hour (given in local winter time) for latitude to solar time
<< where noon is exactly at 12:00. Set this to FALSE to compare to code that uses local winter time}
}
\details{This code assumes that Hour is given in local winter time zone, and corrects it by longitude to
solar time (where noon is exactly at 12:00).
Note: This is different form reference PVWave-code,
that does not account for solar time and uses winter time zone.
Set argument \code{useSolartime.b} to FALSE to use the local winter time instead.}
\value{Data list with the following items:
\item{SolTime}{Solar time (SolTime, hours)}
\item{SolDecl}{Solar declination (SolDecl, rad)}
\item{SolElev}{Solar elevation with 0 at horizon (SolElev, rad)}
\item{SolAzim}{Solar azimuth with 0 at North (SolAzim, rad)
}}

\author{AMM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





