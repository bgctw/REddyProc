\name{fCalcPotRadiation}
\alias{fCalcPotRadiation}
\title{fCalcPotRadiation}
\description{Calculate the potential radiation}
\usage{fCalcPotRadiation(DoY = DoY.V.n, Hour = Hour.V.n, 
    LatDeg = Lat_deg.n, LongDeg = Long_deg.n, 
    TimeZone = TimeZone_h.n, useSolartime = TRUE, 
    DoY.V.n, Hour.V.n, Lat_deg.n, Long_deg.n, 
    TimeZone_h.n, useSolartime.b = TRUE)}
\arguments{
  \item{DoY}{Data vector with day of year (DoY), same length as Hour or length 1}
  \item{Hour}{Data vector with time as decimal hour of local time zone}
  \item{LatDeg}{Latitude in (decimal) degrees}
  \item{LongDeg}{Longitude in (decimal) degrees}
  \item{TimeZone}{Time zone (in hours)}
  \item{useSolartime}{
}
  \item{DoY.V.n}{deprecated}
  \item{Hour.V.n}{deprecated}
  \item{Lat_deg.n}{deprecated}
  \item{Long_deg.n}{deprecated}
  \item{TimeZone_h.n}{deprecated}
  \item{useSolartime.b}{deprecated}
}

\value{Data vector of potential radiation (PotRad, W_m-2)}

\author{AMM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}




\examples{
hour <- seq(8, 16, by = 0.1)
potRadSolar <- fCalcPotRadiation(160, hour, 39.94, -5.77, TimeZone = +1)
potRadLocal <- fCalcPotRadiation(160, hour, 39.94, -5.77, TimeZone = +1
	  , useSolartime = FALSE)
plot(potRadSolar ~ hour, type = 'l')
abline(v = 13, lty = "dotted")
lines(potRadLocal ~  hour, col = "blue")
abline(v = 12, col = "blue", lty = "dotted")
legend("bottomright", legend = c("solar time", "local winter time")
	, col = c("black", "blue"), inset = 0.05, lty = 1)
}
