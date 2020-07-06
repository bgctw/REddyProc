\name{sEddyProc_sSetLocationInfo}
\alias{sEddyProc_sSetLocationInfo}
\title{sEddyProc sSetLocationInfo}
\description{set Location and time Zone information to sLOCATION}
\usage{sEddyProc_sSetLocationInfo(LatDeg = if (!missing(Lat_deg.n)) Lat_deg.n else NA_real_, 
    LongDeg = if (!missing(Long_deg.n)) Long_deg.n else NA_real_, 
    TimeZoneHour = if (!missing(TimeZone_h.n)) TimeZone_h.n else NA_integer_, 
    Lat_deg.n, Long_deg.n, TimeZone_h.n)}
\arguments{
  \item{LatDeg}{Latitude
in (decimal) degrees (-90 to + 90)}
  \item{LongDeg}{Longitude
in (decimal) degrees (-180 to + 180)}
  \item{TimeZoneHour}{Time
zone: hours shift to UTC, e.g. 1 for Berlin}
  \item{Lat_deg.n}{deprecated}
  \item{Long_deg.n}{deprecated}
  \item{TimeZone_h.n}{deprecated}
}



\author{TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}





