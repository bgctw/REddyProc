\name{fCalcSunPosition}
\alias{fCalcSunPosition}
\title{Calculate the position of the sun}

\usage{fCalcSunPosition(DoY.V.n, Hour.V.n, Lat_deg.n, Long_deg.n, TimeZone_h.n)}
\arguments{
  \item{DoY.V.n}{Data vector with day of year (DoY)}
  \item{Hour.V.n}{Data vector with time as decimal hour}
  \item{Lat_deg.n}{Latitude in (decimal) degrees}
  \item{Long_deg.n}{Longitude in (decimal) degrees}
  \item{TimeZone_h.n}{Time zone (in hours)}
}

\value{Data list with the following items:
\item{SolTime}{Solar time (SolTime, hours)}
\item{SolDecl}{Solar declination (SolDecl, rad)}
\item{SolElev}{Solar elevation with 0 at horizon (SolElev, rad)}
\item{SolAzim}{Solar azimuth with 0 at North (SolAzim, rad)
}}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





