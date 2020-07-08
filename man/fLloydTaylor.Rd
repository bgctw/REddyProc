\name{fLloydTaylor}
\alias{fLloydTaylor}
\title{Temperature dependence of soil respiration}
\description{Temperature dependence of soil respiration after Equation 11 in Lloyd & Taylor (1994)}
\usage{fLloydTaylor(RRef = R_ref.n, E0 = E_0.n, 
    TSoil = Tsoil.n, TRef = if (missing(T_ref.n)) 273.15 + 
        10 else T_ref.n, T0 = if (missing(T_0.n)) 227.13 else T_0.n, 
    R_ref.n, E_0.n, Tsoil.n, T_ref.n, T_0.n)}
\arguments{
  \item{RRef}{Respiration rate at reference temperature}
  \item{E0}{Temperature sensitivity ("activation energy")
in Kelvin (degK)}
  \item{TSoil}{Soil temperature in Kelvin (degK)}
  \item{TRef}{
}
  \item{T0}{
}
  \item{R_ref.n}{deprecated way to specify RRef}
  \item{E_0.n}{deprecated way to specify E0}
  \item{Tsoil.n}{deprecated way to specify Tsoil}
  \item{T_ref.n}{deprecated way to specify TRef}
  \item{T_0.n}{deprecated way to specify T0}
}

\value{Data vector of soil respiration rate (R, umol CO2 m-2 s-1)}

\author{AMM
reference<<
Lloyd J, Taylor JA (1994) On the temperature dependence of soil respiration.
Functional Ecology, 8, 315-323.
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}




\examples{
  T <- c(-10:30)
  resp <- fLloydTaylor(10, 330, T + 273.15)
plot(resp ~ T)
}
