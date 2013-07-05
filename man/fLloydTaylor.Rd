\name{fLloydTaylor}
\alias{fLloydTaylor}
\title{Temperature dependence of soil respiration after Equation 11 in Lloyd & Taylor (1994)}

\usage{fLloydTaylor(R_ref.n, E_0.n, Tsoil.n, T_ref.n = 283.15, T_0.n = 227.13)}
\arguments{
  \item{R_ref.n}{Respiration rate at reference temperature}
  \item{E_0.n}{Temperature sensitivity ("activation energy") in Kelvin (degK)}
  \item{Tsoil.n}{Soil temperature in Kelvin (degK)}
  \item{T_ref.n}{Reference temperature of 10 degC in Kelvin (degK)}
  \item{T_0.n}{Regression temperature as fitted by LloydTaylor (1994) in Kelvin (degK)}
}

\value{Data vector of respiration rate}

\author{AMM
reference<<
Lloyd J, Taylor JA (1994) On the temperature dependence of soil respiration. Functional Ecology, 8, 315-323.
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





