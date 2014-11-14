\name{fOptimSingleE0}
\alias{fOptimSingleE0}
\title{Estimate temperature sensitivity E_0 using a Newton type optimization}
\description{Estimate temperature sensitivity E_0 of \code{\link{fLloydTaylor}} for a single time series
using a Newton type optimization.}
\usage{fOptimSingleE0(NEEnight.V.n, Temp_degK.V.n, Trim.n = 5, recoverOnError = FALSE)}
\arguments{
  \item{NEEnight.V.n}{(Original) nighttime ecosystem carbon flux, i.e. respiration vector}
  \item{Temp_degK.V.n}{(Original) air or soil temperature vector (degC)}
  \item{Trim.n}{Percentile to trim residual (\%)}
  \item{recoverOnError}{Set to TRUE to debug errors instead of catching them}
}

\value{Numeric vector with following components:
\item{R_ref}{Estimate of respiration rate at reference temperature}
\item{R_ref_SD}{Standard deviation of R_ref}
\item{E_0}{Estimate of temperature sensitivity ("activation energy") in Kelvin (degK) for untrimmed dataset}
\item{E_0_SD}{Standard deviation of E_0}
\item{E_0_trim}{Estimate of temperature sensitivity ("activation energy") in Kelvin (degK) for trimmed dataset}
\item{E_0_trim_SD}{Standard deviation of E_0_trim}}

\author{AMM, TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





