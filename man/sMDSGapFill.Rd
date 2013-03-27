\name{sMDSGapFill}
\alias{sMDSGapFill}
\title{sEddyProc$sMDSGapFill - MDS gap filling algorithm}
\description{MDS gap filling algorithm adapted after the PV-Wave code and paper by Markus Reichstein.}
\usage{
sMDSGapFill(Var.s,
QFvar.s = "none",
QFvalue.n = NA_real_,
V1.s = "Rg",
T1.n = 50,
V2.s = "VPD",
T2.n = 5,
V3.s = "Tair",
T3.n = 2.5,
V4.s = "none",
T4.n = NA_real_,
V5.s = "none",
T5.n = NA_real_,
Verbose.b = TRUE)
}
\arguments{
  \item{Var.s}{Variable to be filled}
  \item{QFvar.s}{Quality flag of variable to be filled}
  \item{QFvalue.n}{Value of quality flag for _good_ data, other data is set to missing}
  \item{V1.s}{Condition variable 1 (default: Global radiation 'Rg' in  W m-2)}
  \item{T1.n}{Tolerance interval 1 (default: 50 W m-2)}
  \item{V2.s}{Condition variable 2 (default: Vapour pressure deficit 'VPD' in hPa)}
  \item{T2.n}{Tolerance interval 2 (default: 5 hPa)}
  \item{V3.s}{Condition variable 3 (default: Air temperature in °C)}
  \item{T3.n}{Tolerance interval 3 (default: 2.5 °C)}
  \item{V4.s}{Condition variable 4}
  \item{T4.n}{Tolerance interval 4}
  \item{V5.s}{Condition variable 5}
  \item{T5.n}{Tolerance interval 5}
  \item{Verbose.b}{Print status information to screen}
}
\details{sTEMP data frame for temporally generated gap filled data and qualifiers.

Long gaps (larger than 60 days) are not filled.

To run only MDC, set condition variable V1.s to 'none'}
\value{Gap filling columns added to sDATA frame.}
\references{Reichstein M, Falge E, Baldocchi D et al. (2005) On the separation of net ecosystem exchange 
into assimilation and ecosystem respiration: review and improved algorithm. Global Change Biology, 11, 1424-1439.}
\author{AMM
(Antje M. Moffat, Kerstin Sickel, Thomas Wutzler)}





