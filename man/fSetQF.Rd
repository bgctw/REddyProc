\name{fSetQF}
\alias{fSetQF}
\title{Check and set quality flag}
\description{Generate new vector from data and quality flag column.}
\usage{
fSetQF(Data.F,
Var.s,
QFvar.s,
QFvalue.n,
CallFunction.s = "")
}
\arguments{
  \item{Data.F}{Data frame}
  \item{Var.s}{Variable to be filled}
  \item{QFvar.s}{Quality flag of variable to be filled}
  \item{QFvalue.n}{Numeric value of quality flag for _good_ data, other data is set to missing}
  \item{CallFunction.s}{Name of function called from}
}

\value{Numeric vector with _good_ data.}

\author{AMM
(Antje M. Moffat, Kerstin Sickel, Thomas Wutzler)}





