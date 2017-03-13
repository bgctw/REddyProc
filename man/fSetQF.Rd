\name{fSetQF}
\alias{fSetQF}
\title{Check and set quality flag}
\description{Generate new vector from data and quality flag column.}
\usage{fSetQF(Data.F, Var.s, QFVar.s, QFValue.n, CallFunction.s = "")}
\arguments{
  \item{Data.F}{Data frame}
  \item{Var.s}{Variable to be filtered}
  \item{QFVar.s}{Quality flag of variable to be filtered}
  \item{QFValue.n}{Numeric value of quality flag for _good_ data, other data is set to missing}
  \item{CallFunction.s}{Name of function called from}
}
\details{Quality flag will be applied to variable - unless quality flag variables is called 'none' (dummy).}
\value{Numeric vector with _good_ data.}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





