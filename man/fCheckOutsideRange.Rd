\name{fCheckOutsideRange}
\alias{fCheckOutsideRange}
\title{fCheckOutsideRange}
\description{Check if specified variable is outside of provided boundaries}
\usage{fCheckOutsideRange(Data.F, VarName.s, Condition.V.s, 
    CallFunction.s = "")}
\arguments{
  \item{Data.F}{Data frame}
  \item{VarName.s}{Variable (column) name}
  \item{Condition.V.s}{Logical condition for outside values}
  \item{CallFunction.s}{Name of function called from}
}
\details{Example of condition structure: c('<=', 0) or c('<=',0,'|','>',20)
Allowed relational operators: < <= == >= > !=
Allowed logical operators: & |}


\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





