\name{whichValueGreaterEqualR}
\alias{whichValueGreaterEqualR}
\title{whichValueGreaterEqualR}
\description{search first element in an integer vector that is larger }
\usage{whichValueGreaterEqualR(x, threshold, iStart = 1L)}\arguments{
  \item{x}{increasingly sorted numeric vector to search}
  \item{threshold}{integer scalar: searched element will need to be greater or equal as this argument}
  \item{iStart}{index in vector to start search}
}

\value{Scalar integer: first index in x, that is >= iStart, and whose value x[i] is >= threshold.
If no index was found, returns NA}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





