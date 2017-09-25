\name{whichValueGreaterEqualC}
\alias{whichValueGreaterEqualC}
\title{whichValueGreaterEqualC}

\description{Search first element in an integer vector that is larger} 
\usage{whichValueGreaterEqualC(x, threshold, iStart)}
\arguments{
  \item{x}{ increasingly sorted numeric vector to search }
  \item{threshold}{integer scalar: searched element will need to be greater or equal as this argument }
  \item{iStart}{index in vector to start search}
}

\value{
Scalar integer: first index in x, that is >= iStart, and whose value x[i] is >= threshold.
If no index was found, returns NA.
}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





