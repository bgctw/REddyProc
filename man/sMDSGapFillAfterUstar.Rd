\name{sMDSGapFillAfterUstar}
\alias{sMDSGapFillAfterUstar}
\title{sEddyProc$sMDSAfterFiltering- MDS gap filling algorithm after u* filtering}
\description{Calling \code{\link{sMDSGapFill}} after filtering for (provided) friction velocity u*}
\usage{sMDSGapFillAfterUstar(FluxVar.s = "NEE", UstarVar.s = "Ustar", 
    UstarThres.n, UstarSuffix.s = "", ...)}
\arguments{
  \item{FluxVar.s}{Variable, i.e. collumn name,  of net ecosystem fluxes, default 'NEE'}
  \item{UstarVar.s}{Column name of friction velocity u* (ms-1), default 'Ustar'}
  \item{UstarThres.n}{u* threshold (ms-1)}
  \item{UstarSuffix.s}{Suffix required if different u* scenarios are filled}
  \item{\dots}{Other arguments passed to \code{\link{sMDSGapFill}}}
}
\details{The u* threshold(s) are provided for filtering the conditions of low turbulence.
After filtering, the data is gap filled using the MDS algorithm \code{\link{sMDSGapFill}}.}
\value{Vector with quality flag from filtering ustar (0 - good data, 1 - filtered data)}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





