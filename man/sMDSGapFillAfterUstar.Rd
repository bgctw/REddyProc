\name{sMDSGapFillAfterUstar}
\alias{sMDSGapFillAfterUstar}
\title{sEddyProc$sMDSGapFillAfterUstar - MDS gap filling algorithm after u* filtering}
\description{Calling \code{\link{sMDSGapFill}} after filtering for (provided) friction velocity u*}
\usage{sMDSGapFillAfterUstar(FluxVar.s, UstarVar.s = "Ustar", UstarThres.V.n, 
    UstarSuffix.s = "WithUstar", FlagEntryAfterLowTurbulence.b = FALSE, 
    addThresholdCols.b = TRUE, ...)}
\arguments{
  \item{FluxVar.s}{Flux variable to gap fill after ustar filtering}
  \item{UstarVar.s}{Column name of friction velocity u* (ms-1), default 'Ustar'}
  \item{UstarThres.V.n}{u* thresholds (ms-1) as single number or vector with values for each year}
  \item{UstarSuffix.s}{Different suffixes required for different u* scenarios}
  \item{FlagEntryAfterLowTurbulence.b}{Default FALSE for flagging the first entry after low turbulance as bad condition.}
  \item{addThresholdCols.b}{Default TRUE for adding the threshold and the flag (1 bad data) as output columns}
  \item{\dots}{Other arguments passed to \code{\link{sMDSGapFill}}}
}
\details{The u* threshold(s) are provided for filtering the conditions of low turbulence.
After filtering, the data is gap filled using the MDS algorithm \code{\link{sMDSGapFill}}.

With \code{isFlagEntryAfterLowTurbulence set to TRUE}, to be more conservative, in addition
to the data acquired when u* is below the threshold,
the first half hour measured with good turbulence conditions
after a period with low turbulence is also removed (Papaple et al. 2006).}
\value{Vector with quality flag from filtering (bad data are FALSE)
Gap filling results are in sTEMP data frame (with renamed columns) that can be retrieved by \code{\link{sExportResults}}.}

\author{AMM, TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





