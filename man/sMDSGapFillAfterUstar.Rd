\name{sMDSGapFillAfterUstar}
\alias{sMDSGapFillAfterUstar}
\title{sEddyProc$sMDSGapFillAfterUstar - MDS gap filling algorithm after u* filtering}
\description{Calling \code{\link{sMDSGapFill}} after filtering for (provided) friction velocity u*}
\usage{sMDSGapFillAfterUstar(FluxVar.s, UstarVar.s = "Ustar", 
    UstarThres.df = usGetAnnualSeasonUStarMappingFromDistributionResult(sUSTAR$uStarTh), 
    UstarSuffix.s = "WithUstar", FlagEntryAfterLowTurbulence.b = FALSE, 
    ...)}
\arguments{
  \item{FluxVar.s}{Flux variable to gap fill after ustar filtering}
  \item{UstarVar.s}{Column name of friction velocity u* (ms-1), default 'Ustar'}
  \item{UstarThres.df}{data.frame with first column, season names, and second column estimates of uStar Threshold.
<< Alternatively, a single value to be used as threshold for all records}
  \item{UstarSuffix.s}{Different suffixes required for different u* scenarios}
  \item{FlagEntryAfterLowTurbulence.b}{Set to TRUE for flagging the first entry after low turbulance as bad condition (by value of 2).}
  \item{\dots}{Other arguments passed to \code{\link{sMDSGapFill}}}
}
\details{The u* threshold(s) are provided with argument \code{UstarThres.df} for filtering the conditions of low turbulence.
After filtering, the data is gap filled using the MDS algorithm \code{\link{sMDSGapFill}}.

With \code{isFlagEntryAfterLowTurbulence set to TRUE}, to be more conservative, in addition
to the data acquired when u* is below the threshold,
the first half hour measured with good turbulence conditions
after a period with low turbulence is also removed (Papaple et al. 2006).}
\value{Vector with quality flag from filtering (here 0: good data, 1: low turbulence, 2: first half hour after low turbulence, 3: no threshold available, 4: missing uStar value)
Gap filling results are in sTEMP data frame (with renamed columns) that can be retrieved by \code{\link{sExportResults}}.}

\author{AMM, TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\itemize{
\item \code{\link{sMDSGapFillAfterUStarDistr}} for automated gapfilling for several u* threshold estimates.  
\item \code{\link{sEstUstarThreshold}} for estimating the u* threshold from the data.
}}

