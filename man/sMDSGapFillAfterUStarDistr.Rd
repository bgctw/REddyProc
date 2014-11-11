\name{sMDSGapFillAfterUStarDistr}
\alias{sMDSGapFillAfterUStarDistr}
\title{GapFilling for several filters of estimated friction velocity Ustar thresholds.}
\description{sEddyProc$sMDSGapFillUStarDistr - calling \code{\link{sMDSGapFillAfterUstar}} for several filters of friction velocity Ustar}
\usage{sMDSGapFillAfterUStarDistr(FluxVar.s = "NEE", UstarVar.s = "Ustar", 
    UstarThres.m.n, UstarSuffix.V.s = c("Ustar", "U05", "U50", 
        "U95"), ...)}\arguments{
  \item{FluxVar.s}{Variable, i.e. collumn name,  of net ecosystem fluxes, default 'NEE'}
  \item{UstarVar.s}{Column name of friction velocity u* (ms-1), default 'Ustar'}
  \item{UstarThres.m.n}{Numeric matrix( nYear x nUStar):
Each row is a vector of different ustar thresholds to apply before gap filling.
Make sure that one row is given for each year in the dataset to gap-Fill.
If only one row, i.e. a vector is given, then it is used for each year.}
  \item{UstarSuffix.V.s}{String vector
to distinguish results for different ustar values.
Its length must correspond to column numbers in \code{UstarThres.m.n}.
Default suffixes correspond to Ustar thresholds as 
Ustar: estimate on original series, U05: 5\% of bootstrap, U50: median of bootstrap, U95: 95\% of bootstrap) }
  \item{\dots}{other arguments to \code{\link{sMDSGapFillAfterUstar}} and \code{\link{sMDSGapFill}}}
}
\details{The eddy covariance method does not work with low turbulence conditions. Hence the periods with low turbulence
indicated by a low friction velocity u* needs to be filtered out and gapfilled.
The threshold value of a sufficient u* causes one of the largest uncertainty components within the gap-filled data. 
Hence, it is good practice to compare derived quantities based on gap-filled data using different u* threshold values.

The u* threshold(s) are provided for filtering the conditions of low turbulence.
After filtering, the data is gap filled using the MDS algorithm \code{\link{sMDSGapFill}}.}
\value{Matrix (columns correspond to u*Scenarios) with quality flag from filtering ustar (0 - good data, 1 - filtered data)

Gap filling results in sTEMP data frame (with renamed columns), that can be retrieved by \code{\link{sExportResults}}.
Each of the columns is calculated for several u*r-estimates and distinguished by a suffix after the variable. 
E.g. with using a \code{UstarThres.m.n} corresponding to the default  \code{UstarSuffix.V.s}, 
NEE for best UStar estimate is given in column NEE_UStar_f, 
and NEE based on lower and upper 90\% confidence interval estimates of Ustar threshold 
are returned in columns NEE_U05_f and NEE_U95_f respectively.}

\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





