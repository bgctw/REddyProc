\name{sEddyProc.new}
\alias{sEddyProc.new}
\title{sEddyProc$new - Initialization of sEddyProc}
\description{Creates the fields of the sEddyProc R5 reference class for processing of half-hourly eddy data}
\usage{sEddyProc.new(ID.s, Data.F, ColNames.V.s, ColPOSIXTime.s = "DateTime", 
    DTS.n = 48, ColNamesNonNumeric.V.s = character(0), 
    ...)}
\arguments{
  \item{ID.s}{String with site ID}
  \item{Data.F}{Data frame with at least three month of half-hourly site-level eddy data}
  \item{ColNames.V.s}{Vector with selected column names, the less columns the faster the processing}
  \item{ColPOSIXTime.s}{Column name with POSIX time stamp}
  \item{DTS.n}{Daily time steps}
  \item{ColNamesNonNumeric.V.s}{Names of columns that should not be checked for numeric type, e.g. season column}
  \item{\dots}{('...' required for initialization of class fields)}
}
\details{The time stamp must be provided in POSIX format, see also \code{\link{fConvertTimeToPosix}}.
For required properties of the time series, see \code{\link{fCheckHHTimeSeries}}.

Internally the half-hour time stamp is shifted to the middle of the measurement period (minus 15 minutes or 30 minutes).

All other columns may only contain numeric data.
Please use NA as a gap flag for missing data or low quality data not to be used in the processing.
The columns are also checked for plausibility with warnings if outside range.

sID is a string for the site ID.

sDATA is a data frame with site data.

sTEMP is a temporal data frame with the processing results.

sINFO is a list containing the time series information.

sTEMP is a data frame used only temporally.}
\value{Initialized fields of sEddyProc.}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





