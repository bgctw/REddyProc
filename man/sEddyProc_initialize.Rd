\name{sEddyProc_initialize}
\alias{sEddyProc_initialize}
\title{sEddyProc_initialize - Initialization of sEddyProc}
\description{This function is called when writing \code{sEddyProc$new}.
It creates the fields of the sEddyProc R5 reference class for processing of half-hourly eddy data}
\usage{sEddyProc_initialize(ID.s, Data.F, ColNames.V.s, 
    ColPOSIXTime.s = "DateTime", DTS.n = 48, 
    ColNamesNonNumeric.V.s = character(0), 
    Lat_deg.n = NA_real_, Long_deg.n = NA_real_, 
    TimeZone_h.n = NA_integer_, ...)}
\arguments{
  \item{ID.s}{String with site ID}
  \item{Data.F}{Data frame with at least three month of half-hourly site-level eddy data}
  \item{ColNames.V.s}{Vector with selected column names, the less columns the faster the processing}
  \item{ColPOSIXTime.s}{Column name with POSIX time stamp}
  \item{DTS.n}{Daily time steps}
  \item{ColNamesNonNumeric.V.s}{Names of columns that should not be checked for numeric type, e.g. season column}
  \item{Lat_deg.n}{Latitude in (decimal) degrees (-90 to + 90)}
  \item{Long_deg.n}{Longitude in (decimal) degrees (-180 to + 180)}
  \item{TimeZone_h.n}{Time zone (in hours) shift to UTC, e.g. 1 for Berlin}
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

sINFO is a list containing the time series information.\describe{
\item{DIMS}{Number of data rows}
\item{DTS}{Number of daily time steps (24 or 48)}
\item{Y.START}{Starting year}
\item{Y.END}{Ending year}
\item{Y.NUMS}{Number of years}
\item{Y.NAME}{Name for years}
}

sLOCATION is a list of information on site location and timezone (see \code{\link{sEddyProc_sSetLocationInfo}}).

sTEMP is a data frame used only temporally.}
\value{Initialized fields of sEddyProc.}

\author{AMM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





