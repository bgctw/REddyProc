\name{sEddyProc_initialize}
\alias{sEddyProc_initialize}
\title{sEddyProc initialize}
\description{Initializing sEddyProc class during \code{sEddyProc$new}.}
\usage{sEddyProc_initialize(ID = ID.s, Data = Data.F, 
    ColNames = ColNames.V.s, ColPOSIXTime = "DateTime", 
    DTS = if (!missing(DTS.n)) DTS.n else 48, 
    ColNamesNonNumeric = character(0), LatDeg = NA_real_, 
    LongDeg = if (!missing(Long_deg.n)) Long_deg.n else NA_real_, 
    TimeZoneHour = if (!missing(TimeZone_h.n)) TimeZone_h.n else NA_integer_, 
    ID.s, Data.F, ColNames.V.s, ColPOSIXTime.s, 
    DTS.n, ColNamesNonNumeric.V.s, Lat_deg.n, 
    Long_deg.n, TimeZone_h.n, ...)}
\arguments{
  \item{ID}{String with site ID}
  \item{Data}{Data frame with at least three month
of (half-)hourly site-level eddy data}
  \item{ColNames}{Vector with selected column names,
the fewer columns the faster the processing}
  \item{ColPOSIXTime}{Column name with POSIX time stamp}
  \item{DTS}{Daily time steps}
  \item{ColNamesNonNumeric}{Names of columns that should not
be checked for numeric type, e.g. season column}
  \item{LatDeg}{Latitude
in (decimal) degrees (-90 to + 90)}
  \item{LongDeg}{Longitude
in (decimal) degrees (-180 to + 180)}
  \item{TimeZoneHour}{Time
zone: hours shift to UTC, e.g. 1 for Berlin}
  \item{ID.s}{deprecated}
  \item{Data.F}{deprecated}
  \item{ColNames.V.s}{deprecated}
  \item{ColPOSIXTime.s}{deprecated}
  \item{DTS.n}{deprecated}
  \item{ColNamesNonNumeric.V.s}{deprecated}
  \item{Lat_deg.n}{deprecated}
  \item{Long_deg.n}{deprecated}
  \item{TimeZone_h.n}{deprecated}
  \item{\dots}{('...' required for initialization of class fields)}
}
\details{The time stamp must be provided in POSIX format, see also
\code{\link{fConvertTimeToPosix}}.
For required properties of the time series, see \code{\link{fCheckHHTimeSeries}}.

Internally the half-hour time stamp is shifted to the middle of the
measurement period (minus 15 minutes or 30 minutes).

All other columns may only contain numeric data.
Please use NA as a gap flag for missing data or low quality data not to
be used in the processing.
The columns are also checked for plausibility with warnings if outside range.

There are several fields initialized within the class.

sID is a string for the site ID.

sDATA is a data frame with site data.

sTEMP is a temporal data frame with the processing results.

sINFO is a list containing the time series information:\describe{
\item{DIMS}{Number of data rows}
\item{DTS}{Number of daily time steps (24 or 48)}
\item{Y.START}{Starting year}
\item{Y.END}{Ending year}
\item{Y.NUMS}{Number of years}
\item{Y.NAME}{Name for years}
}

\code{sUSTAR_SCEN} a data.frame 	with first column the season, and other
columns different uStar threshold estimates, as returned by
\code{\link{usGetAnnualSeasonUStarMap}}

sLOCATION is a list of information on site location and timezone
(see \code{\link{sEddyProc_sSetLocationInfo}}).

sTEMP is a data frame used only temporally.}
\value{Initialized fields of sEddyProc.}

\author{AMM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





