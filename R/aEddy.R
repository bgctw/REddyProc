#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc R5 reference class definition and methods +++
#+++ Dependencies: DataFunctions.R, package 'methods' for R5 reference class
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ sEddyProc class: Initialization
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' R5 reference class for processing of site-level half-hourly eddy data
#'
#' @import methods
#' @export sEddyProc
#' @exportClass sEddyProc
sEddyProc <- setRefClass('sEddyProc', fields = list(
  ## R5 reference class for processing of site-level half-hourly eddy data
  ##author<< AMM
  ##details<< with fields
  sID = 'character'       ##<< String with Site ID
  , sDATA = 'data.frame'   ##<< Data frame with (fixed) site data
  , sINFO = 'list'         ##<< List with site information
  , sLOCATION = 'list'		##<< List with site location information
  , sTEMP = 'data.frame'   ##<< Data frame with (temporary) result data
  , sUSTAR_DETAILS = 'list'		##<< List with results from uStar Threshold estimation
  , sUSTAR = 'data.frame'		  ##<< data.frame with uStar thresholds per
  ## aggregation mode and season
  , sUSTAR_SCEN = 'data.frame'		##<< data.frame with uStar thresholds per season
  # Note: The documenation of the class is not processed by 'inlinedocs'
))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_initialize <- function(
  ### Initializing sEddyProc class during \code{sEddyProc$new}.
  ID = ID.s                ##<< String with site ID
  , Data = Data.F             ##<< Data frame with at least three month
  ## of (half-)hourly site-level eddy data
  , ColNames = ColNames.V.s      ##<< Vector with selected column names,
  ## the fewer columns the faster the processing
  , ColPOSIXTime = 'DateTime'    ##<<  Column name with POSIX time stamp
  , DTS = if (!missing(DTS.n)) DTS.n else 48           ##<< Daily time steps
  , ColNamesNonNumeric = character(0)	 ##<< Names of columns that should not
  ## be checked for numeric type, e.g. season column
  , LatDeg = NA_real_    	##<< Latitude
  ##  in (decimal) degrees (-90 to + 90)
  , LongDeg = if (!missing(Long_deg.n)) Long_deg.n else NA_real_  ##<< Longitude
  ##  in (decimal) degrees (-180 to + 180)
  , TimeZoneHour = if (!missing(TimeZone_h.n)) TimeZone_h.n else NA_integer_	##<< Time
  ## zone: hours shift to UTC, e.g. 1 for Berlin
  , ID.s                  ##<< deprecated
  , Data.F                ##<< deprecated
  , ColNames.V.s          ##<< deprecated
  , ColPOSIXTime.s        ##<< deprecated
  , DTS.n                 ##<< deprecated
  , ColNamesNonNumeric.V.s ##<< deprecated
  , Lat_deg.n		  ##<< deprecated
  , Long_deg.n		##<< deprecated
  , TimeZone_h.n	##<< deprecated
  , ...                ##<< ('...' required for initialization of class fields)
  ##author<< AMM
) {
  if (!missing(ColNamesNonNumeric.V.s)) ColNamesNonNumeric <- ColNamesNonNumeric.V.s
  if (!missing(ColPOSIXTime.s)) ColPOSIXTime <- ColPOSIXTime.s
  if (!missing(Lat_deg.n)) LatDet <- Lat_deg.n
  varNamesDepr <- c(
    "ID.s","Data.F","ColNames.V.s","ColPOSIXTime.s","DTS.n"
    ,"ColNamesNonNumeric.V.s","Lat_deg.n","Long_deg.n","TimeZone_h.n")
  varNamesNew <- c(
    "ID","Data","ColNames","ColPOSIXTime","DTS"
    ,"ColNamesNonNumeric","LatDeg","LongDeg","TimeZoneHour")
  iDepr = which(!c(
    missing(ID.s),missing(Data.F),missing(ColNames.V.s),missing(ColPOSIXTime.s)
    ,missing(DTS.n),missing(ColNamesNonNumeric.V.s),missing(Lat_deg.n)
    ,missing(Long_deg.n),missing(TimeZone_h.n)))
  if (length(iDepr)) warning(
    "Argument names ",varNamesDepr[iDepr]," have been deprecated."
    ," Please, use instead ", varNamesNew[iDepr])
  ##detail<< A method of class \code{\link{sEddyProc-class}}.
  # Check entries
  if (!fCheckValString(ID) || is.na(ID) )
    stop('For ID, a character string must be provided!')
  fCheckColNames(Data, c(ColPOSIXTime, ColNames), 'fNewSData')

  ##details<<
  ## The time stamp must be provided in POSIX format, see also
  ## \code{\link{fConvertTimeToPosix}}.
  ## For required properties of the time series, see \code{\link{fCheckHHTimeSeries}}.
  fCheckHHTimeSeries(Data[, ColPOSIXTime], DTS = DTS, 'sEddyProc.initialize')

  ##details<<
  ## Internally the half-hour time stamp is shifted to the middle of the
  ## measurement period (minus 15 minutes or 30 minutes).
  #half-period time offset in seconds
  Time.V.p <- Data[, ColPOSIXTime] - (0.5 * 24 / DTS * 60 * 60)

  ##details<<
  ## All other columns may only contain numeric data.
  ## Please use NA as a gap flag for missing data or low quality data not to
  ## be used in the processing.
  ## The columns are also checked for plausibility with warnings if outside range.
  fCheckColNum(
    Data, setdiff(ColNames, ColNamesNonNumeric), 'sEddyProc.initialize')
  fCheckColPlausibility(Data, ColNames, 'sEddyProc.initialize')

  ##details<< There are several fields initialized within the class.
  ##details<< sID is a string for the site ID.
  sID <<- ID
  ##details<< sDATA is a data frame with site data.
  sDATA <<- cbind(sDateTime = Time.V.p, Data[, ColNames, drop = FALSE])
  ##details<< sTEMP is a temporal data frame with the processing results.
  sTEMP <<- data.frame(sDateTime = Time.V.p)
  #Initialization of site data information from POSIX time stamp.
  YStart.n <- as.numeric(format(sDATA$sDateTime[1], '%Y'))
  YEnd.n <- as.numeric(format(sDATA$sDateTime[length(sDATA$sDateTime)], '%Y'))
  YNums.n <- (YEnd.n - YStart.n + 1)
  if (YNums.n > 1) {
    YName.s <- paste(substr(YStart.n, 3, 4), '-', substr(YEnd.n, 3, 4), sep = '')
  } else {
    YName.s <- as.character(YStart.n)
  }

  ##details<<
  ## sINFO is a list containing the time series information:
  ##describe<<
  sINFO <<- list(
    DIMS = length(sDATA$sDateTime) ##<< Number of data rows
    , DTS = DTS                   ##<< Number of daily time steps (24 or 48)
    , Y.START = YStart.n            ##<< Starting year
    , Y.END = YEnd.n                ##<< Ending year
    , Y.NUMS = YNums.n              ##<< Number of years
    , Y.NAME = YName.s              ##<< Name for years
  )
  ##end<<

  ##details<<
  ## \code{sUSTAR_SCEN} a data.frame 	with first column the season, and other
  ## columns different uStar threshold estimates, as returned by
  ## \code{\link{usGetAnnualSeasonUStarMap}}
  sUSTAR_SCEN <<- data.frame()

  ##details<<
  ## sLOCATION is a list of information on site location and timezone
  ## (see \code{\link{sEddyProc_sSetLocationInfo}}).
  .self$sSetLocationInfo( LatDeg , LongDeg , TimeZoneHour)

  ##details<<
  ## sTEMP is a data frame used only temporally.

  #Initialize class fields
  message('New sEddyProc class for site \'', ID, '\'')

  # Required for initialization of class fields as last call of function
  callSuper(...)
  ##value<<
  ## Initialized fields of sEddyProc.
}
sEddyProc$methods( initialize = sEddyProc_initialize)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ sEddyProc class: Data handling functions
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sSetLocationInfo <- function(
	### set Location and time Zone information to sLOCATION
  LatDeg = if (!missing(Lat_deg.n)) Lat_deg.n else NA_real_    	##<< Latitude
  ##  in (decimal) degrees (-90 to + 90)
  , LongDeg = if (!missing(Long_deg.n)) Long_deg.n else NA_real_   	##<< Longitude
  ##  in (decimal) degrees (-180 to + 180)
  , TimeZoneHour = if (!missing(TimeZone_h.n)) TimeZone_h.n else NA_integer_	##<< Time
  ## zone: hours shift to UTC, e.g. 1 for Berlin
  , Lat_deg.n		  ##<< deprecated
	, Long_deg.n		##<< deprecated
	, TimeZone_h.n	##<< deprecated
) {
  varNamesDepr <- c("Lat_deg.n","Long_deg.n","TimeZone_h.n")
  varNamesNew <- c("LatDeg","LongDeg","TimeZoneHour")
  iDepr = which(!c(missing(Lat_deg.n),missing(Long_deg.n),missing(TimeZone_h.n)))
  if (length(iDepr)) warning(
    "Argument names ",varNamesDepr[iDepr]," have been deprecated."
    ," Please, use instead ", paste(varNamesNew[iDepr], collapse = ","))
  ##author<< TW
	# The information is used at several places (e.g. MRPartitioning, GLPartitioning)
	# and therefore should be stored with the class, instead of passed each time.
	if (!is.na(LatDeg) & (LatDeg < -90 | LatDeg > 90)) stop(
	  "Latitude must be in interval -90 to + 90")
	if (!is.na(LongDeg) & (LongDeg < -180 | LongDeg > 180)) stop(
	  "Longitude must be in interval -180 to + 180")
	if (!is.na(TimeZoneHour) &
	    (TimeZoneHour < -12 | TimeZoneHour > +12 |
	     TimeZoneHour != as.integer(TimeZoneHour))) stop(
	       "Timezone must be an integer in interval -12 to 12")
	sLOCATION <<- list(
			LatDeg = LatDeg
			, LongDeg = LongDeg
			, TimeZoneHour = TimeZoneHour
	)
}
sEddyProc$methods(sSetLocationInfo = sEddyProc_sSetLocationInfo)

#' @export
sEddyProc_sSetUstarScenarios <- function(
  ### set uStar processing scenarios
  uStarTh              ##<< data.frame as returned by
  ## \code{\link{usGetAnnualSeasonUStarMap}} or
  ## \code{\link{usGetSeasonalSeasonUStarMap}}:
  ## First column, season names, and remaining columns different estimates of
  ## uStar Threshold.
  ## If \code{uStarTh} has only one row, then each uStar threshold estimate is
  ## applied to the entire dataset.
  ## Entries in first column must match levels in argument \code{seasonFactor}
  ## of \code{\link{sEddyProc_sEstUstarThresholdDistribution}}
  , uStarSuffixes =    ##<< the suffixes appended to result column names
    ## by default the column names of uStarTh unless its first season column
    colnames(uStarTh)[-1]
) {
  ## The eddy covariance method does not work with low turbulence conditions.
  ## Hence the periods with low turbulence
  ## indicated by a low friction velocity u* needs to be filtered out and
  ## gapfilled (see \code{\link{sEddyProc_sMDSGapFill}}).
  ## The threshold value of a sufficient u* causes one of the largest
  ## uncertainty components within the gap-filled data.
  ## Hence, it is good practice to compare derived quantities based on
  ## gap-filled data using different u* threshold values.
  ##
  ## For example a user could provide the the following columns in argument
  ## \code{uStarTh}
  ## \itemize{
  ## \item season: identifier for which season this row is used.
  ## \item Ustar: estimate on original series
  ## \item U05: 5% of bootstrap
  ## \item U50: median of bootstrap
  ## \item U95: 95% of bootstrap)
  ## }
  ## Usually, Scenarios are retrieved from resuls of estimating the
  ## distribution by \code{\link{sEddyProc_sEstUstarThresholdDistribution}}
  ## and then taking either
  ## \itemize{
  ## \item annually aggregated value for all seasons
  ##   (\code{\link{usGetAnnualSeasonUStarMap}}) or
  ## \item seasonal estimates (\code{\link{usGetSeasonalSeasonUStarMap}}).
  ## }
  ##
  ## The following functions apply a processing step to all of the
  ## scenarios.
  ## \itemize{
  ## \item \code{\link{sEddyProc_sMDSGapFillUStarScens}}: gap-filling
  ## \item \code{\link{sEddyProc_sMRFluxPartitionUStarScens}}: flux-partitioning
  ## }
  ## The generated output columns are distinguished by appending a suffix
  ## to the respective column name.
  ## Then the spread across those columns is an estimate of the uncertainty
  ## introduced by not knowing the exact value of the u* threshold.
  #
  ##seealso<< \code{\link{sEddyProc_sGetUstarScenarios}}
  if (!("season" %in% colnames(sTEMP)) ) stop(
    "Seasons not defined yet. Add column 'season' to dataset with entries"
    , " matching column season in UstarThres.df, e.g. by calling"
    , " yourEddyProcClass$sSetUStarSeasons(...)")
  if (!all(is.finite(as.matrix(uStarTh[, -1])))) warning(
    "Provided non-finite uStarThreshold for some periods."
    ," All values in corresponding period will be marked as gap.")
  if (nrow(uStarTh) == 1L) {
    uStarTh <- cbind(data.frame(
      season = levels(.self$sTEMP$season)), uStarTh[, -1], row.names = NULL)
  }
  iMissing <- which( !(levels(.self$sTEMP$season) %in% uStarTh[[1]]))
  if (length(iMissing)) stop(
    "Need to provide uStar threshold for all seasons, but was missing for seasons"
    , paste(levels(.self$sTEMP$season)[iMissing], collapse = ","))
  nEstimates <- ncol(uStarTh) - 1L
  uStarSuffixes <- unique(uStarSuffixes)
  if (length(uStarSuffixes) != nEstimates) stop(
    "umber of unique suffixes must correspond to number of uStar-thresholds"
    ,", i.e. number of columns in uStarTh - 1.")
  sUSTAR_SCEN <<- uStarTh
  colnames(sUSTAR_SCEN)[-1] <<- uStarSuffixes
}
sEddyProc$methods(sSetUstarScenarios = sEddyProc_sSetUstarScenarios)

#' @export
sEddyProc_useSeaonsalUStarThresholds <- function(
  ### use seasonal estimates of uStar thresholds
) {
  ##seealso<< \code{\link{sEddyProc_sSetUstarScenarios}},
  ## \code{\link{sEddyProc_useAnnualUStarThresholds}}
  uStarThAgg <- .self$sGetEstimatedUstarThresholdDistribution()
  uStarMap <- usGetSeasonalSeasonUStarMap(uStarThAgg)
  .self$sSetUstarScenarios(uStarMap)
}
sEddyProc$methods(useSeaonsalUStarThresholds = sEddyProc_useSeaonsalUStarThresholds)

#' @export
sEddyProc_useAnnualUStarThresholds <- function(
  ### use seasonal estimates of uStar thresholds
) {
  ##seealso<< \code{\link{sEddyProc_sSetUstarScenarios}},
  ## \code{\link{sEddyProc_useSeaonsalUStarThresholds}}
  uStarThAgg <- .self$sGetEstimatedUstarThresholdDistribution()
  uStarMap <- usGetAnnualSeasonUStarMap(uStarThAgg)
  .self$sSetUstarScenarios(uStarMap)
}
sEddyProc$methods(useAnnualUStarThresholds = sEddyProc_useAnnualUStarThresholds)

#' @export
sEddyProc_sGetUstarScenarios <- function(
  ### get the current uStar processing scenarios
) {
  ##seealso<<
  ## \code{\link{sEddyProc_sSetUstarScenarios}}
  ##details<< the associated suffixes can be retrieved by
  ## \code{colnames(myClass$sGetUstarScenarios())[-1]}
  ##value<< a data.frame with first column listing each season and
  ## other column a scenario of uStar thresholds.
  if (!nrow(.self$sUSTAR_SCEN)) {
    warning("uStar scenarios not set yet. Setting to annual mapping.")
    .self$useAnnualUStarThresholds()
  }
  .self$sUSTAR_SCEN
}
sEddyProc$methods(sGetUstarScenarios = sEddyProc_sGetUstarScenarios)

#' @export
sEddyProc_sGetData <- function(
  ### Get class internal sDATA data frame
) {
  ##author<< AMM
  'Get class internal sDATA data frame'
    sDATA
    ##value<<
    ## Return data frame sDATA.
}
sEddyProc$methods( sGetData = sEddyProc_sGetData)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sExportData <- function()
  ### Export class internal sDATA data frame
  ##author<< AMM
{
	lDATA <- sDATA
	lDATA$sDateTime <- lDATA$sDateTime + (15L * 60L)
	colnames(lDATA) <- c('DateTime', colnames(lDATA)[-1])
	lDATA
	##value<<
	## Return data frame sDATA with time stamp shifted back to original.
}
sEddyProc$methods( sExportData = sEddyProc_sExportData)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' @export
sEddyProc_sExportResults <- function(
  ### Export class internal sTEMP data frame with result columns
  isListColumnsExported = FALSE	##<< if TRUE export list columns in addition
	  ## to numeric columns,
		## such as the covariance matrices of the the day-time-partitioning LRC fits
) {
    ##author<< AMM
    'Export class internal sTEMP data frame with result columns'
	iListColumns <- which(sapply(sTEMP, is.list) )
	iOmit <- if (isListColumnsExported) c(1L) else c(1L, iListColumns)
    sTEMP[, -iOmit]
    ##value<<
    ## Return data frame sTEMP with results.
}
sEddyProc$methods(sExportResults = sEddyProc_sExportResults)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc_sPrintFrames <- function(
    ##title<<
    ## sEddyProc$sPrintFrames - Print internal sDATA and sTEMP data frame
    ##description<<
    ## Print class internal sDATA and sTEMP data frame
    nRows = if (!missing(NumRows.i)) NumRows.i else 100  ##<< Number of rows to print
    , NumRows.i = 100         ##<< deprecated
)
    ##author<<
    ## AMM
{
    'Print class internal sDATA data frame'
    nRows <- min(nrow(sDATA), nrow(sTEMP), nRows)

    print(cbind(sDATA, sTEMP[, -1])[1:nRows, ])
    ##value<<
    ## Print the first rows of class internal sDATA and sTEMP data frame.
}
sEddyProc$methods(sPrintFrames = sEddyProc_sPrintFrames)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

