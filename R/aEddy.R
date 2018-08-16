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
  ##author<<
  ## AMM, after example code of TW
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
  ##title<<
  ## sEddyProc_initialize - Initialization of sEddyProc
  ##description<<
  ## This function is called when writing \code{sEddyProc$new}.
  ## It creates the fields of the sEddyProc R5 reference class for processing
  ## of half-hourly eddy data
  ID.s                ##<< String with site ID
  , Data.F             ##<< Data frame with at least three month of half-hourly
  ## site-level eddy data
  , ColNames.V.s       ##<< Vector with selected column names, the less columns
  ## the faster the processing
  , ColPOSIXTime.s = 'DateTime' ##<< Column name with POSIX time stamp
  , DTS.n = 48           ##<< Daily time steps
  , ColNamesNonNumeric.V.s = character(0)	##<< Names of columns that should not
  ## be checked for numeric type, e.g. season column
  , Lat_deg.n = NA_real_    	##<< Latitude in (decimal) degrees (-90 to + 90)
  , Long_deg.n = NA_real_   	##<< Longitude in (decimal) degrees (-180 to + 180)
  , TimeZone_h.n = NA_integer_	##<< Time zone: hours shift to UTC, e.g. 1 for Berlin
  , ...                ##<< ('...' required for initialization of class fields)
  ##author<< AMM
) {
  ##detail<< A method of class \code{\link{sEddyProc-class}}.
  'Creates the fields of the sEddyProc R5 reference class for processing of half-hourly eddy data'
  # Check entries
  if (!fCheckValString(ID.s) || is.na(ID.s) )
    stop('For ID, a character string must be provided!')
  fCheckColNames(Data.F, c(ColPOSIXTime.s, ColNames.V.s), 'fNewSData')

  ##details<<
  ## The time stamp must be provided in POSIX format, see also
  ## \code{\link{fConvertTimeToPosix}}.
  ## For required properties of the time series, see \code{\link{fCheckHHTimeSeries}}.
  fCheckHHTimeSeries(Data.F[, ColPOSIXTime.s], DTS.n = DTS.n, 'sEddyProc.initialize')

  ##details<<
  ## Internally the half-hour time stamp is shifted to the middle of the
  ## measurement period (minus 15 minutes or 30 minutes).
  #half-period time offset in seconds
  Time.V.p <- Data.F[, ColPOSIXTime.s] - (0.5 * 24 / DTS.n * 60 * 60)

  ##details<<
  ## All other columns may only contain numeric data.
  ## Please use NA as a gap flag for missing data or low quality data not to
  ## be used in the processing.
  ## The columns are also checked for plausibility with warnings if outside range.
  fCheckColNum(
    Data.F, setdiff(ColNames.V.s, ColNamesNonNumeric.V.s), 'sEddyProc.initialize')
  fCheckColPlausibility(Data.F, ColNames.V.s, 'sEddyProc.initialize')

  ##details<<
  ## sID is a string for the site ID.
  sID <<- ID.s
  ##details<<
  ## sDATA is a data frame with site data.
  sDATA <<- cbind(sDateTime = Time.V.p, Data.F[, ColNames.V.s, drop = FALSE])
  ##details<<
  ## sTEMP is a temporal data frame with the processing results.
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
  ## sINFO is a list containing the time series information.
  ##describe<<
  sINFO <<- list(
    DIMS = length(sDATA$sDateTime) ##<< Number of data rows
    , DTS = DTS.n                   ##<< Number of daily time steps (24 or 48)
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
  .self$sSetLocationInfo( Lat_deg.n , Long_deg.n , TimeZone_h.n)

  ##details<<
  ## sTEMP is a data frame used only temporally.

  #Initialize class fields
  message('New sEddyProc class for site \'', ID.s, '\'')

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
	Lat_deg.n		##<< Latitude in (decimal) degrees (-90 to + 90)
	, Long_deg.n		##<< Longitude in (decimal) degrees (-180 to + 180)
	, TimeZone_h.n	##<< Time zone (in hours) shift to UTC, e.g. + 1 for Berlin
) {
	##author<< TW
	# The information is used at several places (e.g. MRPartitioning, GLPartitioning)
	# and therefore should be stored with the class, instead of passed each time.
	if (!is.na(Lat_deg.n) & (Lat_deg.n < -90 | Lat_deg.n > 90)) stop(
	  "Latitude must be in interval -90 to + 90")
	if (!is.na(Long_deg.n) & (Long_deg.n < -180 | Long_deg.n > 180)) stop(
	  "Longitude must be in interval -180 to + 180")
	if (!is.na(TimeZone_h.n) &
	    (TimeZone_h.n < -12 | TimeZone_h.n > + 12 |
	     TimeZone_h.n != as.integer(TimeZone_h.n))) stop(
	       "Timezone must be an integer in interval -12 to 12")
	sLOCATION <<- list(
			Lat_deg.n = Lat_deg.n
			, Long_deg.n = Long_deg.n
			, TimeZone_h.n = TimeZone_h.n
	)
}
sEddyProc$methods(sSetLocationInfo = sEddyProc_sSetLocationInfo)

#' @export
sEddyProc_sSetUstarScenarios <- function(
  ### set uStar processing scenarios
  uStarTh              ##<< data.frame as returned by
  ## \code{\link{usGetAnnualSeasonUStarMap}}:
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
sEddyProc_sGetUstarScenarios <- function(
  ### get the current uStar processing scenarios
) {
  ##seealso<<
  ## \code{\link{sEddyProc_sSetUstarScenarios}}
  ##details<< the associated suffixes can be retrieved by
  ## \code{colnames(myClass$sGetUstarScenarios())[-1]}
  ##value<< a data.frame with first column listing each season and
  ## other column a scneario of uStar thresholds.
  .self$sUSTAR_SCEN
}
sEddyProc$methods(sGetUstarScenarios = sEddyProc_sGetUstarScenarios)

#' @export
sEddyProc_sGetData <- function()
  ##title<<
  ## sEddyProc$sGetData - Get internal sDATA data frame
  ##description<<
  ## Get class internal sDATA data frame
  ##author<<
  ## AMM
{
    'Get class internal sDATA data frame'
    sDATA
    ##value<<
    ## Return data frame sDATA.
}
sEddyProc$methods( sGetData = sEddyProc_sGetData)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sExportData <- function()
    ##title<<
    ## sEddyProc$sExportData - Export internal sDATA data frame
    ##description<<
    ## Export class internal sDATA data frame
    ##author<<
    ## AMM
{
    'Export class internal sDATA data frame'
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
	  isListColumnsExported = FALSE	##<< if TRUE export list columns in addition
	  ## to numeric columns,
		## such as the covariance matrices of the the day-time-partitioning LRC fits
)
    ##title<<
    ## sEddyProc$sExportData - Export internal sTEMP data frame with result columns
    ##description<<
    ## Export class internal sTEMP data frame with result columns
    ##author<<
    ## AMM
{
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
    NumRows.i = 100         ##<< Number of rows to print
)
    ##author<<
    ## AMM
{
    'Print class internal sDATA data frame'
    NumRows.i <- min(nrow(sDATA), nrow(sTEMP), NumRows.i)

    print(cbind(sDATA, sTEMP[, -1])[1:NumRows.i, ])
    ##value<<
    ## Print the first rows of class internal sDATA and sTEMP data frame.
}
sEddyProc$methods(sPrintFrames = sEddyProc_sPrintFrames)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

