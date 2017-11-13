#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc R5 reference class definition and methods +++
#+++ Dependencies: DataFunctions.R, package 'methods' for R5 reference class
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ sEddyProc class: Initialization
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
  , sUSTAR = 'list'		##<< List with results form uStar Threshold estimation
# Note: The documenation of the class is not processed by 'inlinedocs'
))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_initialize <- function(
    ##title<<
    ## sEddyProc_initialize - Initialization of sEddyProc
    ##description<<
	## This function is called when writing \code{sEddyProc$new}.
    ## It creates the fields of the sEddyProc R5 reference class for processing of half-hourly eddy data
    ID.s                ##<< String with site ID
    , Data.F             ##<< Data frame with at least three month of half-hourly site-level eddy data
    , ColNames.V.s       ##<< Vector with selected column names, the less columns the faster the processing
    , ColPOSIXTime.s = 'DateTime' ##<< Column name with POSIX time stamp
    , DTS.n = 48           ##<< Daily time steps
	, ColNamesNonNumeric.V.s = character(0)	##<< Names of columns that should not be checked for numeric type, e.g. season column
	, Lat_deg.n = NA_real_    	##<< Latitude in (decimal) degrees (-90 to + 90)
	, Long_deg.n = NA_real_   	##<< Longitude in (decimal) degrees (-180 to + 180)
	, TimeZone_h.n = NA_integer_	##<< Time zone (in hours) shift to UTC, e.g. 1 for Berlin
    , ...                ##<< ('...' required for initialization of class fields)
    ##author<<
    ## AMM
    # TEST: ID.s <- 'Tha'; Data.F <- EddyDataWithPosix.F; ColPOSIXTime.s <- 'DateTime'; ColNames.V.s <- c('NEE', 'Rg', 'Tair', 'VPD'); DTS.n = 48
) {
	##detail<< A method of class \code{\link{sEddyProc-class}}.
    'Creates the fields of the sEddyProc R5 reference class for processing of half-hourly eddy data'
    # Check entries
    if (!fCheckValString(ID.s) || is.na(ID.s) )
      stop('For ID, a character string must be provided!')
    fCheckColNames(Data.F, c(ColPOSIXTime.s, ColNames.V.s), 'fNewSData')

    ##details<<
    ## The time stamp must be provided in POSIX format, see also \code{\link{fConvertTimeToPosix}}.
    ## For required properties of the time series, see \code{\link{fCheckHHTimeSeries}}.
    fCheckHHTimeSeries(Data.F[, ColPOSIXTime.s], DTS.n = DTS.n, 'sEddyProc.initialize')

    ##details<<
    ## Internally the half-hour time stamp is shifted to the middle of the measurement period (minus 15 minutes or 30 minutes).
    Time.V.p <- Data.F[, ColPOSIXTime.s] - (0.5 * 24 / DTS.n * 60 * 60)  #half-period time offset in seconds

    ##details<<
    ## All other columns may only contain numeric data.
    ## Please use NA as a gap flag for missing data or low quality data not to be used in the processing.
    ## The columns are also checked for plausibility with warnings if outside range.
    fCheckColNum(Data.F, setdiff(ColNames.V.s, ColNamesNonNumeric.V.s), 'sEddyProc.initialize')
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
	## sLOCATION is a list of information on site location and timezone (see \code{\link{sEddyProc_sSetLocationInfo}}).
	.self$sSetLocationInfo( Lat_deg.n , Long_deg.n , TimeZone_h.n)

    ##details<<
    ## sTEMP is a data frame used only temporally.

    #Initialize class fields
    message('New sEddyProc class for site \'', ID.s, '\'')

    callSuper(...) # Required for initialization of class fields as last call of function
    ##value<<
    ## Initialized fields of sEddyProc.
}
sEddyProc$methods( initialize = sEddyProc_initialize)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ sEddyProc class: Data handling functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
	if (!is.na(Lat_deg.n) & (Lat_deg.n < -90 | Lat_deg.n > 90)) stop("Latitude must be in interval -90 to + 90")
	if (!is.na(Long_deg.n) & (Long_deg.n < -180 | Long_deg.n > 180)) stop("Longitude must be in interval -180 to + 180")
	if (!is.na(TimeZone_h.n) & (TimeZone_h.n < -12 | TimeZone_h.n > + 12 | TimeZone_h.n != as.integer(TimeZone_h.n))) stop("Timezone must be an integer in interval -12 to 12")
	sLOCATION <<- list(
			Lat_deg.n = Lat_deg.n
			, Long_deg.n = Long_deg.n
			, TimeZone_h.n = TimeZone_h.n
	)
}
sEddyProc$methods(sSetLocationInfo = sEddyProc_sSetLocationInfo)


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


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' @export
sEddyProc_sExportResults <- function(
	  isListColumnsExported = FALSE	##<< if TRUE export list columns in addition to numeric columns,
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


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

