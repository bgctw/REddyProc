#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with functions to convert and check data +++
#+++ Dependencies: <none>
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Time format conversion to POSIX
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fConvertTimeToPosix <- function(
  ### Convert different time formats to POSIX
  Data.F                   ##<< Data frame with time columns to be converted
  , TFormat.s              ##<< Abbreviation for implemented time formats
  , Year.s = 'none'        ##<< Column name of year
  , Month.s = 'none'       ##<< Column name of month
  , Day.s = 'none'         ##<< Column name of day
  , Hour.s = 'none'        ##<< Column name of hour
  , Min.s = 'none'         ##<< Column name of min
  , TName.s = 'DateTime'   ##<< Column name of new column
  , tz = 'GMT'				     ##<< timezone used to store the data. Advised to keep
    ## GMT to avoid daytime shifting issues
  )
  ##author<<
  ## AMM
  # TEST: see unit tests in test_fConvertTimeToPosix
  # TEST: Year.s = 'none'; Month.s = 'none'; Day.s = 'none'; Hour.s = 'none'; Min.s = 'none'
  # TEST: Data.F <- Date.F.x; TFormat.s <- 'YDH'; Year.s = 'FluxnetYear.n'; Day.s = 'FluxnetDoY.n'; Hour.s = 'FluxnetHourDec.n'
  #!Attention with MDS pwwave output file: Do not use YDH since julday (day of year) is 366 but year is already the next year, use YMDHM instead!
{
  ##details<<
  ## The different time formats are converted to POSIX (GMT) and a 'TimeDate'
  ## column is prefixed to the data frame
  #Check if specified columns exist and are in data frame, with 'none' as dummy
  NoneCols.b <- c(Year.s, Month.s, Day.s, Hour.s, Min.s) %in% 'none'
  fCheckColNames(Data.F, c(Year.s, Month.s, Day.s, Hour.s, Min.s)[!NoneCols.b]
                 , 'fConvertTimeToPosix')
  fCheckColNum(Data.F, c(Year.s, Month.s, Day.s, Hour.s, Min.s)[!NoneCols.b]
               , 'fConvertTimeToPosix')
  ##details<<
  ## Implemented time formats:
  if (TFormat.s == 'YDH') {
    if (any(c(Year.s, Day.s, Hour.s) == 'none') )
      stop('With time format \'YDH\' year, day of year (DoY), and hour need to be specified!')
    ## YDH - year, day of year, hour in decimal (e.g. 1998, 1, 10.5)
    fCheckOutsideRange(Data.F, Year.s, c('<', 1000, '|', '>', 3000), 'fConvertTimeToPosix')
    ## The day format is day of year (DoY, 1-365 or 1-366 in leap years).
    fCheckOutsideRange(Data.F, Day.s, c('<', 1, '|', '>', 366), 'fConvertTimeToPosix')
    ## The hour format is decimal time (0.0-23.5).
    fCheckOutsideRange(Data.F, Hour.s, c('<', 0.0, '|', '>', 24.0), 'fConvertTimeToPosix')
    ## 366d-correction and 24h-correction to the first day in next year,
    ## see unit test in test_fConvertTimeToPosix.R for details.
    lYear.V.n <- Data.F[, Year.s]
    lDoY.V.n <- Data.F[, Day.s]
    lHour.V.n <- Data.F[, Hour.s] %/% 1
    lMin.V.n <- 60 * Data.F[, Hour.s] %% 1
    #Check time format
    #Important to set time zone to GMT to avoid problems with daylight savings timeshifts
    lTime.V.p <- strptime(paste(lYear.V.n, lDoY.V.n, lHour.V.n, lMin.V.n, sep = '-'), format = '%Y-%j-%H-%M', tz = tz)
    #24h-correction: strptime will be NA for hour 24.0 (needs to be before 366d-correction since DoY changes!)
    Hour24.b <- is.na(lTime.V.p) & (lHour.V.n == 24.0)
    lDoY.V.n[Hour24.b] <- 1 + lDoY.V.n[Hour24.b] #succeding day
    lHour.V.n[Hour24.b] <- 0.0
    #Recheck time format
    lTime.V.p <- strptime(paste(lYear.V.n, lDoY.V.n, lHour.V.n, lMin.V.n, sep = '-'), format = '%Y-%j-%H-%M', tz = tz)
    #366d-correction: strptime will be NA for day 366 (or 367 in leap years)
    DoY366.b <- is.na(lTime.V.p) & (lDoY.V.n == 366 | lDoY.V.n == 367) & (lHour.V.n == 0.0)
    lYear.V.n[DoY366.b] <- 1 + lYear.V.n[DoY366.b] #succeding year
    lDoY.V.n[DoY366.b] <- 1 #first day
    #Set time format
    lTime.V.p <- strptime(paste(lYear.V.n, lDoY.V.n, lHour.V.n, lMin.V.n, sep = '-'), format = '%Y-%j-%H-%M', tz = tz)
    if (sum(is.na(lTime.V.p)) > 0)
      stop(
        sum(is.na(lTime.V.p)), ' errors in convert YDH to timestamp in rows: '
        , which(is.na(lTime.V.p)))
  } else if (TFormat.s == 'YMDH') {
    if (any(c(Year.s, Month.s, Day.s, Hour.s) == 'none') )
      stop('With time format \'YMDH\' year, month, day, and hour need to be specified!')
    ## YMDH - year, month, day of month, hour in decimal (e.g. 1998, 1, 1, 10.5)
    fCheckOutsideRange(Data.F, Year.s, c('<', 1000, '|', '>', 3000), 'fConvertTimeToPosix')
    ## The month format is (1-12)
    fCheckOutsideRange(Data.F, Month.s, c('<', 1, '|', '>', 12), 'fConvertTimeToPosix')
    ## The day format is day of month (1-31).
    fCheckOutsideRange(Data.F, Day.s, c('<', 1, '|', '>', 31), 'fConvertTimeToPosix')
    ## The hour format is decimal time (0.0-23.5)
    fCheckOutsideRange(Data.F, Hour.s, c('<', 0.0, '|', ' >= ', 24.0), 'fConvertTimeToPosix (For format \'YMDH\' no 24h correction in old R versions (2.13 and below))')
    ## No extra corrections.
    lYear.V.n <- Data.F[, Year.s]
    lMonth.V.n <- Data.F[, Month.s]
    lDay.V.n <- Data.F[, Day.s]
    lHour.V.n <- Data.F[, Hour.s] %/% 1
    lMin.V.n <- 60 * Data.F[, Hour.s] %% 1
    #Set time format, important to set time zone to GMT to avoid problems with daylight savings timeshifts
    lTime.V.p <- strptime(paste(lYear.V.n, lMonth.V.n, lDay.V.n, lHour.V.n, lMin.V.n, sep = '-'), format = '%Y-%m-%d-%H-%M', tz = tz)
    if (sum(is.na(lTime.V.p)) > 0)
      stop(sum(is.na(lTime.V.p)), ' errors in convert YDH to timestamp in rows: ', which(is.na(lTime.V.p)))

  } else if (TFormat.s == 'YMDHM') {
    if (any(c(Year.s, Month.s, Day.s, Hour.s, Min.s) == 'none') )
      stop('With time format \'YMDHM\' year, month, day, hour and min need to be specified!')
    ## YMDHM - year, month, day of month, integer hour, minute (e.g. 1998, 1, 1, 10, 30)
    fCheckOutsideRange(Data.F, Year.s, c('<', 1000, '|', '>', 3000), 'fConvertTimeToPosix')
    ## The month format is (1-12)
    fCheckOutsideRange(Data.F, Month.s, c('<', 1, '|', '>', 12), 'fConvertTimeToPosix')
    ## The day format is day of month (1-31).
    fCheckOutsideRange(Data.F, Day.s, c('<', 1, '|', '>', 31), 'fConvertTimeToPosix')
    ## The hour format is (0-23)
    fCheckOutsideRange(Data.F, Hour.s, c('<', 0, '|', '>', 23), 'fConvertTimeToPosix(YMDH no 24h correction)')
    ## The minute format is (0-59)
    fCheckOutsideRange(Data.F, Min.s, c('<', 0, '|', '>', 59), 'fConvertTimeToPosix(YMDH no 24h correction)')
    ## No extra corrections.
    lYear.V.n <- Data.F[, Year.s]
    lMonth.V.n <- Data.F[, Month.s]
    lDay.V.n <- Data.F[, Day.s]
    lHour.V.n <- Data.F[, Hour.s]
    lMin.V.n <- Data.F[, Min.s]
    #Set time format, important to set time zone to GMT to avoid problems with daylight savings timeshifts
    lTime.V.p <- strptime(paste(lYear.V.n, lMonth.V.n, lDay.V.n, lHour.V.n, lMin.V.n, sep = '-'), format = '%Y-%m-%d-%H-%M', tz = tz)
    if (sum(is.na(lTime.V.p)) > 0)
       stop(sum(is.na(lTime.V.p)), ' errors in convert YDH to timestamp in rows: ', which(is.na(lTime.V.p)))

  } else {
    stop('Unknown time format ', TFormat.s, '!')
  }
  #POSIXlt converted to POSIXct in data.frame... !
  Data.F <- cbind(lTime.V.p, Data.F)
  names(Data.F)[1] <- TName.s
  attr(Data.F[, TName.s], 'units') <- 'POSIXDate Time'
  attr(Data.F[, TName.s], 'varnames') <- TName.s
  message('Converted time format \'', TFormat.s, '\' to POSIX with column name \'', TName.s, '\'.')

  Data.F
  ##value<<
  ## Data frame with prefixed POSIX time column.
}
attr(fConvertTimeToPosix, 'ex') <- function() {
  # See unit test in test_fConvertTimeToPosix for example
}

#' @export
getTZone <- function(
	### extracts the timezone attribute from POSIXct with default on missing
	x					          ##<< POSIXct vector
	, default = "GMT"		##<< time zone returned,
	  ## if x has not timezone associated or attribute is the zero string
) {
	tzone <- attr(x, "tzone")
	if (length(tzone) && nzchar(tzone)) tzone else default
}
attr(getTZone, "ex") <- function() {
	getTZone(as.POSIXct("2010-07-01 16:00:00", tz = "etc/GMT-1") )
	getTZone(as.POSIXct("2010-07-01 16:00:00") )
	# printed with local time zone, but actually has no tz attribute
	getTZone(Sys.time())
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fCheckHHTimeSeries <- function(
  ##description<<
  ## Check half-hourly time series data
  Time.V.p              ##<< Time vector in POSIX format
  , DTS.n                ##<< Number of daily time steps (24 or 48)
  , CallFunction.s = ''    ##<< Name of function called from
  )
  ##author<<
  ## AMM
  # TEST: Time.V.p <- Data.F[, 'DateTime']
{
  # Check time series properties
  ##details<<
  ## The number of steps per day can be 24 (hourly) or 48 (half-hourly).
  if (DTS.n != 24 && DTS.n != 48)
    stop(CallFunction.s, ':::fCheckHHTimeSeries::: Daily time step need to be hourly (24) or half-hourly (48). The following value is not valid: ', DTS.n, '!')
  ##details<<
  ## The time stamp needs to be provided in POSIX time format,
  if (!inherits(Time.V.p, 'POSIXt') )
    stop(CallFunction.s, ':::fCheckHHTimeSeries::: Provided time stamp data not in POSIX time format!')
  ##details<<
  ## equidistant half-hours,
  NotDistHH.b <- as.numeric(Time.V.p[2:length(Time.V.p)])-as.numeric(Time.V.p[1:(length(Time.V.p)-1)]) != (24 / DTS.n * 60 * 60)
  NotDistHH.i <- sum(NotDistHH.b)
  if (NotDistHH.i > 0)
    stop(CallFunction.s, ':::fCheckHHTimeSeries::: Time stamp is not equidistant (half-)hours in rows: ', paste(which(NotDistHH.b), collapse = ", "))
  ##details<<
  ## and stamped on the half hour.
  NotOnHH.i <- sum(as.numeric(Time.V.p) %% (24 / DTS.n * 60 * 60) != 0)
  if (NotOnHH.i > 0)
    stop(CallFunction.s, ':::fCheckHHTimeSeries::: Time step is not stamped at half-hours in rows: ', which(as.numeric(Time.V.p) %% (24 / DTS.n * 60 * 60) != 0))
  ##details<<
  ## The sEddyProc procedures require at least three months of data.
  if (length(Time.V.p) < (3 * 30 * DTS.n) )
    stop(CallFunction.s, ':::fCheckHHTimeSeries::: Time series is shorter than 90 days (three months) of data: ', 3 * 30 - length(Time.V.p) / DTS.n, ' days missing!')
  ##details<<
  ## Full days of data are preferred: the total amount of data rows should be a multiple of the daily time step, and
  Residual.i <- length(Time.V.p) %% DTS.n
  if (Residual.i != 0)
    warning(CallFunction.s, ':::fCheckHHTimeSeries::: Data not provided in full days (multiple of daily time step). One day only has ', Residual.i , ' (half-)hours!')
  ##details<<
  ## in accordance with FLUXNET standards, the dataset is spanning from the end of the first (half-)hour (0:30 or 1:00, respectively) and to midnight (0:00).
  if (DTS.n == 48 && !(as.POSIXlt(Time.V.p[1])$hour == 0 && as.POSIXlt(Time.V.p)$min == 30) )
    warning(CallFunction.s, ':::fCheckHHTimeSeries::: Time stamp of first data row is not at the end of the first half-hour: ', format(Time.V.p[1], '%H:%M'), ' instead of 00:30!')
  if (DTS.n == 24 && !(as.POSIXlt(Time.V.p[1])$hour == 1 && as.POSIXlt(Time.V.p)$min == 00) )
    warning(CallFunction.s, ':::fCheckHHTimeSeries::: Time stamp of first data row is not at the end of the first hour: ', format(Time.V.p[1], '%H:%M'), ' instead of 01:00!')
  if (!(as.POSIXlt(Time.V.p[length(Time.V.p)])$hour == 0 && as.POSIXlt(Time.V.p[length(Time.V.p)])$min == 0) )
    warning(CallFunction.s, ':::fCheckHHTimeSeries::: The last time stamp is not midnight: 0:00!')

  ##value<<
  ## Function stops on errors.
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
fFullYearTimeSteps <- function(
  ##description<<
  ## Generate vector with (half-)hourly time steps of full year, stamped in the middle of time unit
  Year.i                ##<< Data frame to be converted
  , DTS.n                ##<< Daily time steps
  , CallFunction.s = ''    ##<< Name of function called from
  #TEST: Year.i <- 2008; DTS.n <- 48
  , tz = 'GMT'				##<< timezone used to store the data. Advised to keep
  ## GMT to avoid daytime shifting issues
)
  ##author<<
  ## AMM
{
  if (DTS.n == 48) {
    TimeStart.n <- strptime(paste(Year.i, 1, 1, 0, 15, sep = '-'), format = '%Y-%m-%d-%H-%M', tz = tz)
    TimeEnd.n <- strptime(paste(Year.i, 12, 31, 23, 45, sep = '-'), format = '%Y-%m-%d-%H-%M', tz = tz)
  } else if (DTS.n == 24) {
    TimeStart.n <- strptime(paste(Year.i, 1, 1, 0, 30, sep = '-'), format = '%Y-%m-%d-%H-%M', tz = tz)
    TimeEnd.n <- strptime(paste(Year.i, 12, 31, 23, 30, sep = '-'), format = '%Y-%m-%d-%H-%M', tz = tz)
  } else {
    stop(CallFunction.s, ':::fFullYearTimeSteps::: Only implemented for 24 or 48 daily time steps, not ', DTS.n , '!')
  }
  #DateTime vector with (half-)hourly time stamps
  #Time.F.p <- data.frame(sDateTime = seq(TimeStart.n, TimeEnd.n, (24 / DTS.n * 60 * 60)))
  FullTime.V.p <- seq(TimeStart.n, TimeEnd.n, (24 / DTS.n * 60 * 60))

  FullTime.V.p
  ##value<<
  ## Vector with time steps of full year in POSIX format
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fExpandToFullYear <- function(
  ##description<<
  ## Generate vector with (half-)hourly time steps of full year, stamped in the middle of time unit
  Time.V.p                    ##<< Time stamp in POSIX time format
  , Data.V.n                   ##<< Data vector to be expanded
  , Year.i                     ##<< Year (e.g. to plot)
  , DTS.n                      ##<< Daily time steps
  , CallFunction.s = ''          ##<< Name of function called from
  #TEST: Time.V.p <- sDATA$sDateTime; DTS.n <- 48
  , tz = getTZone(Time.V.p)	  ##<< timezone used, advised to keep default
)
  ##author<<
  ## AMM
{
  # Check if year within time span of data set
  if (sum(Year.i == as.numeric(format(Time.V.p, '%Y'))) == 0)
    stop(CallFunction.s, ':::fExpandToFullYear::: Year ', Year.i, ' not within time span of this dataset!')

  ##details<<
  ## Function to expand vectors to full year, e.g. to plot in correct time format
  SubCallFunc.s <- paste(CallFunction.s, 'fExpandToFullYear', sep = ':::')
  FullYear.V.p <- fFullYearTimeSteps(Year.i, DTS.n, SubCallFunc.s, tz = tz)
  TimeYear.V.p <- Time.V.p[(Year.i == as.numeric(format(Time.V.p, '%Y')))]
  DataYear.V.n <- Data.V.n[(Year.i == as.numeric(format(Time.V.p, '%Y')))]

  if (sum(!is.na(DataYear.V.n)) == 0)
  {
    ExpData.F.n <- data.frame(cbind(DateTime = FullYear.V.p, Data = rep(NA_real_, length(FullYear.V.p))))
    warning(CallFunction.s, ':::fExpandToFullYear::: Variable \'', attr(Data.V.n, 'varnames'), '\' contains no data for year ', Year.i, '!')
  } else if (length(TimeYear.V.p != length(FullYear.V.p))) {
    ExpData.F.n <- merge(cbind(DateTime = FullYear.V.p), cbind(DateTime = TimeYear.V.p, Data = DataYear.V.n), by = 'DateTime', all = T, sort = T)
  } else {
    ExpData.F.n <- data.frame(cbind(DataTime = TimeYear.V.p, Data = Data.V.n))
  }
  ExpData.F.n$DateTime <- .POSIXct(ExpData.F.n$DateTime, tz = tz)
  attr(ExpData.F.n$Data, 'varnames') <- attr(Data.V.n, 'varnames')
  attr(ExpData.F.n$Data, 'units') <- attr(Data.V.n, 'units')

  ExpData.F.n
  ##value<<
  ## Expanded time and data vector as data frame
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fGetBeginOfEddyPeriod <- function(
		##description<<
		## Get the begin time of a half-hour period, that is denoted by its end time.
		Time.V.p                    ##<< Time stamp in POSIXct time format
		, DTS.n = 48L           ##<< Daily time steps
#TEST: fGetBeginOfEddyPeriod(as.POSIXct(strptime(c("2015-01-01 00:00:00", "2015-01-01 00:30:00"), format = "%Y-%m-%d %H:%M:%S")) )
)
##author<<
## TW
{
	##details<<
	## The timestamp of an Eddy record denotes the end of a half-hour period.
	## This function gets the time, half an hour before
	#
	# substract halv hour, i.e. 1800seconds, to get start of period
	Time.V.p-as.integer(24L / DTS.n * 60L * 60L)
	##value<<
	## integer vector of length(Time.V.p): of times shifted by half an hour.
}

fGetEndOfEddyPeriod <- function(
		##description<<
		## Get the end time of a half-hour period, that is denoted by its beginning time
		Time.V.p                    ##<< Time stamp in POSIXct time format
		, DTS.n = 48L           ##<< Daily time steps
#TEST: fGetEndOfEddyPeriod(as.POSIXct(strptime(c("2014-12-31 23:00:00", "2014-12-31 23:30:00"), format = "%Y-%m-%d %H:%M:%S")) )
)
##author<<
## TW
{
	# add halv hour, i.e. period in seconds, to get start of period
	Time.V.p + as.integer(24L / DTS.n * 60L * 60L)
	##value<<
	## integer vector of length(Time.V.p): of times shifted by half an hour.
}

fGetYearOfEddyPeriod <- function(
		##description<<
		## get the Year from a POSIXct, with new Year (1.1. 00:00) belongs to the old year.
		Time.V.p                    ##<< Time stamp in POSIXct time format
		, DTS.n = 48L           ##<< Daily time steps
#TEST: fGetYearOfEddyPeriod(as.POSIXct(strptime(c("2015-01-01 00:00:00", "2015-01-01 00:30:00"), format = "%Y-%m-%d %H:%M:%S")) )
)
##author<<
## TW
{
	##details<<
	## The timestamp of an Eddy record denotes the end of a half-hour period.
	## If the end is NewYear, e.g. 1.1.2015 00:00) the half hour period is still in the old year.
	#
	year <- 1900L + as.POSIXlt(fGetBeginOfEddyPeriod(Time.V.p, DTS.n))$year
	year
	##value<<
	## integer vector of length(Time.V.p): of calendar years
}



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Gap / NA conversion
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fConvertGapsToNA <- function(
  ##description<<
  ## Converts all gap flags to NA
  Data.F                ##<< Data frame to be converted
  , GapFlag.n =-9999.0    ##<< Flag value used for gaps, defaults to -9999.0
  #TEST: Data.F <- ResultData.D
  )
  ##author<<
  ## AMM
{
  Gaps.i <- sum(Data.F == GapFlag.n, na.rm = T)
  Data.F[Data.F == GapFlag.n] <- NA_real_
  message('Number of \'', GapFlag.n, '\' convertered to NA: ', Gaps.i)

  Data.F
  ##value<<
  ## Data frame with NAs instead of gaps.
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fConvertNAsToGap <- function(
  ##description<<
  ## Converts all NAs to gap flag
  Data.F ##<< Data frame to be converted
  , GapFlag.n =-9999.0 ##<< Flag value used for gaps, defaults to -9999.0
  #TEST: Data.F <- ResultData.D
  )
  ##author<<
  ## AMM
{
  Gaps.i <- sum(is.na(Data.F))
  Data.F[is.na(Data.F)] <- GapFlag.n
  message('Number of NA convertered to \'', GapFlag.n, '\': ', Gaps.i)

  Data.F
  ##value<<
  ## Data frame with gap value instead of NAs.
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCalcLengthOfGaps <- function(
  ##description<<
  ## Calculate length of gaps (with flag NA) in specified vector
  Data.V.n              ##<< Numeric vector with gaps (missing values, NAs)
  )
  ##author<<
  ## AMM
  # TEST: Data.V.n <- sDATA$NEE
{
  Data.V.b <- is.na(Data.V.n)
  #Determine cumulative sums of gaps
  CumSum.V.n <- cumsum(Data.V.b)
  #Calculate sum from start of gap
  LenGaps.V.n <- CumSum.V.n-cummax((!Data.V.b) * CumSum.V.n)

  LenGaps.V.n
  ##value<<
  ## An integer vector with length of gap since start of gap.
}

fInterpolateGaps <- function(
  ##description<<
  ## Interpolate linearly between gaps, with constant values at beginning / end
  Data.V.n              ##<< Numeric vector with gaps (missing values, NAs)
)
  ##author<<
  ## AMM
  # TEST: Data.V.n <- sDATA$NEE
{
  # Fill in both ends to have constant interpolation with first / last value
  Data.V.n[1] <- Data.V.n[which(!is.na(Data.V.n))[1]]
  Data.V.n[length(Data.V.n)] <- Data.V.n[rev(which(!is.na(Data.V.n)))[1]]
  # Linear interpolation between all points
  Filled.V.n <- approx(seq_along(Data.V.n)[!is.na(Data.V.n)], Data.V.n[!is.na(Data.V.n)], xout = seq_along(Data.V.n))$y
  Filled.V.n
  ##value<<
  ## Numeric with NAs linearly interpolated.
}
.tmp.f <- function(){
  # Nice plot to see interpolation
  plot(seq_along(Data.V.n), Data.V.n)
  points(Filled.V.n, col = 2, pch = " * ")
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fSetQF <- function(
  ##title<<
  ## Check and set quality flag
  ##description<<
  ## Generate new vector from data and quality flag column.
  Data.F                ##<< Data frame
  , Var.s                ##<< Variable to be filtered
  , QFVar.s              ##<< Quality flag of variable to be filtered
  , QFValue.n            ##<< Numeric value of quality flag for _good_ data, other data is set to missing
  , CallFunction.s = ''    ##<< Name of function called from
)
  ##author<<
  ## AMM
  # TEST: Data.F <- EddyData.F; Var.s <- 'NEE'; QFVar.s <- 'QF'; QFValue.n <- 0; CallFunction.s = 'dummy'
{
  # Check if specified columns exist and are numeric
  SubCallFunc.s <- paste(CallFunction.s, 'fSetQF', sep = ':::')
  fCheckColNames(Data.F, c(Var.s, QFVar.s), SubCallFunc.s)
  fCheckColNum(Data.F, c(Var.s, QFVar.s), SubCallFunc.s)

  ##details<<
  ## Quality flag will be applied to variable - unless quality flag variables is called 'none' (dummy).
  if (QFVar.s != 'none') {
    # Check quality flag value
    if (fCheckValNum(QFValue.n) != T)
      stop(CallFunction.s, ':::fSetQF::: Quality flag \'', QFVar.s, '\' has a non-numeric value: ', QFValue.n, '!')
    # Only use data values when good data (quality flag is equal to flag value)
    Var.V.n <- ifelse(Data.F[, QFVar.s] == QFValue.n, Data.F[, Var.s], NA_real_)
    if (sum(!is.na(Var.V.n)) == 0)
      warning(CallFunction.s, ':::fSetQF::: Variable \'', Var.s, '\' contains no data after applying quality flag \'', QFVar.s, '\' with value ', QFValue.n, '!')
  } else {
    # Use all data
    Var.V.n <- Data.F[, Var.s]
    if (sum(!is.na(Var.V.n)) == 0)
      warning(CallFunction.s, ':::fSetQF::: Variable \'', Var.s, '\' contains no data!')
  }
  # Add units
  attr(Var.V.n, 'units') <- attr(Data.F[[Var.s]], 'units')
  attr(Var.V.n, 'varnames') <- if (QFVar.s == 'none') { paste(Var.s, sep = '')
  } else { paste(Var.s, '.', QFVar.s, '_', round(QFValue.n, digits = 3), sep = '') }

  Var.V.n
  ##value<<
  ## Numeric vector with _good_ data.
}

fFilterAttr <- function(
  ### filter, i.e. subset rows, a data.frame with keeping attributes (e.g. units) of the columns
  x				##<< data frame to filter
  , isFiltered.v	##<< boolean vector specifying which rows to keep
) {
  if (!is.data.frame(x)) stop("fFilterAttr error: expected first argument to be a data.frame, but was ", class(x))
  ans <- x[isFiltered.v, ]
  for (i in 1:ncol(x) ) {
    attributes(ans[[i]]) <- attributes(x[[i]])
  }
  ##value<< \code{x[, isFiltered.v]} with column attributes preserved
  ans
}

