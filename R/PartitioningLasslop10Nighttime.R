partGLFitNightTimeTRespSens = function(
		### Estimate temperature sensitivity parameters for successive periods
		ds					##<< data.frame with numeric columns NEE, sdNEE, Temp (degC),
		  ## VPD, Rg, and logical columns isNight and isDay
		, winSizeRefInDays = 4L##<< Window size in days for daytime for
		  ## referencing windows to the same base
		, winSizeNight = 12L	##<< Window size in days for nighttime fits
		, winExtendSizes = winSizeNight * c(2L, 4L) ##<< successively increased
		  ## nighttime windows, to obtain a night-time fit
		, strideInDays = 2L	##<< step in days for shifting the windows
		, isVerbose = TRUE		##<< set to FALSE to suppress messages
		, nRecInDay = 48L		##<< number of records within one day
		  ## (for half-hourly data its 48)
		, controlGLPart = partGLControl()		##<< list of further default parameters
) {
	##seealso<< \code{\link{partGLFitNightTempSensOneWindow}}
	if (isVerbose) message(
	  "  Estimating temperature sensitivity from night time NEE "
	  , appendLF = FALSE)
	resNight <- simplifyApplyWindows(tmp <- applyWindows(ds
	        , partGLFitNightTempSensOneWindow, prevRes = data.frame(E0 = NA)
					, winSizeRefInDays = winSizeRefInDays
					, winSizeInDays = winSizeNight
					, isVerbose = isVerbose
					, nRecInDay = nRecInDay
					, controlGLPart = controlGLPart
			))
	iNoSummary <- which(is.na(resNight$E0) )
	iExtend <- 1
	##details<<
	## Window sizes are successively increased to \code{winExtendSizes} in order
	## to obtain parameter estimates where fits with smaller window size failed.
	## This behaviour of repeated attempts can be avoid by setting
	## \code{controlGLPart$isExtendTRefWindow = FALSE}.
	if (!isTRUE(controlGLPart$isExtendTRefWindow) ) { winExtendSizes <- numeric(0) }
	while (length(iNoSummary) && (iExtend <= length(winExtendSizes)) ) {
		if (isVerbose) message(
		  "    increase window size to ", winExtendSizes[iExtend], appendLF = FALSE)
		resNightExtend <- simplifyApplyWindows(applyWindows(ds
		        , partGLFitNightTempSensOneWindow
						, prevRes = data.frame(E0 = NA)
						, winSizeRefInDays = winSizeRefInDays
						, winSizeInDays = winExtendSizes[iExtend]
						, isVerbose = isVerbose
						, nRecInDay = nRecInDay
						, controlGLPart = controlGLPart
				))
		resNight[iNoSummary, ] <- resNightExtend[iNoSummary, ]
		#all(resNight$iCentralRec == resNightExtend$iCentralRec)
		iNoSummary <- which(is.na(resNight$E0) )
		iExtend <- iExtend + 1L
	}
	#
	##seealso<< \code{\link{partGLSmoothTempSens}}
	# remember E0 and sdE0 before overidden by smoothing
	nFiniteE0 <- sum(is.finite(resNight$E0))
	if ( (nFiniteE0 < 5) && (nFiniteE0 < 0.1 * nrow(resNight)) ) stop(
	  "Estimated valid temperature sensitivity for only ", nFiniteE0
	  , " windows. Stopping.")
	resNight$E0Fit <- resNight$E0
	resNight$sdE0Fit <- resNight$sdE0
	E0Smooth <- if (isTRUE(controlGLPart$smoothTempSensEstimateAcrossTime) ) {
	  if (isVerbose) message("  Smoothing temperature sensitivity estimates")
	  E0Smooth <- partGLSmoothTempSens(resNight)
	}else {
	  E0Smooth <- resNight
	  iNonFiniteE0 <- which(!is.finite(E0Smooth$E0))
	  # set uncertainty to the 90% quantile of the distribution of uncertainties
	  E0Smooth$sdE0[iNonFiniteE0] <- quantile(E0Smooth$sdE0, 0.9, na.rm = TRUE)
	  # fill NA with value from previous window
	  E0Smooth$E0 <- fillNAForward(E0Smooth$E0)
	  E0Smooth
	}
	# now all E0 and sdE0 are defined
	#
	##seealso<< \code{\link{partGLFitNightRespRefOneWindow}}
	if (isVerbose) message(
	  "  Estimating respiration at reference temperature for smoothed temperature"
	  ," sensitivity from night time NEE ", appendLF = FALSE)
	resRef15 <- simplifyApplyWindows(tmp <- applyWindows(
	  ds
	  , partGLFitNightRespRefOneWindow
	  , winSizeRefInDays = winSizeRefInDays
	  , winSizeInDays = winSizeNight
	  , nRecInDay = nRecInDay
	  #
	  , E0Win = E0Smooth
	  , controlGLPart = controlGLPart
	  , isVerbose = isVerbose
	))
	# fill those NA caused by not enough night-time records
	E0Smooth$RRef <- fillNAForward(resRef15$RRef, firstValue =
	                                 # on NA at the start of the series, take the first  finite value
	                                 E0Smooth$RRef[which(is.finite(E0Smooth$RRef))[1] ]
	)
	##value<< data.frame with columns of \code{winInfo}
	##from applyWindows, E0, sdE0, RRef
	E0Smooth
}

fillNAForward <- function(
  ### replace NA by value of previous record
  x		##<< numeric vector to fill NAs
  , firstValue = median(x, na.rm = TRUE)	##<< value to be used
  ## for NA at the beginning of x
) {
  iMissing <- which(!is.finite(x))
  if (length(iMissing) && (iMissing[1] == 1L)) {
    # set first vluae
    x[1L] <- firstValue
    iMissing <- iMissing[-1]
  }
  if (length(iMissing)) {
    for (i in iMissing) {
      # set to value from previous window
      x[i] <- x[i - 1L]
    }
  }
  return(x)
}

getStartRecsOfWindows <- function(
  ### compute the starting positions of windows for given size
  nRec					##<< numeric scalar: number of records
  , winSizeInDays = winSizeRefInDays		##<< Window size in days
  , winSizeRefInDays = 4L				##<< Window size in days for reference window
  ## (e.g. day-Window for night time)
  , strideInDays = floor(winSizeRefInDays / 2L)	##<< step in days for
  ## shifting the window, for alligning usually a factor of winSizeRef
  , nRecInDay = 48L			##<< number of records within one day
  ## (for half-hourly data its 48)
) {
  nDay <- as.integer(ceiling(nRec / nRecInDay))
  # center of the reference window still in records
  nDayLastWindow <- nDay - (winSizeRefInDays / 2)
  # specifying the day for each record assuming equidistand records
  #iDayOfRec <- ((c(1:nRec)-1L) %/% nRecInDay) + 1L
  # starting days for each reference window
  startDaysRef <- seq(1, nDayLastWindow, strideInDays)
  # assuming equidistant records
  iCentralRec <- 1L + as.integer((startDaysRef - 1L) + winSizeRefInDays / 2) *
    nRecInDay
  # precomputing the starting and end records for all periods in vectorized way
  # may become negative, negative needed for computation of iRecEnd
  iRecStart0 <- as.integer(iCentralRec -winSizeInDays / 2 * nRecInDay)
  iRecStart <- pmax(1L, iRecStart0)
}


applyWindows <- function(
  ### apply a function to several windows of a data.frame
  ds						##<< dataframe to iterate
  , FUN					##<< function to apply to subsets of the data.frame
  ## taking a subset of the data.frame as first argument
  ## the second: a one-row data.frame with window information
  ## (iWindow, dayStart, dayEnd, iRecStart, iRecEnd, iCentralRec)
  ## the third: most recent valid result of calls FUN. Valid is a
  ## non-NULL result.
  , prevRes = list()						##<< initial values for the list that
  ##is carried between windows
  , winSizeInDays = winSizeRefInDays		##<< Window size in days
  , winSizeRefInDays = 4L				##<< Window size in days for reference window
  ## (e.g. day-Window for night time)
  , strideInDays = floor(winSizeRefInDays / 2L)	##<< step in days for
  ## shifting the window, for alligning usually a factor of winSizeRef
  , isVerbose = TRUE			##<< set to FALSE to suppress messages
  , nRecInDay = 48L			##<< number of records within one day
  ## (for half-hourly data its 48)
  , ...	##<< further arguments to FUN
) {
  ##details<<
  ## Assumes equidistant rows with nRecInDay records forming one day
  ##  and reporting full days
  ## i.e. all of the nRecInDay records are in the first day.
  ##details<<
  ## In order to have a common reference winSizeRefInDays is given so that
  ## results by a different window
  ## size correspond to each window of shifting a window of winSizeRefInDays
  ## Each window is anchord so that the center equals the center of the
  ## reference window.
  ## This becomes important when selecting records at the edges of the series.
  nRec <- nrow(ds)
  nDay <- as.integer(ceiling(nRec / nRecInDay))
  # center of the reference window still in records
  nDayLastWindow <- nDay - (winSizeRefInDays / 2)
  # specifying the day for each record assuming equidistand records
  #iDayOfRec <- ((c(1:nRec)-1L) %/% nRecInDay) + 1L
  # starting days for each reference window
  startDaysRef <- seq(1, nDayLastWindow, strideInDays)
  # assuming equidistant records
  iCentralRec <- 1L + as.integer((startDaysRef-1L) + winSizeRefInDays / 2) *
    nRecInDay
  nWindow <- length(startDaysRef)
  # precomputing the starting and end records for all periods in vectorized way
  # # may become negative, negative needed for computation of dayEnd
  dayStart0 <- as.integer(startDaysRef + winSizeRefInDays / 2 - winSizeInDays / 2)
  dayStart <- pmax(1L, dayStart0)
  dayEnd <- pmin(nDay, dayStart0-1L + winSizeInDays)
  # may become negative, negative needed for computation of iRecEnd
  iRecStart0 <- as.integer(iCentralRec -winSizeInDays / 2 * nRecInDay)
  iRecStart <- pmax(1L, iRecStart0)
  iRecEnd <- pmin(nRec, as.integer(iCentralRec-1L + winSizeInDays / 2 * nRecInDay))
  # central record in each window, only last record, refers to reference window
  # of size winSizeRefInDays
  ##value<< a list with first component a data.frame with columns
  dsRec <- data.frame(
    iWindow = 1:nWindow		##<< integer: counter of the window
    , dayStart = dayStart	##<< integer: starting day of the window
    , dayEnd = dayEnd			##<< integer: ending day of the window
    , iRecStart = iRecStart	##<< integer: first record number of the window
    , iRecEnd = iRecEnd		##<< integer: last record number of the window
    , iCentralRec = iCentralRec	##<< integer: central record within the window
    ## assuming equidistant records
  )
  # each will hold a data.frame to be row-bound afterwards (dont know the cols yet)
  res2List <- vector("list", nWindow)
  for (iWindow in 1:nWindow) {
    if (isVerbose) message(", ", startDaysRef[iWindow], appendLF = FALSE)
    startRec <- iRecStart[iWindow]
    endRec <- iRecEnd[iWindow]
    dsWin <- ds[startRec:endRec, ]
    resFun <- FUN(dsWin, dsRec[iWindow, ], prevRes, ...)
    ##details<< Usually indicate an invalid result by returning NULL.
    ## If one still wants to store results but prevent updating the
    ## \code{prevRes} argument supplied to the next call
    ## then return a list item (or dataframe column) \code{isValid = TRUE}.
    if (length(resFun) ) {
      res2List[[iWindow]] <- resFun
      if (!is.list(resFun) || !length(resFun$isValid) || isTRUE(resFun$isValid) )
        prevRes <- resFun
    }
  }
  if (isVerbose) message("") # LineFeed
  #tmp <- res2List
  #res2List <- tmp
  # rbind res2List, but keep NULL results, therefore add dummy dataframe,
  # and delete dummy column afterward
  #res2List[sapply(res2List, is.null) ] <- list(data.frame(..DUMMY = 1))
  #res2$..DUMMY <- NULL
  ##value<< The second entry (\code{resFUN}) is a list (of length nrow(winInfo))
  ## with each entry a result of a call to FUN.
  list(
    winInfo = dsRec
    , resFUN = res2List
  )
}

simplifyApplyWindows <- function(
  ### simplify the result returned by applyWindows
  resApply	##<< result of \code{\link{applyWindows}}
) {
  ##details<<
  ## If FUN returns a named vector or a single-row data.frame,
  ## the resFUN result component of applyWindows can be condensed to a data.frame.
  ## This result is column-bound to the winInfo result component
  ##value<<
  ## A single data.frame with columns of winInfo and results of FUN
  if (!length(resApply$winInfo) ) return(resApply$winInfo)
  ansFUN <- if (is.data.frame(resApply$resFUN[[1]]) ) {
    bind_rows(resApply$resFUN)
  } else {
    do.call(rbind, resApply$resFUN)
  }
  cbind(resApply$winInfo, ansFUN)
}

partGLFitNightTempSensOneWindow = function(
  ### Estimate temperature sensitivity E0 within one window
  dss					##<< data.frame with numeric columns NEE, sdNEE, Temp (degC)
  ## , VPD, Rg, and logical columns isNight and isDay
  , winInfo			##<< one-row data.frame with window information, including iWindow
  , prevRes			##<< component prevRes from previous result, here with item prevE0
  , isVerbose = TRUE		##<< set to FALSE to suppress messages
  , nRecInDay = 48L	##<< number of records within one day
  ## (for half-hourly data its 48)
  , controlGLPart = partGLControl()	##<< list of further default parameters
) {
  ##author<<
  ## TW
  ##seealso<< \code{\link{partitionNEEGL}}
  ##description<<
  ## Estimation of respiration at reference temperature (RRef) and
  ## temperature sensitivity (E0) for one window.
  isValid <- isValidNightRecord(dss)
  # check that there are enough night and enough day-values for fitting,
  # else continue with next window
  if (sum(isValid) < controlGLPart$minNRecInDayWindow) return(data.frame(
    E0 = NA_real_, sdE0 = NA_real_, TRefFit = NA_real_, RRefFit = NA_real_))
  dssNight <- dss[isValid, ]
  ##seealso<< \code{\link{partGLEstimateTempSensInBoundsE0Only}}
  ##details<<
  ## The reference temperature is taken as the median of the temperatures of
  ## valid records,
  ## unless a fixed reference temperature (in degree Celsius) is specified
  ## by argument \code{controlGLPart$fixedTRefAtNightTime}.
  TRefFitCelsius <- if (length(controlGLPart$fixedTRefAtNightTime) &&
                        is.finite(controlGLPart$fixedTRefAtNightTime)
  ) {
    controlGLPart$fixedTRefAtNightTime
  } else{
    median(dssNight$Temp, na.rm = TRUE)
  }
  resNightFit <- partGLEstimateTempSensInBoundsE0Only(
    dssNight$NEE
    , fConvertCtoK(dssNight$Temp)
    , prevE0 = prevRes$E0, TRefFit = fConvertCtoK(TRefFitCelsius))
  ans <- data.frame(
    E0 = resNightFit$E0, sdE0 = resNightFit$sdE0
    , TRefFit = resNightFit$TRefFit, RRefFit = resNightFit$RRefFit)
  ans
}

isValidNightRecord <- function(
  ### compute logical vector of each rows in ds is its a valid night record
  ds		##<< data.frame with columns isNight, NEE, Temp (degC)
) {
  isValid <- !is.na(ds$isNight) & ds$isNight & !is.na(ds$NEE) & is.finite(ds$Temp)
  ##details<<
  ## For robustness, data is trimmed to conditions at temperature > 1 degC
  ## but only timmed if there are more at least 12 records left
  isFreezing <- ds$Temp[isValid] <= -1
  if (sum(!isFreezing) >= 12L) isValid[isValid][isFreezing] <- FALSE
  ##value<< a logical vector of length nrow(ds)
  return(isValid)
}

partGLEstimateTempSensInBoundsE0Only <- function(
  ### Estimate bounded temperature sensitivity E0 and RRef of ecosystem respiration
  REco					##<< numeric vector: night time NEE
  , temperatureKelvin		##<< numeric vector: temperature in Kelvin
  ## of same length as REco
  , prevE0	= NA			##<< numeric scalar: the previous guess of
  ## Temperature Sensitivity
  , TRefFit = median(temperatureKelvin, na.rm = TRUE)	##<< reference
  ## temperature for which RRef is estimated.
  ## Set it to the center of the data (the default) for best fit
  ## (lowest uncertainty)
) {
  ##author<< MM, TW
  ##seealso<< \code{\link{partGLFitLRCWindows}}
  ##details<<
  ## Basal respiration is reported for temperature of 15 degree Celsius.
  ## However during the fit
  ## a reference temperature of the median of the dataset is used.
  ## This is done to avoid
  ## strong correlations between estimated parameters E0 and RRef,
  ## that occure if reference temperature
  ## is outside the center of the data.
  resFit <- try(
    nls(formula = R_eco ~ fLloydTaylor(RRef, E0, Temp, TRef = TRefFit)
        , algorithm = 'default', trace = FALSE,
        data = as.data.frame(cbind(R_eco = REco, Temp = temperatureKelvin))
        , start = list(RRef = mean(REco, na.rm = TRUE)
                       , E0 = as.vector({if (is.finite(prevE0)) prevE0 else 100}))
        , control = nls.control(maxiter = 20L)
    )
    , silent = TRUE)
  #plot(REco ~ I(temperatureKelvin-273.15) )
  #lines(fLloydTaylor(coef(resFit)["RRef"], coef(resFit)["E0"]
  #, temperatureKelvin, TRef = TRefFit) ~ I(temperatureKelvin-273.15))
  if (inherits(resFit, "try-error")) {
    #stop("debug partGLEstimateTempSensInBounds")
    #plot(REco.V.n	~ temperatureKelvin.V.n)
    E0 <- NA
    sdE0 <- NA
    RRefFit <- NA
  } else {
    E0 <- coef(resFit)['E0']
    sdE0 <- coef(summary(resFit))['E0', 2]
    RRefFit <- coef(resFit)['RRef']
  }
  # resFit$convInfo$isConv
  ##details<<
  ## If E0 is out of bounds [50, 400] then report E0 as NA
  if (is.na(E0) || (E0 < 50) || (E0 > 400)) {
    E0 <- NA
    sdE0 <- NA
    RRefFit <- NA
  }
  ##value<< list with entries
  return(list(
    E0 = E0				      ##<< numeric scalar of estimated temperature
    ## sensitivty E0 bounded to [50, 400]
    , sdE0 = sdE0			  ##<< numeric scalar of standard deviation of E0
    , TRefFit = TRefFit	##<< numeric scalar reference temperature used
    ## in the E0 fit
    , RRefFit = RRefFit	##<< numeric scalar respiration at TRefFit
  ))
}

.tmp.f <- function() {
  # from recover at fitting E0
  temp <- temperatureKelvin - 273.15
  plot(REco ~ temp)
  lines(fLloydTaylor(RRefFit, E0, temperatureKelvin, TRef = TRefFit) ~ temp)
}

partGLSmoothTempSens <- function(
  ### Smoothes time development of E0
  E0Win				##<< data.frame with columns E0 and sdE0, RRefFit,
  ## and TRefFit with one row for each window
) {
  #return(E0Win)
  #E0Win$E0[1] <- NA
  # TODO return NA in the first place where the previous window was used
  E0Win$E0[c(FALSE, (diff(E0Win$E0) == 0))] <- NA
  # mlegp does not work for long time series (Trevors report with Havard data)
  # hence, smooth one year at a time
  E0Win$year <- ceiling(E0Win$dayStart / 365)
  yr <- E0Win$year[1]
  resWin <- lapply(unique(E0Win$year), function(yr) {
    E0WinYr <- E0Win[E0Win$year == yr, , drop = FALSE]
    isFiniteE0 <- is.finite(E0WinYr$E0)
    E0WinFinite <- E0WinYr[isFiniteE0, ]
    if (!nrow(E0WinFinite)) {
      warning(
        "No respiration-temperature relationship for any period of the ",yr,"th year. "
        , "Using mean temperature sensitivity of other years for this year.")
    } else {
      output <- capture.output(
        gpFit <- mlegp(X = E0WinFinite$iCentralRec, Z = E0WinFinite$E0
                       , nugget = E0WinFinite$sdE0^2)
        #gpFit <- mlegp(X = E0WinFinite$iCentralRec, Z = E0WinFinite$E0
        #              , nugget = (E0WinFinite$sdE0 * 2)^2, nugget.known = 1L)
      )
      pred1 <- predict(gpFit, matrix(E0WinYr$iCentralRec, ncol = 1)
                       , se.fit = TRUE)
      nuggetNewObs <- quantile(gpFit$nugget, 0.9)
      nugget <- rep(nuggetNewObs, nrow(E0WinYr))
      nugget[isFiniteE0] <- gpFit$nugget
      E0WinYr$E0 <- as.vector(pred1$fit)
      E0WinYr$sdE0 <- as.vector(pred1$se.fit) + unname(sqrt(nugget))
    }
    E0WinYr
  })
  ##value<< dataframe E0Win with updated columns E0 and sdE0
  E0WinYrs <- do.call(rbind, resWin)
  iNoFiniteE0 <- which(!is.finite(E0WinYrs$E0))
  if (length(iNoFiniteE0)) {
    if (length(iNoFiniteE0) == nrow(E0WinYrs)) stop(
      "Could not estimate respiration~temperature relationship.")
    E0WinYrs$E0[iNoFiniteE0] <- mean(E0WinYrs$E0[-iNoFiniteE0])
    E0WinYrs$sdE0[iNoFiniteE0] <- quantile(E0WinYrs$sdE0[-iNoFiniteE0], 0.9)*1.5
  }
  E0WinYrs
}
.tmp.f <- function() {
  plot(E0 ~ iCentralRec, E0Win, type = "l")
  arrows(E0WinFinite$iCentralRec, E0WinFinite$E0 - E0WinFinite$sdE0
         , y1 = E0WinFinite$E0 + E0WinFinite$sdE0, length = 0, col = "grey")
  plot(E0 ~ iCentralRec, E0WinYrs, type = "l")
  plot(sdE0 ~ iCentralRec, E0WinYrs, type = "l")
  #
  E0Win$day <- (E0Win$iCentralRec - 1) / 48 + 1
  E0WinFinite$day <- (E0WinFinite$iCentralRec - 1) / 48 + 1
  plot(E0WinFinite$E0 ~ E0WinFinite$day)
  plot(E0Win$E0Fit ~ E0Win$day)
  points(E0Win$E0 ~ E0Win$day, col = "blue", type = "b", lty = "dotted")
  #points(E0Win$E0 ~ E0Win$day, col = "blue")
  #arrows(E0Win$day, E0Win$E0Fit, y1 = E0Win$E0, col = "grey", length = 0.1)
  lines(I(E0Win$E0 + 1.06 * E0Win$sdE0) ~ E0Win$day, col = "lightblue")
  lines(I(E0Win$E0 - 1.06 * E0Win$sdE0) ~ E0Win$day, col = "lightblue")
  #
  E0Win$E0 <- E0Win$E0Fit
  E0Win$sdE0 <- E0Win$sdE0Fit
  E0Win$E0[c(FALSE, (diff(E0Win$E0) == 0))] <- NA	# TODO ret NA in the first place
  #
  E0Win$isFiniteE0 <- isFiniteE0
  E0Win[E0Win$E0 < 80, ]
  E0Win[65:75, ]
  subset(E0WinFinite, iWindow %in% 65:75)

  E0Win$sdE0 / E0Win$E0
}

partGLFitNightRespRefOneWindow = function(
  ### Estimate Reference temperature from nicht-time and given tempsens E0
  dss					  ##<< data.frame with numeric columns NEE, isNight, Temp, Rg
  , winInfo			##<< one-row data.frame with window information, including iWindow
  , prevRes = list()		##<< component prevRes from previous result, here not used.
  , E0Win				##<< data.frame with columns E0 and sdE0, RRefFit, and
  ## TRefFit with one row for each window
  , controlGLPart = partGLControl()	##<< list of further default parameters
  , TRef = 15			##<< numeric scalar of Temperature (degree Celsius) for
  ## reference respiration RRef
) {
  ##author<< TW
  ##seealso<< \code{\link{partitionNEEGL}}
  ##description<<
  ## Estimation of respiration at reference temperature (RRef) and
  ## temperature sensitivity (E0) for one window.
  isValid <- isValidNightRecord(dss)
  # check that there are enough night and enough day-values for fitting,
  # else continue with next window
  ##details<<
  ## If there are too few night-time records
  ## (< controlGLPart$minNRecInDayWindow) then return NA
  if (sum(isValid) < controlGLPart$minNRecInDayWindow) return(c(RRef = NA_real_))
  dssNight <- dss[isValid, ]
  ##seealso<< \code{\link{partGLEstimateTempSensInBoundsE0Only}}
  REco <- dssNight$NEE
  E0 <- E0Win$E0[winInfo$iWindow]
  TRefInKelvin <- 273.15 + TRef	# 15degC in Kelvin
  RRef <- if (length(REco) >= 3L) {
    temperatureKelvin <- 273.15 + dssNight$Temp
    T_0.n = 227.13         ##<< Regression temperature as
    ## fitted by LloydTaylor (1994) in Kelvin (degK)
    TFacLloydTaylor <-  exp(E0 * (1 / (TRefInKelvin - T_0.n) -
                                    1 / (temperatureKelvin - T_0.n) ) )
    lm15 <- lm(REco ~ TFacLloydTaylor -1)
    coef(lm15)
  } else
    fLloydTaylor(E0Win$RRefFit[winInfo$iWindow], E0, TRefInKelvin
                 , TRef = E0Win$TRefFit[winInfo$iWindow])
  RRefBounded <- max(0, RRef)
  ##value<< named numeric vector with single entry RRef
  c(RRef = RRefBounded)
}

