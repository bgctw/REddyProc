#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc methods for ustar filtering +++
#+++ Ustar filtering adapted after the idea in Papale, D. et al. (2006) +++
#+++ Dependencies: Eddy.R
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @include aEddy.R
#if (!exists("sEddyProc")) source("R / aEddy.R")

#' @export
sEddyProc_sEstUstarThold <- function(
  ##title<<
  ## sEddyProc$sEstUstarThreshold - Estimating ustar threshold
  ##description<<
  ## Calling \code{\link{usEstUstarThreshold}} for class data and storing results
  UstarColName = "Ustar"		##<< column name for UStar
  , NEEColName = "NEE"			##<< column name for NEE
  , TempColName = "Tair"		##<< column name for air temperature
  , RgColName = "Rg"			  ##<< column name for solar radiation for
  ## omitting night time data
  , ...						          ##<< further arguments to
  ## \code{\link{usEstUstarThreshold}}
  , seasonFactor = usCreateSeasonFactorMonth(sDATA$sDateTime) ##<<
  ## factor of seasons to split
) {
  ##author<< TW
  if (length(seasonFactor) )
    .self$sSetUStarSeasons(seasonFactor)
  if (is.null(.self$sTEMP$season)) stop(
    "uStar seasons need to be set by argument 'seasonFactor' or before",
    " calling sEddyProc_sEstUstarThold by method sEddyProc_sSetUStarSeasons")
  reqCols <- c("sDateTime", UstarColName, NEEColName, TempColName, RgColName)
  iMissing <- which(!(reqCols %in% names(.self$sDATA)))
  if (length(iMissing)) stop(
    "Missing columns in dataset: ",paste(reqCols[iMissing], collapse = ","))
  ds <- .self$sDATA[,reqCols, drop = FALSE]
  colnames(ds) <- c("sDateTime", "Ustar", "NEE", "Tair", "Rg")
  resEst <- usEstUstarThreshold(ds, seasonFactor = .self$sTEMP$season, ...)
  sUSTAR_DETAILS <<- resEst
    #resEst[c("uStarTh", "seasonYear", "season", "tempInSeason")]
  ##value<< result component \code{uStarTh} of \code{\link{usEstUstarThreshold}}.
  ## In addition the result is stored in class variable \code{sUSTAR_DETAILS}.
  resEst$uStar
}
sEddyProc$methods(sEstUstarThold = sEddyProc_sEstUstarThold)

#' @export
sEddyProc_sSetUStarSeasons <- function(
  ### Defining seasons for the uStar threshold estimation
  seasonFactor = usCreateSeasonFactorMonth(sDATA$sDateTime)
  ### factor for subsetting times with different uStar threshold (see details)
) {
  ##author<< TW
  sTEMP$season <<- seasonFactor
  ##value<< class with updated \code{seasonFactor}
  invisible(.self)
}
sEddyProc$methods(sSetUStarSeasons = sEddyProc_sSetUStarSeasons)

#' @export
sEddyProc_sEstUstarThreshold <- function(
  ##title<<
  ## sEddyProc$sEstUstarThreshold - Estimating ustar threshold
  ##description<<
  ## Calling \code{\link{usEstUstarThreshold}} for class data and storing results
  UstarColName = "Ustar"		##<< column name for UStar
  , NEEColName = "NEE"			##<< column name for NEE
  , TempColName = "Tair"		##<< column name for air temperature
  , RgColName = "Rg"			  ##<< column name for solar radiation for
  ## omitting night time data
  , ...						          ##<< further arguments to
  ## \code{\link{usEstUstarThreshold}}
  , isWarnDeprecated = TRUE			##<< set to FALSE to avoid deprecated warning.
) {
  ##author<< TW
  if (isWarnDeprecated) warning(
    "sEddyProc_sEstUstarThreshold has been deprecated and will be removed"
    , " in future. Instead, use function"
    , " sEddyProc_sEstUstarThold, which returns only component 'uStarTh' of"
    , " the current result. The other components are still available"
    , " with class variable sUSTAR_DETAILS.")
  reqCols <- c("sDateTime", UstarColName, NEEColName, TempColName, RgColName)
  iMissing <- which(!(reqCols %in% names(.self$sDATA)))
  if (length(iMissing)) stop(
    "Missing columns in dataset: ",paste(reqCols[iMissing], collapse = ","))
  ds <- .self$sDATA[,reqCols, drop = FALSE]
  colnames(ds) <- c("sDateTime", "Ustar", "NEE", "Tair", "Rg")
  resEst <- usEstUstarThreshold(ds, ...)
  sUSTAR_DETAILS <<-
    resEst[c("uStarTh", "seasonYear", "season", "tempInSeason")]
  # sDATA$season <<-  resEst$bins$season
  # sDATA$tempBin <<-  resEst$bins$tempBin
  # sDATA$uStarBin <<-  resEst$bins$uStarBin
  sTEMP$season <<-  resEst$bins$season
  ##value<< result of \code{\link{usEstUstarThreshold}}. In addition the
  ## result is stored in class variable sUSTAR_DETAILS and the bins as
  ## additional columns to sTemp
  resEst
}
sEddyProc$methods(sEstUstarThreshold = sEddyProc_sEstUstarThreshold)

#' @export
usEstUstarThreshold = function(
		##title<<
		## usEstUstarThreshold - Estimating ustar threshold
		##description<<
		## Estimate the Ustar threshold by aggregating the estimates for
		## seasonal and temperature subsets.
		ds 	##<< data.frame with columns "sDateTime", "Ustar", "NEE", "Tair", and "Rg"
		, seasonFactor = usCreateSeasonFactorMonth(ds$sDateTime)
		    ### factor for subsetting times (see details)
		, yearOfSeasonFactor = usGetYearOfSeason(seasonFactor, ds$sDateTime)
		    ### named integer vector: for each seasonFactor level, get the year
		    ### (aggregation period) that this season belongs to
		, ctrlUstarEst = usControlUstarEst()
		    ### control parameters for estimating uStar on a single binned series,
		    ### see \code{\link{usControlUstarEst}}
		, ctrlUstarSub = usControlUstarSubsetting()	##<< control parameters for
		    ## subsetting time series (number of temperature and Ustar classes \ldots),
		    ## see \code{\link{usControlUstarSubsetting}}
		, fEstimateUStarBinned = usEstUstarThresholdSingleFw2Binned	##<< function to
		    ## estimate UStar on a single binned series,
		    ## see \code{\link{usEstUstarThresholdSingleFw2Binned}}
		, isCleaned = FALSE			##<< set to TRUE, if the data was cleaned already,
		    ## to avoid expensive call to \code{usGetValidUstarIndices}.
) {
	##author<<
	## TW, OM
	##references<<
	## Ustar filtering following the idea in Papale, D. et al. (2006)
	## Towards a standardized processing of net ecosystem exchange measured with
	## eddy covariance technique: algorithms and uncertainty estimation.
	## Biogeosciences 3(4): 571-583.

	##details<<
	## The threshold for sufficiently turbulent conditions u * (Ustar)
	## is estimated for different subsets of the time series.
	## From the estimates for each season (each value in \code{seasonFactor})
	## the maximum of all seasons of one year is reported as estimate for this year.
	## Within each season the time series is split by temperature classes.
	## Among these Ustar estimates, the median is reported as season value.
	##
	## In order to split the seasons, the uses must provide a vector with argument
	##  \code{seasonFactor}.
	## All positions with the same factor, belong to
	## the same season. It is conveniently generated by one of the following functions:
	## \itemize{
	## 	\item{ \code{\link{usCreateSeasonFactorMonth}}
	## 	  (default DJF-MAM-JJA-SON with December from previous to January of the year) }
	## 	\item{ \code{\link{usCreateSeasonFactorMonthWithinYear}}
	## 	  (default DJF-MAM-JJA-SON with December from the same year) }
	## 	\item{ \code{\link{usCreateSeasonFactorYday}}
	## 	  for a refined specification of season starts. }
	## 	\item{ \code{\link{usCreateSeasonFactorYdayYear}}
	## 	  for specifying different seasons season between years. }
	## }
	##
	## The estimation of Ustar on a single binned series can be selected argument
	## \code{fEstimateUStarBinned}.
	## \itemize{
	## 	\item{ \code{\link{usEstUstarThresholdSingleFw1Binned}} }
	## 	\item{ \code{\link{usEstUstarThresholdSingleFw2Binned}} (default) }
	## }
	##
	## This function is called by
	## \itemize{
	## \item{ \code{\link{sEddyProc_sEstUstarThold}} which stores the result
	##    in the class variables (sUSTAR and sDATA).}
	## \item{ \code{\link{sEddyProc_sEstUstarThresholdDistribution}} which
	##    additionally estimates median and confidence intervals for each year
	##    by bootstrapping the original data within seasons.}
	## }
	##
	## For inspecting the NEE~uStar relationship plotting is provided by
	## \code{\link{sEddyProc_sPlotNEEVersusUStarForSeason}}
	#
	# add index columns to locate which season / tempClass / uStarBin each
	# record belongs
	# cannot directly change sDATA, in EddyProcC, because will be overwritten
	# in each bootstrap
	if (any(is.na(seasonFactor)) ) stop("usEstUstarThreshold: ",
      "encountered NA in seasonFactor.",
      " Need to specify a valid season for each record.")
  ds$season <- as.factor(seasonFactor)
	ds$seasonYear <- yearOfSeasonFactor[seasonFactor]
	ds$tempBin <- NA_integer_
	ds$uStarBin <- NA_integer_
	# extract valid (nighttime records)
	if (isCleaned) {
		isValidUStar <- TRUE
		dsc <- ds
	} else {
		isValidUStar <- usGetValidUstarIndices(ds,  swThr = ctrlUstarSub$swThr)
		dsc <- 	ds[isValidUStar, , drop = FALSE]
	}
	if (nrow(dsc) == 0L) stop("sEstUstarThreshold: no finite records in dataset")
	#
	tdsc <- as.data.frame(table(dsc$season)); colnames(tdsc) <- c("season", "nRec")
	# some seasons might be absent in dsc from cleaning, construct vectors
	# that report NA for missing seasons
	nRecValidInSeason <- merge(data.frame(season = levels(ds$season) )
	                           , tdsc, all.x = TRUE)
	nRecValidInSeasonYear <- merge(nRecValidInSeason, data.frame(
	  season = names(yearOfSeasonFactor), seasonYear = yearOfSeasonFactor)
	                            , all.x = TRUE)
	nYear <- nRecValidInSeasonYear %>%
	  group_by(!!sym("seasonYear")) %>%
	  summarize(nRec = sum(!!sym("nRec"), na.rm = TRUE))
	seasonYearsWithFewData <- nYear$seasonYear[
	    nYear$nRec < ctrlUstarSub$minRecordsWithinYear]
	#nRecValidInSeasonYear$seasonAgg <- nRecValidInSeasonYear$season
	#
	#dsi <- subset(dsc, season == 4)
	#dsi <- subset(dsc, season == 0)
	fEstimateUStarSeason <- function(...) {
		if (isTRUE(ctrlUstarEst$isUsingCPTSeveralT)) {
			##details<< \describe{\item{change point detection (CPT) method}{
			## With specifying
			## \code{ctrlUstarEst = usControlUstarEst(isUsingCPTSeveralT = TRUE)}
			## change point detection is applied instead of the moving point test
			## (e.g. with Fw2Binned).
			##
			## The sometimes sensitive binning of uStar values within a temperature
			## class is avoided.
			## Further, possible spurious thresholds are avoid by testing that the
			## model with a threshold
			## fits the data better than a model without a threshold using a
			## likelihood ratio test.
			## In addition, with CPT seasons are excluded where a threshold
			## was detected in only less
			## than ctrlUstarEst$minValidUStarTempClassesProp (default 20%) of the
			## temperature classes.
			##
			## Note, that this method often gives higher estimates of the u * threshold.
			## }}
			.estimateUStarSeasonCPTSeveralT(...)
		} else .estimateUStarSeason(...)
	}
	UstarSeasonsTempL <- dsc %>% split(.$season) %>% map(fEstimateUStarSeason
			#, .drop_o = FALSE, .inform = TRUE
			, ctrlUstarSub = ctrlUstarSub
			, ctrlUstarEst = ctrlUstarEst
			, fEstimateUStarBinned = fEstimateUStarBinned
	)
	#UstarSeasonsTemp <- laply(UstarSeasonsTempL, "[[", 1L)	# matrix (nSeason x nTemp)
	UstarSeasonsTemp <- do.call(rbind, map(UstarSeasonsTempL, 1L))
	uStarSeasons <- apply(UstarSeasonsTemp, 1, median, na.rm = TRUE)
	# different to C-version, report NA where threshold was found in
	# less than 20% of temperature classes
	iNonValid <- (rowSums(is.finite(UstarSeasonsTemp)) / ncol(UstarSeasonsTemp)) <
	    ctrlUstarEst$minValidUStarTempClassesProp
	uStarSeasons[iNonValid] <- NA_real_
	# extract the temperature and bin indices
	# season <- names(UstarSeasonsTempL)[2]
	for (season in names(UstarSeasonsTempL) ) {
		dsc[dsc$season == season, c("tempBin", "uStarBin")] <-
		  UstarSeasonsTempL[[season]]$bins.F
	}
	# check correct ordering
	#plot(tempBin ~ Tair, dsc, col = rainbow(8)[as.factor(dsc$season)] )
	#
	resultsSeason <- nRecValidInSeasonYear
	resultsSeason$uStarSeasonEst <- uStarSeasons
	#
	resultsSeasonYear <- resultsSeason %>%
	  group_by(!!sym("seasonYear")) %>%
	  summarize(
			uStarMaxSeason = {if (all(!is.finite(!!sym("uStarSeasonEst")))  ) NA_real_ else
			    max(!!sym("uStarSeasonEst"), na.rm = TRUE)}
			, nRec = sum(!!sym("nRec"))
			)
	resultsSeasonYear$uStarAggr <- resultsSeasonYear$uStarMaxSeason
	#---- for seasonYears with too few records and for seasonYears with no
	# seasonal estimate do a pooled estimate
	##details<< \describe{\item{One-big-season fallback}{
	## If there are too few records within one year, of when no season yielded a
	## finite u * Threshold estimate, then
	## the yearly u * Th is estimated by pooling the data from seasons within one
	## seasonYear.
	## The user can suppress using pooled data on few records by providing option
	## \code{ctrlUstarSub$isUsingOneBigSeasonOnFewRecords = FALSE}
	## (see \code{\link{usControlUstarSubsetting}})
	## }}
	seasonYearsPooled <- resultsSeasonYear$seasonYear[
	  !is.finite(resultsSeasonYear$uStarAggr) & resultsSeasonYear$nRec > 0]
	if (isTRUE(ctrlUstarSub$isUsingOneBigSeasonOnFewRecords) )
		seasonYearsPooled <- union(seasonYearsWithFewData, seasonYearsPooled)
	resultsSeasonYearPooled <- if (!length(seasonYearsPooled) ) {
				resultsSeasonYearPooled <- data.frame(seasonYear = NA_character_
				        , nRec = NA_integer_ , uStarPooled = NA_real_)[FALSE, ]
			} else {
				#dscPooled <- dsc %>% filter(UQ(sym("seasonYear")) %in% !!seasonYearsPooled)
				dscPooled <- dsc %>% filter(.data$seasonYear %in% !!seasonYearsPooled)
				if (!nrow(dscPooled)) {
				  stop("Expected valid uStar records for year ", seasonYearsPooled,
				       ", but got no records.\n",
				       "Does the analyzed dataset include single records of",
				       " adjacent years?")
				}
				UstarYearsTempL <- tmp <- dscPooled %>%
				  split(.$seasonYear) %>%
				  map(fEstimateUStarSeason
						, ctrlUstarSub = ctrlUstarSub
						, ctrlUstarEst = ctrlUstarEst
						, fEstimateUStarBinned = fEstimateUStarBinned
				)
				# matrix (nSeason x nTemp)
				UstarYearsTemp <-  do.call(rbind, map(UstarYearsTempL, 1L))
				uStarYears <- apply(UstarYearsTemp, 1, median, na.rm = TRUE)
				# different to C-version, report NA where threshold was found in less
				# than 20% of temperature classes
				iNonValid <- (rowSums(is.finite(UstarYearsTemp)) / ncol(UstarYearsTemp)) <
				  ctrlUstarEst$minValidUStarTempClassesProp
				uStarYears[iNonValid] <- NA_real_
				# omit gettting binning into ds
				# (do not overwrite bins from seasonal estimates)
				resultsSeasonYearPooled <- data.frame(
				  seasonYear = names(uStarYears)
				  , nRec = as.vector(table(dscPooled$seasonYear))
				  , uStarPooled = uStarYears
				)
			}
	resultsSeasonYear <- merge(resultsSeasonYear, resultsSeasonYearPooled
	                           , all.x = TRUE)
	isFinitePooled <- is.finite(resultsSeasonYear$uStarPooled)
	resultsSeasonYear$uStarAggr[isFinitePooled] <-
	  resultsSeasonYear$uStarPooled[isFinitePooled]
	#----- overall is the median across years
	uStarMedianYears <- median(resultsSeasonYear$uStarAggr, na.rm = TRUE)
	message(paste("Estimated UStar threshold of: ", signif(uStarMedianYears, 2)
					, "by using controls:\n", paste(capture.output(unlist(ctrlUstarSub))
					, collapse = "\n")
			))
	#----- propagate aggregate estimates back to NA-slots of years and seaons
	isNonFinite <- !is.finite(resultsSeasonYear$uStarAggr)
	resultsSeasonYear$uStarAggr[isNonFinite] <- uStarMedianYears
	# merge yearly aggregated estimates to season and replace by finite
	# seasonal estimates
	resultsSeason <- merge(resultsSeason
	                       , resultsSeasonYear[, c("seasonYear", "uStarAggr")]
	                       , all.x = TRUE)
	isFiniteEst <- (is.finite(resultsSeason$uStarSeasonEst))
	resultsSeason$uStarAggr[isFiniteEst] <- resultsSeason$uStarSeasonEst[isFiniteEst]
	#
	resultsDf <- resultsSeason[, c("season", "seasonYear", "uStarAggr")]
	resultsDf$season <- as.factor(resultsDf$season)
	resultsDf$aggregationMode <- "season"
	resultsDf <- tmp <- rbind(cbind(data.frame(aggregationMode = "year"
	       , season = as.factor(NA_integer_))
	       , resultsSeasonYear[, c("seasonYear", "uStarAggr")])
	       , resultsDf)
	resultsDf <- tmp <- rbind(cbind(data.frame(aggregationMode = "single"
	       , season = as.factor(NA)
	       , seasonYear = NA_integer_)
	       , uStarAggr = uStarMedianYears)
	       , resultsDf)
	resultsDf$uStar <- resultsDf$uStarAggr
	# store indices in ds, first remove columns
	ds[isValidUStar, ] <- dsc
	# check correct ordering
	#plot(tempBin ~ Tair, ds, col = rainbow(8)[as.factor(ds$season)] )
	##value<< A list with entries
	res <- list(
			uStarTh = resultsDf[, c("aggregationMode", "seasonYear", "season", "uStar")]	##<<
			## data.frame with columns "aggregationMode", "seasonYear", "season", "uStar"
			## with rows for "single": the entire aggregate (median across years)
			##, "seasonYear": each year (maximum across seasons or estimate on pooled data)
			##, "season": each season (median across temperature classes)
			, seasonYear = resultsSeasonYear		##<< data.frame
			## listing results for year with columns "seasonYear"
			## , "uStarMaxSeason" the maximum across seasonal estimates within the year
			## , "uStarPooled" the estimate based on data pooled across the year
			##    (only calculated on few valid records or on uStarMaxSeason was nonfinite)
			## , "nRec" number of valid records  (only if the pooled estimate was calculated)
			## , "uStarAggr" chosen estimate, corresponding to uStarPooled
			##  if this was calculated,
			##   or uStarMaxSeason or uStarTh across years if the former was non-finite
			, season =  resultsSeason	##<< data.frame listing results for each season
			## , "nRec" the number of valid records
			## , "uStarSeasonEst" the estimate for based on data within the season
			##    (median across temperature classes)
			## , "uStarAggr" chose estimate, corresponding to uStarSeasonEst,
			##   or the yearly seasonYear$uStarAggr, if the former was non-finite
			, tempInSeason = t(UstarSeasonsTemp)		##<< numeric matrix
			## (nTemp x nAggSeason):
			## estimates for each temperature subset for each season
			, bins = ds[, c("season", "tempBin", "uStarBin")]		##<< columns
			## \code{season}, \code{tempBin} and \code{uStarBin}
			##  for each record of input \code{ds}
			## reporting classes of similar environmental conditions
			## that the record belongs to.
	)
	res
}


.plotNEEVersusUStarTempClass <- function(
	### plot NEE versus uStar for data of a subset with estimates
	NEEUStar.F		##<< data.frame or tibble with columns of NEE, Ustar and
	  ## Temperature, and columns 'uStarBin' and 'sDateTime',
	, uStarTh		##<< value of uStar of an estimated threshold
	, UstarColName = "Ustar"		##<< column name for UStar
	, NEEColName = "NEE"			##<< column name for NEE
	, TempColName = "Tair"		##<< column name for air temperature
	, xlab = bquote(u['*']*" (" * m~s^-1 * ")")
	, ylab = bquote("NEE (" * gC~ m^-2~yr^-1 * ")")
) {
	##author<< TW
  dss <- NEEUStar.F[, c(NEEColName, UstarColName, TempColName, "uStarBin", "sDateTime")]
	colnames(dss) <- c("NEE", "Ustar", "Temp", "uStarBin", "sDateTime")
	##details<< for each uStarBin, mean of NEE and uStar is calculated.
	dssm <- dss %>%
	  group_by(!!sym("uStarBin")) %>%
	  summarise(mUStar = mean(!!sym("Ustar")), mNEE = mean(!!sym("NEE")))
	plot(NEE ~ Ustar, dss, col = adjustcolor("brown", alpha.f = 0.3)
	   , ylim = quantile(dss$NEE, c(0.02, 0.98))
		, xlab = xlab, ylab = ylab
		#, col = rainbow(20)[dss$uStarBin] )
	)
	points(mNEE ~ mUStar, dssm, pch = "+", cex = 1.5)
	abline(v = uStarTh, lty = "dashed", col = "darkgrey", lwd = 2)
	dateRange <- strftime(range(dss$sDateTime), "%d.%m.%y")
	#\u2103 is degree Centigrade (degree symbol is not ascii) but does not
	# work with some devices
	#\u00B0 is degree only
	legend("topleft", legend = c(
					paste(NEEUStar.F$season[1], " (", dateRange[1], "-", dateRange[2], ")"
					  , sep = "")
					, sprintf(" (%1.1f-%1.1f\u00B0C)", min(dss$Temp), max(dss$Temp))
					, sprintf("uStarThr =%1.2f", uStarTh)
			))
	##value<< side effect of plotting NEE ~ Ustar with indicating Means of the bins,
	## uStarThreshold, Date-Range, and Temperature range
}
attr(.plotNEEVersusUStarTempClass, "ex") <- function() {
	EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F
	                   , c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
	res <- EddyProc.C$sEstUstarThold()
	dss <- cbind(EddyProc.C$sDATA, EddyProc.C$sTEMP
	             , EddyProc.C$sUSTAR_DETAILS$bins[,c("uStarBin","tempBin")])
	REddyProc:::.plotNEEVersusUStarTempClass(
	  subset(dss, season == "1998001" & tempBin == 5 & is.finite(NEE))
	  , uStarTh = 0.65)
}

.estimateUStarSeason <- function(
		### Estimate uStar threshold for data of a given season
		dsi			##<< dataframe with columns
		  ## UstarColName, NEEColName, TempColName, and RgColName
		, ctrlUstarSub, ctrlUstarEst, fEstimateUStarBinned
) {
	##author<< TW
	# result in correct format when no quit early
	resNA <- 	list(
			UstarTh.v = rep(NA_real_, ctrlUstarSub$taClasses)	##<< vector of
			  ## uStar for temperature classes
			, bins.F = data.frame(tempBin = rep(NA_integer_, nrow(dsi))
							 , uStarBin = rep(NA_integer_, nrow(dsi)) )			##<< data.frame
			  ## with columns tempBin, uStarBin for each row in dsi
	)
	if (nrow(dsi) < ctrlUstarSub$minRecordsWithinSeason) {
		warning("sEstUstarThreshold: too few finite records within season (n = ", nrow(dsi)
		        , "). Need at least n = ", ctrlUstarSub$minRecordsWithinSeason
		        , ". Returning NA for this Season.")
		return(resNA)
	}
	if (nrow(dsi) / ctrlUstarSub$taClasses < ctrlUstarSub$minRecordsWithinTemp) {
		warning("sEstUstarThreshold: too few finite records within season (n = ", nrow(dsi)
		        , ") for ", ctrlUstarSub$taClasses, " temperature classes."
		        ," Need at least n = ", ctrlUstarSub$minRecordsWithinTemp * ctrlUstarSub$taClasses
				    , ". Returning NA for this Season.")
		return(resNA)
	}
	orderTemp <- order(dsi[["Tair"]])
	uStarBinSortedT <- integer(nrow(dsi))		# default for methods that do not bin uStar
	dsiSort <- dsi[orderTemp, , drop = FALSE] 	#sort values in a season by
	# air temperature (later in class by ustar)
	# N <- nrow(dsi) #number of observations (rows) total,
	# probably can get from elsewhere..
	# T_bin_size <- round(N / ctrlUstarSub$taClasses) #set T_bin size so
	# that every bin has equal #values
	# set up vector that contains Ustar values for temperature classes
	UstarTh.v = vector(length = ctrlUstarSub$taClasses)
	# twutz 1505: changed temperature binning of records to put equals temperatures
	# into the same bin (compatibility with C code)
	#trace(.binWithEqualValues, recover)		#untrace(.binWithEqualValues)
	TId <- .binWithEqualValuesBalanced(dsiSort[, "Tair"], ctrlUstarSub$taClasses)
	#k<- 1L
	for (k in 1:ctrlUstarSub$taClasses) {	# k temperature class
		isCurrentTclass <- TId == k
		dsiSortTclass <- dsiSort[isCurrentTclass, ]
		#constraint: u * threshold only accepted if
		#T and u * are not or only weakly correlated..
		Cor1 = suppressWarnings(
		  abs(cor(dsiSortTclass[["Ustar"]], dsiSortTclass[["Tair"]])) )
		# maybe too few or degenerate cases
		#if (inherits(Cor1, "try-error") ) recover()
		# TODO: check more correlations here? [check C code]
		#      Cor2 = abs(cor(dataMthTsort$Ustar, dataMthTsort$nee))
		#      Cor3 = abs(cor(dataMthTsort$tair, dataMthTsort$nee))
		if ( (is.finite(Cor1)) && (Cor1 < ctrlUstarEst$corrCheck)) {
		  #& Cor2 < CORR_CHECK & Cor3 < CORR_CHECK) {
			if (isTRUE(ctrlUstarEst$isUsingCPT) ) {
				if (!requireNamespace('segmented') ) stop(
				  "Need to install package segmented before using Change Point Decetion "
				  ,"for estimation of UStar threshold.")
				resCPT <- try(
				  .fitSeg1(dsiSortTclass[["Ustar"]], dsiSortTclass[["NEE"]])
				  , silent = TRUE)
				UstarTh.v[k] <- if (
				    inherits(resCPT, "try-error") ||
				    !is.finite(resCPT["p"]) ||
				    resCPT["p"] > 0.05
				  ) NA else resCPT["cp"]
				#plot(dsiSortTclass[["Ustar"]], dsiSortTclass[["NEE"]] )
			} else {
				resBin <- .binUstar(dsiSortTclass[["NEE"]], dsiSortTclass[["Ustar"]]
				                    , ctrlUstarSub$UstarClasses)
				dsiBinnedUstar <- resBin$binAverages
				uStarBinSortedT[isCurrentTclass] <- resBin$uStarBins
				#plot(NEE_avg ~ Ust_avg, dsiBinnedUstar)
				if (any(!is.finite(dsiBinnedUstar[[2]])) ) {
					stop("Encountered non-finite average NEE for a UStar bin.",
							"You need to provide data with non-finite columns uStar"
							," and NEE for UStar Threshold detection.")
				}
				UstarTh.v[k] <- if (dsiBinnedUstar[[1]][1] > ctrlUstarEst$firstUStarMeanCheck
				) {
					##details<<
					## If the first mean uStar bin is already large
					## (>ctrlUstarEst$firstUStarMeanCheck)
					## Then this temperature class is skipped from estimation
					NA_real_
				} else {
					fEstimateUStarBinned( dsiBinnedUstar, ctrlUstarEst = ctrlUstarEst)
				}
			}
		} else {#correlation between T and u * too high
			#fill respective cell with NA
			UstarTh.v[k] = NA
			#TODO: should a message be printed here to the user??
		}
	}
	# bins of temperature and uStar have been generated on sorted data.frame.
	# Need to assign to original positions
	TIdUnsorted <- uStarBinUnsortedT <- integer(length(orderTemp));
	TIdUnsorted[orderTemp] <- TId
	uStarBinUnsortedT[orderTemp] <- uStarBinSortedT
	##value<< list with entries
	invisible(list(
			UstarTh.v = UstarTh.v 	##<< vector of uStar for temperature classes
			, bins.F = data.frame(tempBin = TIdUnsorted, uStarBin = uStarBinUnsortedT)	##<<
			  ## data.frame with columns tempBin, uStarBin for each row in dsi
	))
}


#' @export
usControlUstarEst <- function(
  ### Default list of parameters for determining UStar of a single binned series
  ustPlateauFwd = 10 	  ##<< number of subsequent uStar bin values to compare
    ## to in fwd mode
  , ustPlateauBack = 6	##<< number of subsequent uStar bin values to compare
    ## to in back mode
  , plateauCrit = 0.95	##<< significant differences between a uStar value and
    ## the mean of a "plateau"
  , corrCheck = 0.5 		##<< threshold value for correlation between Tair
    ## and u * data
  , firstUStarMeanCheck = 0.2	##<< if first uStar bin average of a class is already
    ## larger than this value, the temperature class is skipped.
  , isOmitNoThresholdBins = TRUE	##<< if TRUE, bins where no threshold was found
    ## are ignored. Set to FALSE to report highest uStar bin for these cases
  , isUsingCPTSeveralT = FALSE		##<< set to TRUE to use change point detection
    ## without binning uStar but with additionally changed aggregation scheme for
    ## several temperature classifications
  , isUsingCPT = FALSE				##<< set to TRUE to use change point detection without
    ## binning uStar before in usual aggregation method (good for comparing methods,
    ## but not recommended, overruled by isUsingCPTSeveralT = TRUE)
  , minValidUStarTempClassesProp = 0.2 ##<< seasons, in which only less than this
    ## proportion of temperature classes a threshold was detected,
    ## are excluded from aggregation
  , minValidBootProp = 0.4	##<< minimum proportion of bootstrap samples
    ## for which a threshold was detected. Below this proportion
    ## NA quantiles are reported.
  , minNuStarPlateau = 3L  ##<< minimum number of records in plateau, threshold
    ## must be larger than mean of this many bins
  #TODO: what does the following param do?
  #define FIRST_Ustar_MEAN_CHECK  		0.2
  # 4.) const int percentiles[PERCENTILES_COUNT] = { 5, 10, 25, 50, 75, 90, 95};
) {
	##author<< TW
  ##seealso<< \code{\link{usEstUstarThresholdSingleFw2Binned}},
  ##  \code{\link{usControlUstarSubsetting}}
  ctrl <- list(
    #taClasses = taClasses
    #, UstarClasses = UstarClasses
    #percentile = percentile
    #percentile_check = percentile_check #enable percentile check\n ... double check!
    ustPlateauFwd = ustPlateauFwd
    , ustPlateauBack = ustPlateauBack
    , plateauCrit = plateauCrit
    , corrCheck = corrCheck
	, firstUStarMeanCheck = firstUStarMeanCheck
	, isOmitNoThresholdBins = isOmitNoThresholdBins
	, isUsingCPT = isUsingCPT
	, isUsingCPTSeveralT = isUsingCPTSeveralT
	, minValidUStarTempClassesProp = minValidUStarTempClassesProp
	, minValidBootProp = minValidBootProp
	, minNuStarPlateau = minNuStarPlateau
	#, seasons = seasons # switch for three different seasonal modes
    #(seasons or "groupby" may easily extended to an input vector or matrix)
    #, bt = bt #flag for bootstrapping
    #, btTimes = btTimes #number of bootstrap samples
  )
  #display warning message for the following variables that we advise not to be changed
  if (corrCheck != 0.5) warning("WARNING: parameter corrCheck set to non default value!")
  ctrl
}
attr(usControlUstarEst, "ex") <- function() {
	usControlUstarEst()
}

#' @export
usControlUstarSubsetting <- function(
	### Default list of parameters for subsetting the data for uStarThreshold estimation
	taClasses = 7 		##<< set number of air temperature classes
	, UstarClasses = 20 	##<< set number of Ustar classes
	# seasons param deprecated
  # TODO: add seasons handling to documentation
  #, seasons = 1 # switch for different seasonal modes #TODO: Update?!
	, swThr = 10  		##<< nighttime data threshold for solar radiation [Wm-2]
	, minRecordsWithinTemp = 100		##<< integer scalar: the minimum number of
	  ## Records within one Temperature-class
	, minRecordsWithinSeason = 160	##<< integer scalar: the minimum number of
	  ## Records within one season
	, minRecordsWithinYear	= 3000	##<< integer scalar: the minimum number of
	  ## Records within one year
	, isUsingOneBigSeasonOnFewRecords = TRUE ##<< boolean scalar: set to FALSE to
	  ## avoid aggregating all seasons on too few records
	# 1.) , selection parameter for which fwd and back modes? fwd2 as default...
	# 2.) , MIN_VALUE_PERIOD <<- 3000 # per whole data set... double check C code
	# 3.) , MIN_VALUE_SEASON <<- 160 #if #number of data points in one any season
	# are smaller than that, merge to one big season
	#define MIN_VALUE_PERIOD    		3000		/* min values for compute u * threshold */
	#define MIN_VALUE_SEASON				160			/* min for seasons */
	#define TA_CLASS_MIN_SAMPLE				100
) {
	  ##author<< TW
	  ##seealso<< \code{\link{usEstUstarThresholdSingleFw2Binned}}
	  ##, \code{\link{usControlUstarSubsetting}}
	ctrl <- list(
    	taClasses = taClasses
		, UstarClasses = UstarClasses
  	, swThr = swThr
		, minRecordsWithinTemp = minRecordsWithinTemp
		, minRecordsWithinSeason = minRecordsWithinSeason
		, minRecordsWithinYear = minRecordsWithinYear
		, isUsingOneBigSeasonOnFewRecords = isUsingOneBigSeasonOnFewRecords
  )
  if (ctrl$swThr != 10) warning(
    "WARNING: parameter swThr set to non default value!")
  if (ctrl$taClasses != 7) warning(
    "WARNING: parameter taClasses set to non default value!")
  if (ctrl$UstarClasses != 20) warning(
    "WARNING: parameter UstarClasses set to non default value!")
  if (ctrl$minRecordsWithinTemp != 100) warning(
    "WARNING: parameter minRecordsWithinTemp set to non default value!")
  if (ctrl$minRecordsWithinSeason != 160) warning(
    "WARNING: parameter minRecordsWithinSeason set to non default value!")
  if (ctrl$minRecordsWithinYear != 3000) warning(
    "WARNING: parameter minRecordsWithinYear set to non default value!")
  ctrl
}
attr(usControlUstarSubsetting, "ex") <- function() {
	usControlUstarSubsetting()
}

#' @export
usCreateSeasonFactorMonth <- function(
		### Compute year-spanning Seasonfactor by starting month
		dates							##<< POSIXct vector of length of the data set to
		  ##be filled, specifying the center-time of each record
		, month = as.POSIXlt(dates)$mon + 1L   	##<< integer (1-12) vector of
		  ## length of the data set to be filled, specifying the month for each record
		, year = as.POSIXlt(dates)$year + 1900L	##<< integer vector of length of
		  ## the data set to be filled, specifying the year
		, startMonth = c(3, 6, 9, 12)		##<< integer vector specifying
		  ##the starting month for each season, counting from one. Default is
		  ## (Dez, Jan, Feb)(Mar, April, May)(June, July, August), (Sept, Oct, Nov)
) {
	##author<<
	## TW
	##seealso<<
	## \code{\link{usCreateSeasonFactorMonthWithinYear}},
	## \code{\link{usCreateSeasonFactorYday}},
	## \code{\link{usCreateSeasonFactorYdayYear}}
	##details<<
	## Compute factors to denote the season for uStar-Filtering by specifying
	## starting months, with continuous seasons spanning year boundaries
	## If Jan is not a starting month, then the first months of each year will be
	## part of the last period in the year.
	## E.g. with the default the fourth period of the first year consists of
	## Jan, Feb, Dec.
	##
	## REddyProc internally works with a timestamp 15 minutes after the start
	## of each half hour.
	## When providing the \code{dates} argument, user may shift the start time
	## by \code{dates = myDataset$DateTime + 15 * 60}
	#
	if (length(year) == 1L) year <- rep(year, length(month))
	if (length(month) != length(year) ) stop(
	    "Month and Year arguments need to have the same length.")
	if (any(month < 1 | month > 12) ) stop("Month out of range 1..12")
	starts <- data.frame(
	  month = sort(unique(startMonth))
	  , year = rep(sort(unique(year))
	   , each = length(startMonth)) )
	if (starts$month[1] != 1L) starts <- rbind(data.frame(month = 1L
	     , year = starts$year[1]), starts)
	seasonFac <- integer(length(month)) # 0L
	starts$startYearMonths <- startYearMonths <- starts$year * 1000L + starts$month
	yearMonths <- year * 1000L + month
	# i <- 1
	for (i in 1:(length(startYearMonths) - 1) ) {
		bo <- (yearMonths >= startYearMonths[i]) & (yearMonths < startYearMonths[i + 1])
		seasonFac[bo] <- starts$year[i] * 1000L + starts$month[i]
	}
	# last period with no end border defined
	i <- length(startYearMonths)
	bo <- (yearMonths >= startYearMonths[i])
	seasonFac[bo] <- starts$year[i] * 1000L + starts$month[i]
	#plot(seasonFac ~ dates)
	as.factor(seasonFac)
	##value<<
	## Integer vector length(dates), with each unique value representing one season
}

.tmp.f <- function() {
	EddyDataWithPosix.F <- ds <- fConvertTimeToPosix(
	  Example_DETha98, 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour')
	(res <- usCreateSeasonFactorMonth(ds$DateTime))
	plot.default(res ~ ds$DateTime, type = "p")
}

#' @export
usCreateSeasonFactorMonthWithinYear <- function(
  ### Compute year-bounded Seasonfactor by starting month
	dates							##<< POSIXct vector of length of the data set to be filled,
	  ## specifying the center-time of each record
	, month = as.POSIXlt(dates)$mon + 1   ##<< integer (1-12) vector of length
	  ## of the data set to be filled, specifying the month for each record
	, year = as.POSIXlt(dates)$year + 1900	##<< integer vector of length of
	  ## the data set to be filled, specifying the year
	, startMonth = c(3, 6, 9, 12)		##<< integer vector specifying the starting
	  ## month for each season, counting from one. Default is
	  ## (Dez, Jan, Feb)(Mar, April, May)(June, July, August), (Sept, Oct, Nov)
) {
	##author<< TW
	##seealso<< \code{\link{usCreateSeasonFactorMonth}}
  ##details<<
  ## Calculate factors to denote the season for uStar-Filtering by specifying
  ## starting months, with seasons not spanning year boundaries
  ## If Jan is not a starting month, then the first months of each year will be
  ## part of the last period in the year.
  ## E.g. with the default the fourth period of the first year consists of
  ## Jan, Feb, Dec.
  if (length(year) == 1L) year <- rep(year, length(month))
  if (length(month) != length(year) ) stop(
    "Month and Year arguments need to have the same length.")
  if (any(month < 1 | month > 12) ) stop("Month out of range 1..12")
  startMonth <- sort(unique(startMonth))
  boLastPeriod <- month < startMonth[1]
  # translate month before the first specified beginning month to be
  # after last specified month (1 becomes 13)
  month[boLastPeriod] <- month[boLastPeriod] + 12
  startMonthAdd <- c(startMonth, startMonth[1] + 12)
  seasonFac <- year * 1000L + rep(startMonth[1], length(month) )
  # i <- 2
  for (i in 2:length(startMonth) ) {
	  bo <- (month >= startMonthAdd[i]) & (month < startMonthAdd[i + 1])
	  seasonFac[bo] <- year[bo] * 1000L + startMonth[i]
  }
  table(seasonFac)
  #plot.default(as.factor(seasonFac) ~ as.POSIXlt(dates)$mon + 1)
  # levels(as.factor(seasonFac))
  as.factor(seasonFac)
  ##value<<
  ## Integer vector length(dates), with each unique value representing one season
}

.tmp.f <- function() {
	EddyDataWithPosix.F <- ds <- fConvertTimeToPosix(
	  Example_DETha98, 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour')
	table(res <- usCreateSeasonFactorMonthWithinYear(ds$DateTime - 1))
	  #-1 to move last record of newYear to 1998
}


#' @export
usCreateSeasonFactorYday <- function(
  ### Compute year-spanning Seasonfactor by starting year-day
	dates							##<< POSIXct vector of length of the data set to be filled,
	  ## specifying the center-time of each record
	, yday = as.POSIXlt(dates)$yday + 1L  ##<< integer (1-366) vector of length
	  ## of the data set to be filled, specifying the day of the year
	  ## (1..366) for each record
	, year = as.POSIXlt(dates)$year + 1900L	##<< integer vector of length of
	  ## the data set to be filled, specifying the year
	, startYday = c(335, 60, 152, 244)	 ##<< integer vector (1-366) specifying
	  ## the starting yearDay for each season in increasing order
) {
	##author<< TW
	##seealso<< \code{\link{usCreateSeasonFactorMonth}}
	##details<<
	## With default parameterization, dates are assumed to denote begin
	## or center of the eddy time period.
	## If working with dates that denote the end of the period,
	## use \code{yday = as.POSIXlt(fGetBeginOfEddyPeriod(dates))$yday}
	starts <- data.frame(
	             yday = sort(unique(startYday))
	           , year = rep(sort(unique(year)), each = length(startYday)) )
	usCreateSeasonFactorYdayYear(dates, yday, year, starts)
	##value<<
	## Integer vector of length \code{nrow(ds)},
	## each unique class representing one season
}

.tmp.f  <- function() {
	EddyDataWithPosix.F <- ds <- fConvertTimeToPosix(
	  Example_DETha98, 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour')
	table(res <- usCreateSeasonFactorYday(ds$DateTime))
	plot.default(res ~ ds$DateTime)
}

#' @export
usCreateSeasonFactorYdayYear <- function(
    ### Compute year-spanning Seasonfactor by starting year and yearday
		dates							##<< POSIXct vector of length of the data set to be filled,
		  ## specifying the center-time of each record
		, yday = as.POSIXlt(dates)$yday + 1L  ##<< integer (1-366) vector of
		  ## length of the data set to be filled, specifying the day of the
		  ## year (1..366) for each record
		, year = as.POSIXlt(dates)$year + 1900L	##<< integer vector of length of
		  ## the data set to be filled, specifying the year
		, starts	 					 ##<< data.frame with first column specifying the
		  ## starting yday (integer 1-366) and second column the year
		  ## (integer e.g. 1998) for each season in increasing order
) {
	##author<< TW
	##seealso<< \code{\link{usCreateSeasonFactorMonth}}
	##details<<
	## With default parameterization, dates are assumed to denote begin
	## or center of the eddy time period.
	## If working with dates that denote the end of the period,
	## use \code{yday = as.POSIXlt(fGetBeginOfEddyPeriod(dates))$yday}
	##
	if (length(year) == 1L) year <- rep(year, length(yday))
	if (length(yday) != length(year) ) stop(
	    "Month and Year arguments need to have the same length.")
	if (any(yday < 1 | yday > 366) ) stop("yday out of range 1..366")
	#
	colnames(starts) <- c("yday", "year")
	if (starts$yday[1] != 1L) starts <- rbind(data.frame(yday = 1L
	                                 , year = starts$year[1]), starts)
	seasonFac <- integer(length(yday)) # 0L
	starts$startYearDays <- startYearDays <- starts$year * 1000L + starts$yday
	yearDays <- year * 1000L + yday
	# i <- 1
	for (i in 1:(length(startYearDays) - 1) ) {
		bo <- (yearDays >= startYearDays[i]) & (yearDays < startYearDays[i + 1])
		seasonFac[bo] <- starts$year[i] * 1000L + starts$yday[i]
	}
	# last period with no defined end border
	i <- length(startYearDays)
	bo <- (yearDays >= startYearDays[i])
	seasonFac[bo] <- starts$year[i] * 1000L + starts$yday[i]
	#plot(seasonFac ~ dates); levels(as.factor(seasonFac))
	as.factor(seasonFac)
	##value<<
	## Integer vector of length \code{nrow(ds)},
	## each unique class representing one season
}


#' @export
usGetYearOfSeason <- function(
		### determine the year of the record of middle of seasons
		seasonFactor		##<< factor vector of length data:
		  ## for each record which season it belongs to
		, sDateTime.v		  ##<< POSIX.t vector of length data:
		  ## for each record: center of half-hour period
		  ## (corresponding to sDATA$sDateTime)
) {
	##author<<
	## TW
	originCt <- as.POSIXct("1970-01-01 00:00.00", tz = "UTC")
	timezone <- attr(sDateTime.v[1], "tzone")
	#dates <- sDateTime.v[seasonFactor == seasonFactor[1]]
	res <- tapply(sDateTime.v, seasonFactor, FUN = function(dates) {
				x <- as.numeric(dates)
				xCenter <- x[1] + (x[length(x)] - x[1]) / 2
				1900L + as.POSIXlt(xCenter, origin = originCt, tz = timezone)$year
			})
	##value<<  named integer vector, with names corresponding to seasons
	# need to convert 1d array to vector
	structure(as.vector(res), names = rownames(res))
}
.tmp.f <- function() {
	ds <- eddyProc$sDATA
	sDateTime.v <- ds$sDateTime
	seasonFactor <- usCreateSeasonFactorMonth(ds$sDateTime)
	usGetYearOfSeason(seasonFactor, sDateTime.v)
	usGetYearOfSeason(seasonFactor, sDATA$sDateTime)
}




.binUstar <- function(
	### Bin the NEE for a number of classes of UStar classes
	NEE.v				##<< vector with value of Net Ecosystem exchange
	, Ustar.v 			##<< vector with u * (friction velocity (m2 / s)
	, UstarClasses = usControlUstarSubsetting()$UstarClasses	##<<
	  ## the number of binning classes
	, isUStarSorted = FALSE	##<< set to TRUE, if NEE and Ustar are already
	  ## sorted by increasin Ustar values (performance gain)

) {
	##author<< TW
	ds.F <- ds0.F <- data.frame(NEE = NEE.v, Ustar = Ustar.v)
	#within data frame sort values by Ustar
	if (!isTRUE(isUStarSorted)) {
		orderUStar <- order(Ustar.v)
		ds.F <- ds.F[orderUStar, ]
	}else{
		orderUStar <- TRUE
	}
	#
	# twutz 1505: changed binning to take care of equal values in uStar column
	# when assigning uStar classes, only start a new class when uStar value changes
	ds.F$uClass <- .binWithEqualValuesMinRec(
	  ds.F$Ustar, nBin = UstarClasses, tol = 1e-14)
	binAverages <- ds.F %>%
	  group_by(!!sym("uClass")) %>%
	  summarise(
			  Ust_avg = mean(!!sym("Ustar"), na.rm = TRUE)
			, NEE_avg = mean(!!sym("NEE"), na.rm = TRUE)
			, nRec = length(!!sym("NEE"))
			) %>%
		select(-1)	# omit first column
	uStarBinsUnsorted <- integer(nrow(ds.F))
	uStarBinsUnsorted[orderUStar] <- as.integer(ds.F$uClass)
	# plot(uStarBinsUnsorted ~ ds0.F$Ustar)	# for checking correct ordering
	##value<< list with entries
	list(
			binAverages = binAverages		    ##<< data.frame with columns Ust_avg,
			  ## NEE_avg nRec with one row for each bin
			, uStarBins = uStarBinsUnsorted	##<< integer vector reporting the bin
			  ## for each record in Ustar.v
			)
}

.binWithEqualValuesBalanced <- function(
  ### Create a binning factor with shortening following bins
	x				        ##<< sorted numeric vector to sort into bins
	, nBin			    ##<< intended number of bins
	, tol = 1e-8		##<< distance between successive values of x that
	  ## are treated to be equal
	, isBinSizeFloorUsed = TRUE	##<< set to FALSE to postpone rounding on
	  ## start and end values
) {
	if (nBin == 1L) return(integer(length(x)))
	binSize <- length(x) / nBin
	##details<<
	## Equal values of x end up in the same bin
	## It shortens the following bins
	## By not taking the floor, a better distribution of
	## samples across bins is achieved.
	## But here keep it due to compatibility to C-Code.
	if (isBinSizeFloorUsed) binSize <- floor(binSize)
	breaksX <- which(diff(x) > tol) + 1
	binId <- rep(1L, length(x))
	iBreak <- 1L	# index from which to seek next break
	#iClass <- 2L
	for (iClass in 2L:as.integer(nBin)) {
		start0 <- round((iClass - 1) * binSize) + 1
		iBreak <- .whichValueGreaterEqual(breaksX, start0, iStart = iBreak)
		start1 <- breaksX[iBreak]
		# find next uStar change at or after position start0
		#start1Slow <- breaksX[breaksX >= start0][1]
		binId[start1:length(x)] <- iClass
	}
	##value<< integer vector of same length as x, with unique value for each bin
	binId
}

.binWithEqualValuesMinRec <- function(
  ### Create a binning factor with shifting following bins
	x				    ##<< sorted numeric vector to sort into bins
	, nBin			##<< intended number of bins
	, tol = 1e-8		##<< distance between successive values of x
	  ## that are treated to be equal
) {
	##author<< TW
	lengthX <- length(x)
	binId <- integer(lengthX)
	binSize <- as.integer(floor(lengthX / nBin))
	iBreaksX <- which(diff(x) > tol)		# positions in x where value
	# is numerically different from following element
	iBreak <- 0L		# start index in iBreaks, to avoid searching the samller el.
	iEnd <- 0L			# index in x, end of the (previous) period
	iBin <- 0L			# bin Id
	while (iEnd < lengthX) {
		iBin <- iBin + 1L
		iStart <- iEnd + 1L
		iEnd <- iEnd + binSize	# same as iStart + binsSize-1,
		# with counting from 1 instead of 0
		# find the next break after iEnd
		iBreak <- .whichValueGreaterEqual(iBreaksX, iEnd, iBreak + 1L)
		if (is.na(iBreak) ) {
			# no break was found, set period end to vector end and finish
			# if length of last bin is smaller than 90% of intended binsize,
			# sort records to former bin
			if ( (lengthX + 1L - iStart) < binSize * 0.9 && iBin != 1L)
				iBin <- iBin - 1L
			binId[iStart:lengthX] <- iBin
			break
		} else {
			iEnd <- iBreaksX[iBreak]	# update iEnd to position with break after it
			binId[iStart:iEnd] <- iBin
		}
	}
	##value<< integer vector of same length as x, with unique value for each bin.
	## Each bin holds at least floor(length(x) / nBin) records, or more if there
	## were values after the bin that were
	## numerically equal to last value of the bin.
	## The actual number of bins might be differnt from argument nBin due
	## to numericall equal values
	## and is reported with attribute \code{nBin}
	## Because of using floor in bin-width calculation, the last, i.e. nbin,
	## of the bins may hold more values.
	#
	# for C-code compatibility do not use more than nBin classes, and increase
	# the size of the last class
	binId[binId > nBin] <- nBin
	attr(binId, "nBin") <- min(iBin, nBin)
	binId
}

.whichValueGreaterEqual <- function(
	### search first element in an integer vector that is larger
	x			##<< increasingly sorted numeric vector to search
	, threshold	##<< integer scalar: searched element will need to
	  ## be greater or equal as this argument
	, iStart = 1L	##<< index in vector to start search
) {
	##author<< TW
	# see tests / test_binWithEqualValues.R
	#which(x >= threshold)[1]
  #
	# for performance reasons call a c ++ function that loops across the vector
	#
	# cannot generate C function with dot
	# Rcpp::compileAttributes() generates a function without leading dot,
	# need to adjust by hand afterwards
	# or otherwise export but make sure its documented
	##details<<
	## searches a sorted integer vector for the next element
	## that is >= a threshold in fast C-code
  ans <- whichValueGreaterEqualC(
	  as.integer(x), as.integer(threshold), as.integer(iStart) )
  return(ans)
  #if (iStart > length(x)) return(NA_integer_)
  #  return(iStart - 1 + which(x[iStart:length(x)] >= threshold)[1])
  ##value<<
	## Scalar integer: first index in x, that is >= iStart,
	## and whose value x[i] is >= threshold.
	## If no index was found, returns NA
}

#' @export
usEstUstarThresholdSingleFw1Binned <- function(
		### estimate the Ustar threshold for single subset, using FW1 algorithm
		Ust_bins.f			##<< data.frame with columns NEE_avg and Ust_avg, of Ustar bins
		, ctrlUstarEst = usControlUstarEst() ##<< parameter list,
		  ##see \code{\link{usControlUstarEst}} for defaults and description
) {
	##author<< TW, OM
	##references<< inspired by Papale 2006
  ##details<<
  ## Relying on binned NEE and Ustar
	# algorithm to check when plateau is reached
	flag <- FALSE
	#for every u * bin compare to avg of subsequent UST_PLATEAU, until found
	u <- 1
	#TODO: change to for loop 1:ustClasses and then break
	# in order to avoid infinite loop in case of error
	# optimize with Thomas?

  while (!flag) { #only stop if threshold is found
		if (!flag & (Ust_bins.f$NEE_avg[u] >=
		          (ctrlUstarEst$plateauCrit * mean(Ust_bins.f$NEE_avg[
		            (u + 1):(u + ctrlUstarEst$ustPlateauFwd)]
		            , na.rm = T)))
		    ) {
		  #na.rm = T to exclude NAs out of bounds..
			#   NEE_i >= .95 * avg(i, i + 1, ..., i + 10)  [FW]
			UstarThSingle <- Ust_bins.f$Ust_avg[u]
			flag <- TRUE #set flag for threshold found in this mode
		}
		# case with no threshold could be found by plateau method,
		# use maximum u * in that T_class...
		if (u == (nrow(Ust_bins.f) - 1)) { #FW1: -1 ; FW2:
			UstarThSingle <- Ust_bins.f$Ust_avg[u + 1]
			break;
		}
		u <- u + 1 #increase index by 1
	}
	return(UstarThSingle)
}

#' @export
usEstUstarThresholdSingleFw2Binned <- function(
  ### estimate the Ustar threshold for single subset, using FW2 algorithm
  Ust_bins.f	##<< data.frame with column s NEE_avg and Ust_avg, of Ustar bins
  , ctrlUstarEst = usControlUstarEst()	##<< parameter list,
    ## see \code{\link{usControlUstarEst}} for defaults and description
) {
	##author<< TW, OM
	# algorithm to check when plateau is reached
  flag <- FALSE
  #for every u * bin compare to avg of subsequent UST_PLATEAU, until found
  u <- 1
  UstarThSingle <- NA_real_
  ##details<<
  ## Demand that threshold is higher than \code{ctrlUstarEst$minNuStarPlateau}
  ## records. If fewer records
  # FF2 neads at least two bins after threshold
  umax <- nrow(Ust_bins.f) - max(2L, ctrlUstarEst$minNuStarPlateau)
  while (u <= umax) {
    if (
		(Ust_bins.f$NEE_avg[u] >= (ctrlUstarEst$plateauCrit *
		    mean(Ust_bins.f$NEE_avg[(u + 1):(u + ctrlUstarEst$ustPlateauFwd)]
		    , na.rm = T))) &
		(Ust_bins.f$NEE_avg[u + 1] >= (ctrlUstarEst$plateauCrit *
		    mean(Ust_bins.f$NEE_avg[(u + 1 + 1):(u + ctrlUstarEst$ustPlateauFwd + 1)]
		    , na.rm = T)))
	) {
    UstarThSingle <- Ust_bins.f$Ust_avg[u]
	  break
  }
	u = u + 1L
  }
  #case that no threshold could be found by plateau method, use maximum u*
  # in that T_class...
  # twutz: 1505: implemented option to return NA, to omit
  # from median over bins (C-compatibility)
  if (is.na(UstarThSingle) & !isTRUE(ctrlUstarEst$isOmitNoThresholdBins) )
	  UstarThSingle <- Ust_bins.f$Ust_avg[u + 1]
  return(UstarThSingle)
}

.tmp.f <- function() {
	plot(Ust_bins.f$NEE_avg ~ Ust_avg, Ust_bins.f)
}


usGetValidUstarIndices <- function(
		### remove non-finite cases and omit night time data.
		ds						            ##<< data.frame with columns
		, UstarColName = "Ustar"	##<< column name for UStar
		, NEEColName = "NEE"			##<< column name for NEE
		, TempColName = "Tair"		##<< column name for air temperature
		, RgColName = "Rg"			  ##<< column name for solar radiation
		  ## for omitting night time data
		, swThr = usControlUstarSubsetting()$swThr	##<< threshold below
		  ## which data is acknowledged as night time respiration.
) {
	##author<< TW
	bo <-
			is.finite(ds[, NEEColName]) &
					is.finite(ds[, TempColName]) &
					is.finite(ds[, UstarColName]) &
					is.finite(ds[, RgColName])
	bo <- bo & ds[, RgColName] < swThr
	##value<< boolean vector with non-finite cases and cases
	## with radiation < swThr set to FALSE.
	bo
}

#' @export
usGetAnnualSeasonUStarMap <- function(
		### extract mapping season -> uStar columns from Distribution result
		uStarTh		##<< result of \code{\link{sEddyProc_sEstUstarThresholdDistribution}}
		  ## or \code{\link{sEddyProc_sEstUstarThreshold}}$uStarTh
) {
	##author<< TW
	dsYear <- uStarTh[uStarTh$aggregationMode == "year", , drop = FALSE]
	dsYear$season <- NULL
	dsYear$aggregationMode <- NULL
	# deprecated: done in sEstUstarThreshold
	dsSeasons <- uStarTh[uStarTh$aggregationMode == "season"
	                   , c("season", "seasonYear"), drop = FALSE]
	res2 <- merge(dsSeasons, dsYear)
	res2$seasonYear <- NULL
	# transform column names of "x%" to "Ux" with leading zeros
	colnames(res2)[-(1:2)] <- (gsub(" ", "0", sprintf("U%2s", gsub("%", ""
	                                             , colnames(res2)[-(1:2)]))))
	##value<< a data frame with first column the season, and other columns
	## different uStar threshold estimates
	res2
}

#' @export
usGetSeasonalSeasonUStarMap <- function(
		### extract mapping season -> uStar columns from Distribution result
		uStarTh		##<< result of \code{\link{sEddyProc_sEstUstarThresholdDistribution}}
		## or \code{\link{sEddyProc_sEstUstarThreshold}}$uStarTh
) {
	##author<< TW
	#
	##details<<
	## from result of \code{\link{sEddyProc_sEstUstarThresholdDistribution}}
	# omit aggregation model and seasonYear column
	dsSeasons <- uStarTh[uStarTh$aggregationMode == "season", , drop = FALSE]
	dsSeasons$seasonYear <- NULL
	dsSeasons$aggregationMode <- NULL
	##value<< a data frame with first column the season, and other columns
	## different uStar threshold estimates
	# transform column names of "x%" to "Ux" with leading zeros
	colnames(dsSeasons)[-(1:2)] <- (gsub(" ", "0", sprintf("U%2s",
	                       gsub("%", "", colnames(dsSeasons)[-(1:2)]))))
	dsSeasons
}


#' @export
sEddyProc_sEstUstarThresholdDistribution <- function(
		### Estimate the distribution of u* threshold by bootstrapping over data
		...  ##<< further parameters to
		## \code{\link{sEddyProc_sEstimateUstarScenarios}}
) {
  ##details<< This method returns the results directly, without modifying
  ## the class. It is there for portability reasons. Recommended is
  ## using method \code{\link{sEddyProc_sEstimateUstarScenarios}} to
  ## update the class and then getting the results from the class by
  ## \code{\link{sEddyProc_sGetEstimatedUstarThresholdDistribution}}.
  updatedClass <- .self$sEstimateUstarScenarios(...)
  ##value<< result of
  ## \code{\link{sEddyProc_sGetEstimatedUstarThresholdDistribution}}
  updatedClass$sGetEstimatedUstarThresholdDistribution()
}
sEddyProc$methods(sEstUstarThresholdDistribution =
                    sEddyProc_sEstUstarThresholdDistribution)


#' @export
sEddyProc_sEstimateUstarScenarios <- function(
  ### Estimate the distribution of u* threshold by bootstrapping over data
  ctrlUstarEst = usControlUstarEst()			    ##<< control parameters
  ## for estimating uStar on a single binned series,
  ## see \code{\link{usControlUstarEst}}
  , ctrlUstarSub = usControlUstarSubsetting()	##<< control parameters
  ## for subsetting time series (number of temperature and Ustar classes
  ## \ldots), see \code{\link{usControlUstarSubsetting}}
  , UstarColName = "Ustar"		##<< column name for UStar
  , NEEColName = "NEE"			  ##<< column name for NEE
  , TempColName = "Tair"		  ##<< column name for air temperature
  , RgColName = "Rg"			    ##<< column name for solar radiation for
  ## omitting night time data
  , ...		##<< further arguments to \code{\link{sEddyProc_sEstUstarThreshold}}
  , seasonFactor = usCreateSeasonFactorMonth(sDATA$sDateTime) ##<<
  ## factor of seasons to split (data is resampled only within the seasons)
  , nSample = 200L				      ##<< the number of repetitions in the bootstrap
  , probs = c(0.05, 0.5, 0.95)	##<< the quantiles of the bootstrap sample
  ## to return. Default is the 5%, median and 95% of the bootstrap
  , isVerbose = TRUE				##<< set to FALSE to omit printing progress
) {
  ##author<< TW
  ##details<<
  ## The choice of the criterion for sufficiently turbulent conditions
  ## (u * > chosen threshold)
  ## introduces large uncertainties in calculations based on gap-filled Eddy data.
  ## Hence, it is good practice to compare derived quantities based on
  ## gap-filled data using a range of u * threshold estimates.
  ##
  ## This method explores the probability density of the threshold by
  ## repeating its estimation
  ## on a bootstrapped sample.
  ## By default it returns the 90% confidence interval (argument \code{probs}).
  ## For larger intervals the sample number need to be
  ## increased (argument \code{probs}).

  ##seealso<< \code{\link{sEddyProc_sEstUstarThold}}
  ##, \code{\link{sEddyProc_sGetEstimatedUstarThresholdDistribution}}
  ##, \code{\link{sEddyProc_sSetUstarScenarios}}
  ##, \code{\link{sEddyProc_sMDSGapFillUStarScens}}
  .self$sSetUStarSeasons(seasonFactor)
  ds <- sDATA[, c("sDateTime", UstarColName, NEEColName, TempColName, RgColName)]
  colnames(ds) <- c("sDateTime", "Ustar", "NEE", "Tair", "Rg")
  ds$seasonFactor <- .self$sTEMP$season
  res0 <- suppressMessages(.self$sEstUstarThold(
    UstarColName = UstarColName
    , NEEColName = NEEColName
    , TempColName = TempColName
    , RgColName = RgColName
    , ...
    , ctrlUstarEst = ctrlUstarEst, ctrlUstarSub = ctrlUstarSub
    , seasonFactor = NULL	))
  iPosAgg <- which(res0$aggregationMode == "single")
  iPosYears <- which(res0$aggregationMode == "year")
  iPosSeasons <- which(res0$aggregationMode == "season")
  years0 <- res0$seasonYear[iPosYears]
  seasons0 <- res0$season[iPosSeasons]
  fWrapper <- function(iSample, ...) {
    dsBootWithinSeason <- ds2 <- ds %>%
      split(.$seasonFactor) %>%
      map_df(function(dss) {
        iSample <- sample.int(nrow(dss), replace = TRUE)
        dss[iSample, , drop = FALSE]
      })
    if (isTRUE(isVerbose) ) message(".", appendLF = FALSE)
    res <- usEstUstarThreshold(
      dsBootWithinSeason, ...
      , seasonFactor = dsBootWithinSeason$season
      , ctrlUstarEst = ctrlUstarEst, ctrlUstarSub = ctrlUstarSub	)
    gc()
    # need to check if years and seasons have been calculated
    # differently due to subsetting with
    # too few values within a season
    # then report NA for those cases
    resAgg <- res$uStarTh$uStar[iPosAgg]
    years <- res$uStarTh$year[iPosYears]
    resYears <- structure(
      if (all(years == years0) ) res$uStarTh$uStar[iPosYears] else
        rep(NA_real_, length(years0)), names = as.character(years0) )
    resSeasons <- structure(
      if (
        nrow(res$uStarTh) == nrow(res0) &&
        all((seasons <- res$uStarTh$season[iPosSeasons]) == seasons0)
      ) res$uStarTh$uStar[iPosSeasons] else rep(NA_real_, length(seasons0))
      , names = as.character(seasons0) )
    return(c(aggYears = resAgg, resYears, resSeasons))
    #return(length(res$UstarSeason$uStar))
    #res$UstarAggr
  }
  # collect into one big matrix
  Ustar.l0 <- res0$uStar[c(iPosAgg, iPosYears, iPosSeasons)]
  Ustar.l <- suppressMessages(
    Ustar.l <- lapply(1:(nSample - 1), fWrapper, ...)
  )
  if (isTRUE(isVerbose) ) message("")	# line break
  stat <- do.call(rbind, c(list(Ustar.l0), Ustar.l))
  ##details<< \describe{\item{Quality Assurance}{
  ## If more than \code{ctrlUstarEst$minValidBootProp}
  ## (default 40%) did not report a threshold,
  ## no quantiles (i.e. NA) are reported.
  ## }}
  # workaround: if probs is a scalar, apply returns vector without names
  # in order to get the matrix in all caes, prepend extend probs
  # and delete the the corresponding row afterwards
  resQuantiles0 <-	apply(stat, 2, quantile, probs = c(0,probs), na.rm = TRUE)
  resQuantiles <- t(resQuantiles0[-1,,drop = FALSE])
  iInvalid <- colSums(is.finite(stat)) / nrow(stat) <
    ctrlUstarEst$minValidBootProp
  resQuantiles[iInvalid, ] <- NA_real_
  rownames(resQuantiles) <- NULL
  resDf <- cbind(res0, resQuantiles)
  message(paste("Estimated UStar distribution of:\n"
                , paste(capture.output(resDf[resDf$aggregationMode == "single"
                                             , -(1:3)]), collapse = "\n")
                , "\nby using ", nSample, "bootstrap samples and controls:\n"
                , paste(capture.output(unlist(ctrlUstarSub)), collapse = "\n")
  ))
  .self$sUSTAR <- resDf
  .self$sSetUstarScenarios(usGetAnnualSeasonUStarMap(resDf))
  ##value<< updated class. Request results by
  ##\code{\link{sEddyProc_sGetEstimatedUstarThresholdDistribution}}
  invisible(.self)
}
sEddyProc$methods(sEstimateUstarScenarios =
                    sEddyProc_sEstimateUstarScenarios)


#' @export
sEddyProc_sGetEstimatedUstarThresholdDistribution <- function(
  ### return the results of \code{\link{sEddyProc_sEstimateUstarScenarios}}
) {
  ##seealso<< \code{\link{sEddyProc_sSetUstarScenarios}}
  ##value<<
  ## A data.frame with columns \code{aggregationMode}, \code{year},
  ## and \code{UStar} estimate based on the non-resampled data.
  ## The other columns correspond to the quantiles of Ustar estimate
  ## for given probabilities (argument \code{probs}) based on the distribution
  ## of estimates using resampled the data.
  if (nrow(.self$sUSTAR)) {
    .self$sUSTAR
  } else {
    # distribution has no been estimated, return uStarDetails
    # from single call to sEstUstarThold
    if (is.null(.self$sUSTAR_DETAILS$uStarTh)) warning(
      "uStar threshold has not been estimated. Returning null")
    .self$sUSTAR_DETAILS$uStarTh
  }
}
sEddyProc$methods(sGetEstimatedUstarThresholdDistribution =
                    sEddyProc_sGetEstimatedUstarThresholdDistribution)

#' @export
sEddyProc_sApplyUStarScen <- function(
  ### apply a function with changing the suffix argument
  FUN  ##<< function to be applied
  , ...  ##<< further arguments to FUN
  , uStarScenKeep = character(0) ##<< Scalar string specifying the scenario
  ## for which to keep parameters. If not specified defaults to the first
  ## entry in \code{uStarSuffixes}.
) {
  ##details<<
  ## When repeating computations, some of the
  ## output variables maybe replaced. Argument \code{uStarKeep}
  ## allows to select the scenario which is computed last,
  ## and hence to which output columns refer to.
  uStarSuffixes = colnames(.self$sGetUstarScenarios())[-1]
  if (length(uStarScenKeep) != 1) uStarScenKeep = uStarSuffixes[1]
  iKeep = match(uStarScenKeep, uStarSuffixes)
  if (is.na(iKeep)) stop(
    "Provided uStarScenKeep=",uStarScenKeep," was not among Scenarios: "
    ,paste(uStarSuffixes,collapse = ","))
  uStarSuffixesOrdered = c(uStarSuffixes[iKeep], uStarSuffixes[-iKeep])
  resScen <- setNames(rev(lapply(rev(uStarSuffixesOrdered), function(suffix){
    FUN(..., suffix = suffix)
  })), uStarSuffixesOrdered)
}
sEddyProc$methods(sApplyUStarScen =
                    sEddyProc_sApplyUStarScen)

