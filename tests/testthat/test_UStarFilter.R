#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for fConvertTimeToPosix functions +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: TW
#require(testthat)
context("UStarFilter")
# Furher context: fCheckColNames, fCheckColNumeric, fCheckOutsideRange

if (!exists(".binUstar")) .binUstar <- REddyProc:::.binUstar

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Example is accessible if package is installed, otherwise need to load it from
#  data directory below package root
tmp <- Example_DETha98
#if (!exists("Example_DETha98")) load("data/Example_DETha98.RData")
EddyData.F <- Example_DETha98

EddyDataWithPosix.F <- ds <- suppressMessages(
  fConvertTimeToPosix(
    EddyData.F, 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour'))
dss <- subset(EddyDataWithPosix.F, DoY >= 150 & DoY <= 250)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that(".binUstar classes are correct",{
	res <- .binUstar(dss$NEE, dss$Ustar)$binAverages
  UstarClasses <- usControlUstarSubsetting()$UstarClasses
  Ust_bin_size <- round(nrow(dss)/UstarClasses)
  # the following is based on regression
  expect_true( all(abs(res$nRec[-nrow(res)] - Ust_bin_size) < 60) )
  # create df of NEE and UStar ordered by Ustar
  ds.f <- data.frame(NEE = dss$NEE,Ustar = dss$Ustar)
  ds.f <- dplyr::arrange(ds.f,ds.f[,2])
  # create bin id column, use the one from result
  ds.f$bin <- rep(1:nrow(res), times = res$nRec)
  # do the averaging
  tmp1 <- do.call( rbind ,ds.f %>% split(.$bin) %>% purrr::map( function(dsBin){
    c(Ust_avg = mean(dsBin[,2], na.rm = TRUE),NEE_avg = mean(dsBin[,1], na.rm = TRUE))}))
  resm <- as.matrix(res[,1:2])
  rownames(tmp1) <- rownames(resm)
  expect_equal( tmp1, resm, tolerance = 0.01)
})

test_that("usEstUstarThresholdSingleFw2Binned",{
			# regression test
			Ust_bins.f <- structure(list(Ust_avg = c(0.276666666666667, 0.326666666666667,
									0.341111111111111, 0.363333333333333, 0.384444444444444, 0.401111111111111,
									0.417777777777778, 0.434444444444444, 0.462222222222222, 0.495555555555556,
									0.521111111111111, 0.553333333333333, 0.587777777777778, 0.602222222222222,
									0.624444444444444, 0.654444444444444, 0.693333333333333, 0.77,
									0.936666666666667, 2.11), NEE_avg = c(1.105, 1.26888888888889,
									1.22444444444444, 0.908888888888889, 0.888888888888889, 1.13333333333333,
									1.85888888888889, 1.46777777777778, 2.30222222222222, 1.95, 0.948888888888889,
									1.09888888888889, 2.36666666666667, 0.985555555555556, 1.30777777777778,
									1.63555555555556, 1.75777777777778, 1.84222222222222, 1.59, -0.316666666666667
							)), .Names = c("Ust_avg", "NEE_avg"), row.names = c(NA, -20L), class = "data.frame")
			ctrlUstarEst <- list(ustPlateauFwd = 10, ustPlateauBack = 6, plateauCrit = 0.95,
							corrCheck = 0.5)
					#?usEstUstarThresholdSingleFw2Binned
			#trace(usEstUstarThresholdSingleFw2Binned, recover)	# untrace(usEstUstarThresholdSingleFw2Binned)
			UstarEst <- REddyProc:::usEstUstarThresholdSingleFw2Binned(
			  Ust_bins.f, ctrlUstarEst = ctrlUstarEst )
			#plot( NEE_avg ~ Ust_avg , Ust_bins.f);	abline(v = UstarEst)
			expect_equal( UstarEst, 0.46, tolerance = 0.01 )
		})


test_that("sEddyProc_sSetUStarSeasons",{
  eddyC <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
  eddyC$sSetUStarSeasons()
  expect_true(length(eddyC$sTEMP$season) > 0)
})

test_that("sEstUstarThold: standard case",{
			eddyC <- sEddyProc$new(
			  'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
			(res <- eddyC$sEstUstarThold())
			# regresssion test: 0.42 by former run
			expect_equal( res$uStar[1], 0.42, tolerance = 0.01, scale = 1 )
			expect_equal( dim(eddyC$sUSTAR_DETAILS$tempInSeason)
				, c( usControlUstarSubsetting()$taClasses
				     , length(unique(usCreateSeasonFactorMonth(eddyC$sDATA$sDateTime))) ))
			#REddyProc:::.plotNEEVersusUStarTempClass(
			#subset(eddyC$sDATA, season == "1998001" & tempBin == 5 & is.finite(NEE)), uStarTh = 0.65)
		})

test_that("sEstUstarThold: missing Ustar column",{
  eddyC <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','VPD'))
  expect_error(
    res <- eddyC$sEstUstarThold()
    ,"Ustar"
  )
})

test_that("sEstUstarThold: changing to FW1",{
			eddyC <- sEddyProc$new(
			  'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
			(res <- eddyC$sEstUstarThold(
			  fEstimateUStarBinned = usEstUstarThresholdSingleFw1Binned))
			# regresssion test: 0.42 by former run
			expect_equal( res$uStar[1], 0.34, tolerance = 0.01, scale = 1 )
			expect_equal( dim(eddyC$sUSTAR_DETAILS$tempInSeason)
					, c( usControlUstarSubsetting()$taClasses
					     , length(unique(usCreateSeasonFactorMonth(eddyC$sDATA$sDateTime))) ))
		})

test_that("sEstUstarThold: different seasons",{
			eddyC <- sEddyProc$new(
			  'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
			seasonFactor <- usCreateSeasonFactorYdayYear(
			  eddyC$sDATA$sDateTime, starts = data.frame(
							startyday = c(30,300,45,280),startyear = c(1998,1998,1999,1999) ))
			expect_warning(	# on too few records
				resUStar <- eddyC$sEstUstarThold(seasonFactor = seasonFactor )
			)
			expect_equal( levels(seasonFactor), levels(eddyC$sTEMP$season))
		})

test_that("sEstUstarThold: using ChangePointDetection",{
			eddyC <- sEddyProc$new(
			  'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
			resUStar <- try(eddyC$sEstUstarThold(
								#ctrlUstarEst = usControlUstarEst(isUsingCPT = TRUE)
								ctrlUstarEst = usControlUstarEst(isUsingCPTSeveralT  = TRUE)
			), silent = TRUE)
			if (!requireNamespace("segmented", quietly = TRUE)) {
        expect_true(inherits(resUStar,"try-error"))
			} else {
			  # CPT does no binning uStar
			  details <- eddyC$sUSTAR_DETAILS
			  expect_equal( c(0L), as.vector(na.omit(unique(details$bins$uStarBin))))
			  expect_true( all(is.finite(resUStar$uStar)))
			}
		})

test_that("sEstUstarThold distribution: using ChangePointDetection ",{
  skip_on_cran()
  eddyC <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
  eddyC$sEstimateUstarScenarios(
    nSample = 3L, probs = c(0.05, 0.5, 0.95),
    ctrlUstarEst = usControlUstarEst(isUsingCPTSeveralT = TRUE)
  )
  if (!requireNamespace("segmented", quietly = TRUE)) {
    expect_true(inherits(resUStar,"try-error"))
  } else {
    uStarDist <- eddyC$sGetEstimatedUstarThresholdDistribution()
    expect_true( all(is.finite(as.matrix(uStarDist[4:7]))))
    expect_true( all(uStarDist[["5%"]] < uStarDist[["95%"]]))
  }
})

test_that("sEstUstarThold: multi-year and One-big-season",{
  skip_on_cran()
  EddyData.F99 <- EddyData.F
	EddyData.F99$Year <- EddyData.F$Year + 1
	EddyDataWithPosix.F99 <- fConvertTimeToPosix(
	  EddyData.F99, 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour')
	dsAll <- EddyDataWithPosix.F # rbind(EddyDataWithPosix.F, EddyDataWithPosix.F99)
	# construct in a way so that that in each seasons there are not enough
	# valid values in 98
	nRec <- max(usControlUstarSubsetting()$minRecordsWithinSeason
	            , usControlUstarSubsetting()$taClasses *
	              usControlUstarSubsetting()$minRecordsWithinTemp) - 50
	dsAll$seasonFactor <- usCreateSeasonFactorMonthWithinYear(dsAll$DateTime)
	dsFew <- dsAll %>% split(.$seasonFactor) %>% purrr::map_df(function(dss){
		isValid <- REddyProc:::usGetValidUstarIndices(dss)
		if (sum(isValid) >= nRec)
			dss$NEE[isValid][(nRec):sum(isValid)] <- NA
		#print(dss$seasonFactor[1])
		#print(nrow(dss))
		dss
	})
	dsFew$seasonFactor <- NULL
	dsFew <- dplyr:::arrange(dsFew, DateTime)
	dsComb <- rbind(dsFew,EddyDataWithPosix.F99)
	eddyC <- sEddyProc$new(
	  'DE-Tha', dsComb, c('NEE','Rg','Tair','VPD','Ustar'))
	expect_warning(
	res <- eddyC$sEstUstarThold(
						seasonFactor =
						  usCreateSeasonFactorMonthWithinYear(eddyC$sDATA$sDateTime)
						))
	expect_true(
	  all(eddyC$sUSTAR_DETAILS$seasonAggregation$seasonAgg ==
	        eddyC$sUSTAR_DETAILS$seasonAggregation$seasonAgg[1] ))
	# regresssion test: 0.42 by former run
	expect_equal( res$uStar[1], 0.43, tolerance = 0.01, scale = 1 )
	res98 <- subset(eddyC$sUSTAR_DETAILS$seasonYear, seasonYear == 1998)
	# result for 98 taken from pooled, i.e. from 99
	expect_equal(
	  res98$uStarAggr[1], res98$uStarPooled, tolerance = 0.01, scale = 1 )
	res99 <- subset(eddyC$sUSTAR_DETAILS$seasonYear, seasonYear == 1999)
	# regresssion test: 0.42 by former run
	expect_equal(
	  res99$uStarAggr[1], res99$uStarMaxSeason, tolerance = 0.01, scale = 1 )
	expect_equal(eddyC$sDATA$tempBin, res$bins$tempBin)
	#
  .tmp.f <- function(){
    eddyC$sGetUstarScenarios()
    eddyC$sPlotNEEVersusUStarForSeason("1998003")
    str(eddyC$sUSTAR_DETAILS)
  }
	#
})


