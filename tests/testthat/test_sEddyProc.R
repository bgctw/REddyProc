#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for sEddyProc functions +++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM
#require(testthat)
context("sEddyProc-Class")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Example is accessible if package is installed, otherwise need to load it from
# data directory below package root
if (!exists("Example_DETha98")) load("data/Example_DETha98.RData")
EddyData.F <- Example_DETha98

#Include POSIX time stamp column
EddyDataWithPosix.F <- suppressMessages(fConvertTimeToPosix(
  EddyData.F, 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour')) %>%
  filterLongRuns("NEE")
# construct multiyear dataset
EddyData99.F <- EddyData.F
EddyData99.F$Year <- 1999
EddyDataWithPosix2yr.F <- suppressMessages(
  fConvertTimeToPosix(rbind(EddyData.F, EddyData99.F)
                      , 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour'))
rm( EddyData99.F )
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Check sEddyProc initialization: POSIX time stamp
test_that("POSIX time stamp: correct format",{
  EddyProc.C <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD'))
  expect_that(as.numeric(EddyProc.C$sDATA$sDateTime[1]), equals(883613700))
})
test_that("POSIX time stamp: missing column",{
  expect_error(
    EddyProc.C <- sEddyProc$new(
      'DE-Tha', EddyData.F, c('NEE','Rg', 'Tair', 'VPD'))
  )
})
test_that("POSIX time stamp: wrong column type",{
  expect_error(
    EddyProc.C <- sEddyProc$new(
      'DE-Tha', EddyData.F, c('NEE','Rg', 'Tair', 'VPD'), 'Year')
  )
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Check sEddyProc initialization: Time series problems
test_that("Invalid number of daily time steps",{
  expect_error(
    EddyProc.C <- sEddyProc$new(
      'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD'), DTS = 12)
  )
})
test_that("Time series not in equidistant steps",{
  expect_error(
    EddyProc.C <- sEddyProc$new(
      'DE-Tha', EddyDataWithPosix.F[c(-50,-60),], c('NEE','Rg', 'Tair', 'VPD'))
  )
  expect_error( #Pseudo hourly by [c(F,T),]
    EddyProcH.C <- sEddyProc$new(
      'DE-Tha', EddyDataWithPosix.F[c(F,T),][c(-50,-60),]
      , c('NEE','Rg', 'Tair', 'VPD'), DTS = 24)
  )
})
test_that("Time series not stamped on the (half-)hour",{
  #Shift half-hourly time stamp
  EddyDataShiftedPosix.F <- EddyDataWithPosix.F
  EddyDataShiftedPosix.F$DateTime <- EddyDataShiftedPosix.F$DateTime - (15 * 60)
  expect_error(
    EddyProc.C <- sEddyProc$new(
      'DE-Tha', EddyDataShiftedPosix.F, c('NEE','Rg', 'Tair', 'VPD'))
  )
  expect_error(
    EddyProc.C <- sEddyProc$new(
      'DE-Tha', EddyDataShiftedPosix.F[c(F,T),]
      , c('NEE','Rg', 'Tair', 'VPD'), DTS = 24)
  )
})
test_that("Time series not in full days and starting at end of first (half-)hour",{
  # (and ending at midnight).
  expect_warning(
    EddyProc.C <- sEddyProc$new(
      'DE-Tha', EddyDataWithPosix.F[1:(nrow(EddyDataWithPosix.F) - 1),]
      , c('NEE','Rg', 'Tair', 'VPD'))
  )
  expect_warning(
    EddyProc.C <- sEddyProc$new(
      'DE-Tha'
      , EddyDataWithPosix.F[c(F,T),][1:(nrow(EddyDataWithPosix.F[c(F,T),]) - 1),]
      , c('NEE','Rg', 'Tair', 'VPD'), DTS = 24)
  )
  expect_warning(
    EddyProc.C <- sEddyProc$new(
      'DE-Tha', EddyDataWithPosix.F[2:(nrow(EddyDataWithPosix.F) - 47),]
      , c('NEE','Rg', 'Tair', 'VPD'))
  )
  expect_warning(
    EddyProc.C <- sEddyProc$new(
      'DE-Tha'
      , EddyDataWithPosix.F[c(F,T),][2:(nrow(EddyDataWithPosix.F[c(F,T),]) - 23),]
      , c('NEE','Rg', 'Tair', 'VPD'), DTS = 24)
  )
})
test_that("Time series less than three month of data",{
  expect_error(
    EddyProc.C <- sEddyProc$new(
      'DE-Tha', EddyDataWithPosix.F[1:(48*(3*30 - 1)),]
      , c('NEE','Rg', 'Tair', 'VPD'))
  )
  expect_error(
    EddyProc.C <- sEddyProc$new(
      'DE-Tha', EddyDataWithPosix.F[c(F,T),][1:(24*(3*30 - 1)),]
      , c('NEE','Rg', 'Tair', 'VPD'), DTS = 24)
  )
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Test sGetData",{
  EddyProc.C <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD'))
  Data.F <- EddyProc.C$sGetData()
  expect_that(Data.F[,1], equals(EddyProc.C$sDATA[,1]))
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# see .profileGapFill in develop/profile/profile.R

test_that("Test sMDSGapFillAfterUStar default case",{
  skip_on_cran()
  EddyProc.C <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD', 'Ustar'))
  uStarTh <- EddyProc.C$sEstUstarThold()
  uStar98 <- subset(
    uStarTh, aggregationMode == "year" & seasonYear == 1998, "uStar" )[1,1]
  #EddyProc.C$trace("sMDSGapFillAfterUstar", recover)
  #EddyProc.C$untrace("sMDSGapFillAfterUstar")
  EddyProc.C$sMDSGapFillAfterUstar(
    'NEE', FillAll.b = FALSE, uStarTh	= uStar98)
  expect_equal(
    uStar98, min(EddyProc.C$sDATA$Ustar[
      EddyProc.C$sTEMP$NEE_uStar_fqc == 0 &
        (EddyProc.C$sDATA$Rg < 10)], na.rm = TRUE), tolerance = 0.05  )
})

test_that("Test sMDSGapFillAfterUStar single value",{
  skip_on_cran()
  EddyProc.C <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD', 'Ustar'))
  uStarFixed <- 0.46
  EddyProc.C$sMDSGapFillAfterUstar(
    'NEE', FillAll.b = FALSE, uStarTh = uStarFixed)
  expect_equal( uStarFixed, min(EddyProc.C$sDATA$Ustar[
    EddyProc.C$sTEMP$NEE_uStar_fqc == 0 & (EddyProc.C$sDATA$Rg < 10)]
    , na.rm = TRUE), tolerance = 0.05  )
})

test_that("Test sMDSGapFillAfterUStar error on season mismatch",{
  EddyProc.C <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD', 'Ustar'))
  uStarTh <- EddyProc.C$sEstUstarThold()
  uStarTh <- usGetAnnualSeasonUStarMap(uStarTh)[-1, ,drop = FALSE]
  expect_error(
    EddyProc.C$sMDSGapFillAfterUstar(
      'NEE', uStarTh = uStarTh, FillAll.b = FALSE)
  )
})

test_that("Test sMDSGapFillAfterUStar error on na-values",{
  EddyProc.C <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD', 'Ustar'))
  uStarTh <- EddyProc.C$sEstUstarThold()
  uStarTh <- usGetAnnualSeasonUStarMap(uStarTh)
  uStarTh[1,2] <- NA
  expect_error(
    EddyProc.C$sMDSGapFillAfterUstar(
      'NEE', uStarTh = uStarTh, FillAll.b = FALSE)
  )
})

test_that("Test sMDSGapFillAfterUStarDistr standard and colnames in FluxPartitioning",{
  skip_on_cran()
  EProc <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
  .tmp.debug <- function(){
    EddySetups.C <- sEddyProc$new(
      'DE-Tha', EddyDataWithPosix.F
      , c('NEE','Rg','Tair','VPD','Ustar'))$import(EddySetups.C)
  }
  # Note that for each period a distribution of estimates is obtained,
  # and quantiles are reported
  #EddySetups.C <- EddySetups.C$sEstUstarThresholdDistribution( nSample = 3L )
  #(uStarRes <- EddySetups.C$sGetEstimatedUstarThresholdDistribution())
  (uStarRes <- EProc$sEstUstarThresholdDistribution( nSample = 3L ))
  (uStarTh <- usGetAnnualSeasonUStarMap(uStarRes))
  #expUStarScen <- usGetAnnualSeasonUStarMap(uStarRes)
  #expect_equal( EddySetups.C$sUSTAR_SCEN, expUStarScen)
  EProc$sMDSGapFillAfterUStarDistr(
    'NEE', uStarTh = uStarTh, FillAll.b = FALSE )
  # Note the columns with differnt suffixes for different uStar
  # estimates (uStar, U05, U50, U95)
  cNames <- grep("U50", colnames(EProc$sExportResults()), value = TRUE)
  expect_true( all(c(
    "Ustar_U50_Thres", "Ustar_U50_fqc", "NEE_U50_orig", "NEE_U50_f",
    "NEE_U50_fqc", "NEE_U50_fall", "NEE_U50_fall_qc", "NEE_U50_fnum",
    "NEE_U50_fsd", "NEE_U50_fmeth", "NEE_U50_fwin")
    %in% cNames) )
  #
  EProc$sMDSGapFill('Tair', FillAll.b = FALSE)
  EProc$sSetLocationInfo(
    LatDeg = 51.0, LongDeg = 13.6, TimeZoneHour = 1)
  # EddySetups.C <- sEddyProc$new(
  #   'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))$import(
  #     EddySetups.C
  #   )
  for (suffix in c('U05', 'U50')) {
    EProc$sMRFluxPartition(suffix = suffix)
  }
  cNames2 <- grep("U50", colnames(EProc$sExportResults()), value = TRUE)
  expect_true( all(			c("PotRad_U50",	"FP_NEEnight_U50", "FP_Temp_U50"
                        , "E_0_U50", "R_ref_U50", "Reco_U50",
                        "GPP_U50_f", "GPP_U50_fqc")
                      %in% cNames2) )
})

test_that("Test sMDSGapFillAfterUStarDistr single quantile",{
  skip_on_cran()
  eddyC <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
  uStarRes <- eddyC$sEstUstarThresholdDistribution(
    nSample = 2L, probs = 0.5
    #,ctrlUstarEst = usControlUstarEst(isUsingCPTSeveralT = TRUE)
    )
  expect_true( all(c("uStar","50%") %in% names(uStarRes)))
  expect_true( all(is.finite(uStarRes$uStar)))
  expect_true( all(is.finite(uStarRes$"50%")))
})


test_that("Test sMDSGapFillAfterUStarDistr single row",{
  skip_on_cran()
  EProc <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
  # Note that for each period a distribution of estimates is obtained,
  # and quantiles are reported
  (uStarRes <- EProc$sEstUstarThresholdDistribution( nSample = 3L ))
  # take only the first row, would throw an error in test on season mismatch,
  # but with one row applied for all
  (uStarTh <- usGetAnnualSeasonUStarMap(uStarRes)[1, c(1,3,4),drop = FALSE])
  EProc$sMDSGapFillAfterUStarDistr(
    'NEE', uStarTh = uStarTh, FillAll = FALSE)
  # Note the columns with differnt suffixes for different uStar
  # estimates (uStar, U05, U50, U95)
  cNames <- grep("U50", colnames(EProc$sExportResults()), value = TRUE)
  expect_true( all(c(
    "Ustar_U50_Thres", "Ustar_U50_fqc", "NEE_U50_orig", "NEE_U50_f",
    "NEE_U50_fqc", "NEE_U50_fall", "NEE_U50_fall_qc", "NEE_U50_fnum",
    "NEE_U50_fsd", "NEE_U50_fmeth", "NEE_U50_fwin")
    %in% cNames) )
})

