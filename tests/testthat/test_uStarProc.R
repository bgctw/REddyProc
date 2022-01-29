.tmp.f <- function(){
  Sys.setenv(NOT_CRAN = "true")
  require(testthat)
}
context("sEddyProc-Class")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Example is accessible if package is installed, otherwise need to load it from
# data directory below package root
if (!exists("Example_DETha98")) load("data/Example_DETha98.RData")
EddyData.F <- filterLongRuns(Example_DETha98, "NEE")

#Include POSIX time stamp column
EddyDataWithPosix.F <- suppressMessages(fConvertTimeToPosix(
  EddyData.F, 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour'))
# construct multiyear dataset
EddyData99.F <- EddyData.F
EddyData99.F$Year <- 1999
EddyDataWithPosix2yr.F <- suppressMessages(fConvertTimeToPosix(
  rbind(EddyData.F, EddyData99.F)
  , 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour'))
rm( EddyData99.F )

test_that("UStarProcessing",{
  skip_on_cran()
  EProc <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD', 'Ustar'))
  EProc$sEstimateUstarScenarios(
    nSample = 30L, probs = c(0.1, 0.5, 0.9))
  uStarScen <- EProc$sGetUstarScenarios()
  expect_equal( colnames(uStarScen), c("season", "uStar", "U10", "U50", "U90"))
  # omit the uStar scenario
  EProc$sSetUstarScenarios(uStarScen[-2])
  uStarScen <- EProc$sGetUstarScenarios()
  expect_equal( colnames(uStarScen), c("season", "U10", "U50", "U90"))
  # go on with processing without the need to specify scenarios again
  EProc$sMDSGapFillUStarScens("NEE")
  dsFilled <- EProc$sExportResults()
  expect_true(all(
    c("NEE_U10_f","NEE_U50_f","NEE_U90_f") %in% colnames(dsFilled)))
  expect_true(all(
    c("NEE_U10_fqc","NEE_U50_fqc","NEE_U90_fqc") %in% colnames(dsFilled)))
  expect_true(!any(c("NEE_f","NEE_uStar_f") %in% colnames(dsFilled)))
  # MR flux partitioning
  EProc$sSetLocationInfo(LatDeg = 51.0, LongDeg = 13.6, TimeZoneHour = 1)
  EProc$sMDSGapFill('Tair', FillAll = FALSE)
  EProc$sMDSGapFill('VPD', FillAll = FALSE)
  #EProc$sApplyUStarScen( EProc$sMRFluxPartition )
  EProc$sMRFluxPartitionUStarScens()
  dsFilled <- EProc$sExportResults()
  expect_true(all(
    c("GPP_U10_f","GPP_U50_f","GPP_U90_f") %in% colnames(dsFilled)))
  expect_true(all(
    c("GPP_U10_fqc","GPP_U50_fqc","GPP_U90_fqc") %in% colnames(dsFilled)))
  expect_true(!any(c("GPP_f","GPP_uStar_f") %in% colnames(dsFilled)))
})

test_that("sApplyUStarScen",{
  skip_on_cran()
  EProc <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD', 'Ustar'))
  EProc$sEstimateUstarScenarios(
    nSample = 30L, probs = c(0.1, 0.5, 0.9))
  uStarScen <- EProc$sGetUstarScenarios()
  scenKept <- scenFirst <- colnames(uStarScen)[2]
  #
  # by default suffix of first scenario is kept
  res = unlist(EProc$sApplyUStarScen(function(suffix){EProc$sTEMP$suffix <- suffix}))
  expect_equal(scenKept, unname(res[1]))
  expect_equal(scenKept, EProc$sTEMP$suffix[1])
  #
  # specify different one
  scenKept <- colnames(uStarScen)[3]
  res = unlist(EProc$sApplyUStarScen(
    function(suffix){EProc$sTEMP$suffix <- suffix}
    , uStarScenKeep = scenKept
    ))
  expect_equal(scenKept, unname(res[1]))
  expect_equal(scenKept, EProc$sTEMP$suffix[1])
})


