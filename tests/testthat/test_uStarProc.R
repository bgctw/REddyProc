.tmp.f <- function(){
  Sys.setenv(NOT_CRAN = "true")
  require(testthat)
}
context("sEddyProc-Class")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Example is accessible if package is installed, otherwise need to load it from
# data directory below package root
if (!exists("Example_DETha98")) load("data/Example_DETha98.RData")
EddyData.F <- Example_DETha98

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
  EddyProc.C <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD', 'Ustar'))
  EddyProc.C$sEstimateUstarScenarios(
    nSample = 30L, probs = c(0.1, 0.5, 0.9))
  uStarScen <- EddyProc.C$sGetUstarScenarios()
  expect_equal( colnames(uStarScen), c("season", "uStar", "U10", "U50", "U90"))
  # omit the uStar scenario
  EddyProc.C$sSetUstarScenarios(uStarScen[-2])
  uStarScen <- EddyProc.C$sGetUstarScenarios()
  expect_equal( colnames(uStarScen), c("season", "U10", "U50", "U90"))
  # go on with processing without the need to specify scenarios again
  EddyProc.C$sMDSGapFillUStarScens("NEE")
  dsFilled <- EddyProc.C$sExportResults()
  expect_true(all(
    c("NEE_U10_f","NEE_U50_f","NEE_U90_f") %in% colnames(dsFilled)))
  expect_true(all(
    c("NEE_U10_fqc","NEE_U50_fqc","NEE_U90_fqc") %in% colnames(dsFilled)))
  expect_true(!any(c("NEE_f","NEE_uStar_f") %in% colnames(dsFilled)))
  # MR flux partitioning
  EddyProc.C$sSetLocationInfo(LatDeg = 51.0, LongDeg = 13.6, TimeZoneHour = 1)
  EddyProc.C$sMDSGapFill('Tair', FillAll.b = FALSE)
  EddyProc.C$sMDSGapFill('VPD', FillAll.b = FALSE)
  #EddyProc.C$sApplyUStarScen( EddyProc.C$sMRFluxPartition )
  EddyProc.C$sMRFluxPartitionUStarScens()
  dsFilled <- EddyProc.C$sExportResults()
  expect_true(all(
    c("GPP_U10_f","GPP_U50_f","GPP_U90_f") %in% colnames(dsFilled)))
  expect_true(all(
    c("GPP_U10_fqc","GPP_U50_fqc","GPP_U90_fqc") %in% colnames(dsFilled)))
  expect_true(!any(c("GPP_f","GPP_uStar_f") %in% colnames(dsFilled)))
})

