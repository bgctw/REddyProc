.tmp.f <- function(){
  library(testthat)
  library(dplyr)
  library(purrr)
  #library(tidyselect)
}
context("FileHandlingFormats")

test_that("extract_FN15",{
  ds <- Example_DETha98 %>%
    filterLongRuns("NEE") %>%
    fConvertTimeToPosix('YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour')
  EProc <- sEddyProc$new("DE-Tha", ds, c('NEE','Rg','Tair','VPD', 'Ustar'))
  ds_fn15 <- extract_FN15(EProc)
  expect_equal(nrow(ds_fn15), nrow(EProc$sExportData()))
  expect_equal(nrow(ds_fn15), nrow(EProc$sExportData()))
  expect_true( all(c("NEE_ORIG", "SW_IN", "TA",
                     "USTAR", "VPD") %in% names(ds_fn15) ))
})

test_that("extract_FN15_QC",{
  skip("testing exporting (after filling) QC of several uStar thresholds takes too long")
  EddyDataWithPosix <- fConvertTimeToPosix(
    Example_DETha98, 'YDH',Year = 'Year',Day = 'DoY', Hour = 'Hour') %>%
    filterLongRuns("NEE")
  EProc <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix, c('NEE','Rg','Tair','VPD', 'Ustar'))
  EProc$sSetUStarSeasons()
  EProc$sSetUstarScenarios(data.frame(season = "all", U50 = 0.2, U75 = 0.25))
  #EProc$sGetUstarScenarios()
  EProc$sMDSGapFillUStarScens("NEE")
  ds_fn15 <- extract_FN15(EProc)
  expect_true( all(c("NEE_VUT_USTAR50_QC", "NEE_VUT_USTAR75_QC") %in% names(ds_fn15) ))
})

test_that("extract_Rg_VPD_Tair",{
  EddyDataWithPosix <- fConvertTimeToPosix(
    Example_DETha98, 'YDH',Year = 'Year',Day = 'DoY', Hour = 'Hour') %>%
    filterLongRuns("NEE")
  EProc <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix, c('NEE','Rg','Tair','VPD', 'Ustar'))
  EProc$sMDSGapFill(c("Rg"), FillAll = FALSE)
  EProc$sMDSGapFill(c("Tair"), FillAll = FALSE)
  EProc$sMDSGapFill(c("VPD"), FillAll = FALSE)
  EProc$sSetLocationInfo(LatDeg = 51, LongDeg = 13, TimeZoneHour = -1)
  EProc$sCalcPotRadiation()
  ds_fn15 <- extract_FN15(EProc)
  expect_true( all(c("SW_IN_F_MDS", "SW_IN_F_MDS_QC", "TA_F_MDS","TA_F_MDS_QC",
                     "VPD_F_MDS","VPD_F_MDS_QC","SW_IN_POT") %in% names(ds_fn15) ))
  expect_true( !any(c("SW_IN_F_MDSnum") %in% names(ds_fn15) ))
})

test_that("read_from_fluxnet15",{
  ds <- Example_DETha98 %>%
    filterLongRuns("NEE") %>%
    fConvertTimeToPosix('YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour')
  EProc <- sEddyProc$new("DE-Tha", ds, c('NEE','Rg','Tair','VPD', 'Ustar'))
  ds_fn15 <- extract_FN15(EProc)
  ds_fn15$season <- factor("199801")
  ds_eproc <- read_from_fluxnet15(rename(ds_fn15, NEE = "NEE_ORIG"))
	expect_true(
	  all(c('DateTime','NEE','Rg','Tair','VPD', 'Ustar') %in% names(ds_eproc)) )
	#
	fname <- tempfile()
	write_csv(mutate(ds_fn15, season = factor(199801), NEE = .data$NEE_ORIG),
	          fname, na = "-9999")
	on.exit(unlink(fname)) # in case the test fails clean up
	ds_eproc <- fLoadFluxnet15(fname, "season")
	expect_true(
	  all(c('DateTime','NEE','Rg','Tair','VPD', 'Ustar','season') %in% names(ds_eproc)) )
	fname <- tempfile()
	write_csv(mutate(ds_fn15, season = factor(199801)), fname, na = "-9999")
	on.exit(unlink(fname)) # in case the test fails clean up
	ds_eproc <- fLoadFluxnet15(fname, colname_NEE = "NEE_ORIG",
	                           additional_columns = cols(season = col_factor()))
	expect_true(
	  all(c('DateTime','NEE','Rg','Tair','VPD', 'Ustar','season') %in% names(ds_eproc)) )
	# test creating REddyProc class with default names
	EProc <- sEddyProc$new("DE-Tha", ds)
})

