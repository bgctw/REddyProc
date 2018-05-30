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
  EddyData.F, 'YDH', Year.s = 'Year', Day.s = 'DoY', Hour.s = 'Hour'))
# construct multiyear dataset
EddyData99.F <- EddyData.F
EddyData99.F$Year <- 1999
EddyDataWithPosix2yr.F <- suppressMessages(fConvertTimeToPosix(rbind(EddyData.F, EddyData99.F), 'YDH', Year.s = 'Year', Day.s = 'DoY', Hour.s = 'Hour'))
rm( EddyData99.F )

test_that("UStarProcessing",{
  skip("TODO")
  EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD', 'Ustar'))
  uStarTh <- EddyProc.C$sEstUstarThresholdDistribution(
    nSample = 100L, probs = c(0.05, 0.5, 0.95))
  EddyProc.C$sSetUstarScenarios(usGetAnnualSeasonUStarMap(uStarTh))
})

