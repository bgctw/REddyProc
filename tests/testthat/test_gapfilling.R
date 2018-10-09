#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for fConvertTimeToPosix functions +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM, TW
#require(testthat)
context("gapfilling")
# Furher context: fCheckColNames, fCheckColNumeric, fCheckOutsideRange

if (!exists("Example_DETha98")) load("data/Example_DETha98.RData")
EddyData <- Example_DETha98
#Include POSIX time stamp column
EddyDataWithPosix <- suppressMessages(fConvertTimeToPosix(
  EddyData, 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour'))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
test_that("runLength",{
  x <- rnorm(10)
  x[5:9] <- 7/8
  ans <- REddyProc:::.runLength(x)
  expect_equal(ans$index, 5)
  expect_equal(ans$nRep, 5)
  #
  x <- rep(6:10, 1:5)
  ans <- REddyProc:::.runLength(x)
  expect_equal( ans$nRep, 2:5)
  expect_equal( x[ans$index], 7:10)
  #
  x <- rep(6:10, 1:5)
  x[13:14] <- NA # two NAs is not a run
  ans <- REddyProc:::.runLength(x)
  expect_equal( ans$nRep, c(2:4,2))
  expect_equal( x[ans$index], 7:10)
  #
  x <- rep(6:10, 1:5)
  x[2] <- NA # two NAs is not a run
  x[13] <- NA # two NAs is not a run
  ans <- REddyProc:::.runLength(x)
  expect_equal( ans$nRep, c(3,4,2,2))
  expect_equal( x[ans$index], c(8:10,10))
  #
  x <- 1:5
  ans <- REddyProc:::.runLength(x)
  expect_equal(nrow(ans), 0)
})

test_that("Test sMDSGapFill",{
  EddyDataWithPosix2 <- cbind(EddyDataWithPosix.F, QF = c(1,0,1,0,1,0,0,0,0,0))
  #EddyDataWithPosix2$NEE[1397+(0:10)] <- 0.965
  EP <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix2[1:(48*3*30),], c('NEE','Rg', 'Tair', 'VPD', 'QF'))
  expect_error( #Not existing variable
    EP$sMDSGapFill('fee','QF', 0, isVerbose = FALSE)
  )
  expect_warning( #Empty variable to fill
    EP$sMDSGapFill('Rg','QF', 100 , isVerbose = FALSE)
  )
  EP$sMDSGapFill('NEE', isVerbose = FALSE)
  EP$sMDSGapFill('Tair','QF', 0, isVerbose = FALSE)
  ans <- EP$sExportResults()
  # Regression test of results
  #Equal to 53 with old MR PV-Wave congruent settings
  expect_that(ans[1,'NEE_fnum'], equals(54))
  #Equal to 96 with old MR PV-Wave congruent settings
  expect_that(ans[1,'Tair_fnum'], equals(173))
  # Shorter version for hourly
  EPHour <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix2[c(F,T),][1:(24*3*30),]
    , c('NEE','Rg', 'Tair', 'VPD', 'QF'), DTS = 24)
  EPHour$sMDSGapFill('Tair','QF', 0, isVerbose = FALSE)
  ans <- EPHour$sExportResults()
  #Equal to 68 with old MR PV-Wave congruent settings
  expect_that(ans[1,'Tair_fnum'], equals(124))
})



