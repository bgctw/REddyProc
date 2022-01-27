#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for gapfilling functions +++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM, TW
#require(testthat)
context("gapfilling")
# Furher context: fCheckColNames, fCheckColNumeric, fCheckOutsideRange

if (!exists("Example_DETha98")) load("data/Example_DETha98.RData")
EddyData <- Example_DETha98 %>% filterLongRuns("NEE")
#Include POSIX time stamp column
EddyDataWithPosix <- suppressMessages(fConvertTimeToPosix(
  EddyData, 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour'))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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


test_that("filterLongRunsInVector",{
  x <- rnorm(10)
  x[5:9] <- 7/8
  ans <- filterLongRunsInVector(x,2)
  expect_true(all(is.na(ans[5:9])))
  expect_equal(ans[-(5:9)], x[-(5:9)])
  # no long runs:
  ans <- filterLongRunsInVector(x, minNRunLength = 80)
  expect_equal(ans, x)
  #
  x <- rep(6:10, 1:5)
  ans <- filterLongRunsInVector(x,2)
  expect_equal( ans[1], x[1])
  expect_true(all(is.na(ans[-1])))
  #
  x <- rep(6:10, 1:5)
  x[2] <- NA # two NAs is not a run
  x[13] <- NA # two NAs is not a run
  x[15] <- 11
  ans <- filterLongRunsInVector(x, 2, na.rm = FALSE)
  expect_equal(length(x), length(ans))
  expect_equal( ans[c(1,3,14,15)], x[c(1,3,14,15)])
  expect_true(all(is.na(ans[-c(1,3,14,15)])))
  ans <- filterLongRunsInVector(x, 2, na.rm = TRUE)
  # now the 10 at index 14 is within the run containing NA
  expect_equal(length(x), length(ans))
  expect_equal( ans[c(1,3,15)], x[c(1,3,15)])
  expect_true(all(is.na(ans[-c(1,3,15)])))
})

test_that("filterLongRuns",{
  y <- rep(6:10, 1:5)
  x <- rnorm(length(y))
  x[5:9] <- 7/8
  data <- data.frame(x,y)
  colNames <- c("x","y")
  ans <- filterLongRuns(data, colNames, minNRunLength = 2)
  expect_true(all(is.na(ans$x[5:9])))
  expect_equal(ans$x[-(5:9)], x[-(5:9)])
  expect_equal( ans$y[1], y[1])
  expect_true(all(is.na(ans$y[-1])))
})


test_that("sMDSGapFill",{
  skip_on_cran()
  EddyDataWithPosix2 <- cbind(EddyDataWithPosix, QF = c(1,0,1,0,1,0,0,0,0,0))
  #EddyDataWithPosix2$NEE[1397+(0:10)] <- 0.965
  EP <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix2[1:(48*3*30),], c('NEE','Rg', 'Tair', 'VPD', 'QF'))
  expect_error( #Not existing variable
    EP$sMDSGapFill('fee','QF', 0, isVerbose = FALSE)
  )
  EP <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix2[1:(48*3*30),], c('NEE','Rg', 'Tair', 'VPD', 'QF'))
  expect_warning( #Empty variable to fill
    EP$sMDSGapFill('Rg','QF', 100 , isVerbose = FALSE)
  )
  EP <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix2[1:(48*3*30),], c('NEE','Rg', 'Tair', 'VPD', 'QF'))
  EP$sMDSGapFill('NEE', isVerbose = FALSE)
  EP$sMDSGapFill('Tair','QF', 0, isVerbose = FALSE, minNWarnRunLength = NA)
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
  EPHour$sMDSGapFill('Tair','QF', 0, isVerbose = FALSE, minNWarnRunLength = NA)
  ans <- EPHour$sExportResults()
  #Equal to 68 with old MR PV-Wave congruent settings
  expect_that(ans[1,'Tair_fnum'], equals(124))
})

test_that("sMDSGapFill runs of equal values",{
  EddyDataWithPosix2 <- cbind(EddyDataWithPosix, QF = c(1,0,1,0,1,0,0,0,0,0))
  EddyDataWithPosix2$NEE[1397 + (0:24)] <- 0.965
  EP <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix2[1:(48*3*30),], c('NEE','Rg', 'Tair', 'VPD', 'QF'))
  expect_warning(
    EP$sMDSGapFill('NEE', isVerbose = FALSE)
  )
  ans <- EP$sExportResults()
  # Regression test of results
  #Equal to 53 with old MR PV-Wave congruent settings
  expect_that(ans[1,'NEE_fnum'], equals(54))
})





