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
  ans <- ans_Reichstein05 <- EP$sExportResults()
  # Regression test of results
  #Equal to 53 with old MR PV-Wave congruent settings
  expect_that(ans[1,'NEE_fnum'], equals(54))
  #Equal to 96 with old MR PV-Wave congruent settings
  expect_that(ans[1,'Tair_fnum'], equals(173))
  expect_equal(ans[3,'NEE_f'], 1.006569, tolerance = 1e-6)
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

test_that("calculate_gapstats_Vekuri23",{
  expect_warning(
    expect_equal(REddyProc:::calculate_gapstats_Vekuri23(c(),c()>0,FALSE), c(NA, 0, NA))
  )
  # only one value -> sd = NA
  expect_equal(REddyProc:::calculate_gapstats_Vekuri23(1,TRUE,FALSE), c(1, 1, NA))
  expect_equal(REddyProc:::calculate_gapstats_Vekuri23(1,FALSE,FALSE), c(1, 1, NA))
  # only one value in either lower or upper -> sd from mean of two numbers
  expect_equal(REddyProc:::calculate_gapstats_Vekuri23(
    c(1,2),c(TRUE,FALSE),FALSE), c(1.5, 2, sd(1:2)))
  expect_equal(REddyProc:::calculate_gapstats_Vekuri23(
    #c(1,1,2),c(TRUE,TRUE,FALSE),FALSE), c(1.5, 3, sd(c(1,2))))
    c(1,1,2),c(TRUE,TRUE,FALSE),FALSE), c(1.5, 3, sd(c(1,1,2))))
  expect_equal(REddyProc:::calculate_gapstats_Vekuri23(
    # c(1,3,4),c(TRUE,TRUE,FALSE),FALSE), c(3, 3, sd(c(2,4))))
    c(1,3,4),c(TRUE,TRUE,FALSE),FALSE), c(3, 3, sd(c(1,3,4))))
  expect_equal(REddyProc:::calculate_gapstats_Vekuri23(
    #c(1,2,4),c(TRUE,FALSE,FALSE),FALSE), c(2, 3, sd(c(1,3))))
    c(1,2,4),c(TRUE,FALSE,FALSE),FALSE), c(2, 3, sd(c(1,2,4))))
  # error propagation of two sd estimates
  expect_equal(REddyProc:::calculate_gapstats_Vekuri23(
    c(2,4,6,8),c(TRUE,TRUE,FALSE,FALSE),FALSE),
    #c(5, 4, sqrt(var(c(2,4))+var(c(6,8)))/2), tolerance=1e-6 )
    c(5, 4, sd(c(2,4,6,8))), tolerance=1e-6 )
})

test_that("sMDSGapFill Vekuri23",{
  skip_on_cran()
  EddyDataWithPosix2 <- cbind(EddyDataWithPosix, QF = c(1,0,1,0,1,0,0,0,0,0))
  EP <- sEddyProc$new(
    'DE-Tha', EddyDataWithPosix2[1:(48*3*30),], c('NEE','Rg', 'Tair', 'VPD', 'QF'))
  EP$sMDSGapFill('NEE', isVerbose = FALSE, method="Vekuri23")
  EP$sMDSGapFill(
    'Tair','QF', 0, isVerbose = FALSE, minNWarnRunLength = NA, method="Vekuri23")
  expect_error( #Unknown method
    EP$sMDSGapFill('NEE', isVerbose = FALSE, method="unknownLUTMethod")
  )
  ans <- ans_Vekuri23 <- EP$sExportResults()
  # Regression test of results
  expect_that(ans[1,'NEE_fnum'], equals(54))
  expect_that(ans[1,'Tair_fnum'], equals(173))
  # the following differs from Reichstein gapfilling method, regression to prev.
  expect_equal(ans[19,'NEE_f'], -1.857619, tolerance = 1e-6)
})

tmp.f <- function(){
  #library(dplyr) # filter
  #library(ggplot2)
  ans_Vekuri23$t <- ans_Reichstein05$t <- EddyDataWithPosix2$DateTime[1:nrow(ans_Reichstein05)]
  ans_Vekuri23$Rg <- ans_Reichstein05$Rg <- EddyDataWithPosix2$Rg[1:nrow(ans_Reichstein05)]
  plot(NEE_orig~t, data=ans_Reichstein05, col="gray")
  points(NEE_f~t, data=filter(ans_Reichstein05, NEE_fqc > 0), col="blue")
  points(NEE_f~t, data=filter(ans_Vekuri23, NEE_fqc > 0), col="orange", pch="+")
  abline(h=0)
  df <- bind_rows(cbind(method="Vekuri23", ans_Vekuri23),
                  cbind(method="Reichstein05",ans_Reichstein05))
  df <- cbind(method="Vekuri23", ans_Vekuri23)
  filter(df, Rg >=10 ) %>% ggplot(aes(Rg, NEE_f, color=method)) + geom_point(shape = 1)
  ans_Vekuri23$d_NEE <- ans_Vekuri23$NEE_f - ans_Reichstein05$NEE_f
  filter(ans_Vekuri23, Rg > 10) %>% ggplot(aes(t, d_NEE)) + geom_point()
}




sMDSGapFill_user = function(
    var_tofill
    , QFVar =  'none'
    , QFValue = NA_real_
    , FillAll = TRUE
    , isVerbose =  TRUE
    , suffix = ''
    , minNWarnRunLength = NA_integer_
) {
  # initialized one output column
  var_f = paste0(var_tofill,"_uStar_f")
  .self$sTEMP[[var_f]] <- .self$sDATA[[var_tofill]]
  # set bad quality (not apssing uStarTrheshold) to NA
  .self$sTEMP[[var_f]][.self$sTEMP[[QFVar]] != QFValue] <- NA
  # simulate gapfilling by setting all gaps to zero
  .self$sTEMP[[var_f]][is.na(.self$sTEMP[[var_f]])] <- 0.0
}
# create a derived class and override sMDSGapFill
sEddyProcGapfill <- setRefClass("sEddyProcGapfill", contains = "sEddyProc", inheritPackage=TRUE)
sEddyProcGapfill$methods(sMDSGapFill = sMDSGapFill_user)

test_that("sEddyProc_sMDSGapFillAfterUstar with user gafilling function",{
  uStarTh <- 0.15
  data <- EddyDataWithPosix[1:(48*3*30),]
  # note, using the derived class
  EP <- sEddyProcGapfill$new('DE-Tha', data, c('NEE','Rg', 'Tair', 'VPD', 'Ustar'))
  EP$sMDSGapFillAfterUstar("NEE", uStarTh = uStarTh, isFilterDayTime = TRUE)
  ans <- EP$sExportResults()
  expect_true(all(ans$NEE_uStar_f[is.na(data$NEE)] == 0.0))
  expect_true(all(ans$NEE_uStar_f[data$Ustar < uStarTh] == 0.0))
  expect_true(all(ans$NEE_uStar_f[!is.na(data$NEE) & data$NEE != 0.0 & data$Ustar > uStarTh] != 0.0))
})





