# Author: TW
#require(testthat)
context("plotFingerPrint")

if (!exists("Example_DETha98")) load("data/Example_DETha98.RData")
EddyData.F <- Example_DETha98

#Include POSIX time stamp column
EddyDataWithPosix.F <- suppressMessages(fConvertTimeToPosix(
  EddyData.F, 'YDH', Year.s = 'Year', Day.s = 'DoY', Hour.s = 'Hour'))
# construct multiyear dataset
EddyData99.F <- EddyData.F
EddyData99.F$Year <- 1999
EddyDataWithPosix2yr.F <- suppressMessages(fConvertTimeToPosix(rbind(
  EddyData.F, EddyData99.F), 'YDH', Year.s = 'Year', Day.s = 'DoY', Hour.s = 'Hour'))
rm( EddyData99.F )

EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD'))
data <- cbind( EddyProc.C$sDATA, EddyProc.C$sTEMP)
dts <- EddyProc.C$sINFO$DTS

test_that("plotting NEE with class method",{
  EddyProc.C$sPlotFingerprintY("NEE", Year.i = 1998)
})

test_that("plotting NEE",{
  sEddyProc_sPlotFingerprintY("NEE", Year.i = 1998, data = data, dts = dts)
})

test_that("plotting NEE with Inf-values",{
  data2 <- data
  data2$NEE[5:10][is.finite(data2$NEE[5:10])] <- Inf
  sEddyProc_sPlotFingerprintY("NEE", Year.i = 1998, data = data2, dts = dts)
})
