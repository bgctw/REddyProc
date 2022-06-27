# Author: TW
#require(testthat)
context("plotFingerPrint")

if (!exists("Example_DETha98")) load("data/Example_DETha98.RData")
EddyData.F <- Example_DETha98

#Include POSIX time stamp column
EddyDataWithPosix.F <- suppressMessages(fConvertTimeToPosix(
  EddyData.F, 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour'))
# construct multiyear dataset
EddyData99.F <- EddyData.F
EddyData99.F$Year <- 1999
EddyDataWithPosix2yr.F <- suppressMessages(fConvertTimeToPosix(rbind(
  EddyData.F, EddyData99.F), 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour'))
rm( EddyData99.F )

EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD'))
data <- cbind( EddyProc.C$sDATA, EddyProc.C$sTEMP)
dts <- EddyProc.C$sINFO$DTS

test_that("plotting NEE with class method",{
  EddyProc.C$sPlotFingerprintY("NEE", Year = 1998)
})

test_that("plotting NEE with different range",{
  EddyProc.C$sPlotFingerprintY(
    "NEE", Year = 1998,
    valueLimits = quantile(EddyProc.C$sDATA$NEE,
                           prob = c( 0.05, 0.99), na.rm = TRUE))
})


test_that("plotting legend only",{
  EddyProc.C$sPlotFingerprintY("NEE", Year = 1998, onlyLegend = TRUE)
})

test_that("plotting NEE",{
  sEddyProc_sPlotFingerprintY("NEE", Year = 1998, data = data, dts = dts)
})

test_that("plotting NEE with Inf-values",{
  data2 <- data
  data2$NEE[5:10][is.finite(data2$NEE[5:10])] <- Inf
  sEddyProc_sPlotFingerprintY("NEE", Year = 1998, data = data2, dts = dts)
})

test_that("plotting NEE to pdf",{
  skip_on_cran()
  EddyProc.C$sPlotFingerprint("NEE", Dir = tempdir())
})

test_that("plot diurnal cycle of NEE to pdf",{
  skip_on_cran()
  EddyProc.C$sPlotDiurnalCycle("NEE", Dir = tempdir())
})

test_that("sPlotHHFluxes",{
  skip_on_cran()
  EddyProc.C$sPlotHHFluxes("NEE", Dir = tempdir())
})

test_that("compute_daily_mean",{
  nday = 5
  nRecInDay = 48
  x0 = 1.2  # mumol CO2 / s
  x0_sd = 0.1*x0
  x = rep(x0, nday*nRecInDay)
  x_sd = rep(x0_sd, nday*nRecInDay)
  # no noise, 
  res = REddyProc:::compute_daily_mean(x, x_sd, nRecInDay, 1, 1)
  expect_equal(res$x, rep(x0,5))
  expect_equal(res$x_sd, rep(x0_sd,5)) # no uncertainty decrease: correlated
  # convert to mumol CO2 per day 
  timeFactor = 3600 * 24
  res = REddyProc:::compute_daily_mean(x, x_sd, nRecInDay, timeFactor, 1)
  expect_equal(res$x, rep(x0,5)*timeFactor)
  expect_equal(res$x_sd, rep(x0_sd,5)*timeFactor) # correlated
  # convert to gCO2 per second: (g CO2/mumol CO2) * (gC/gCO2)
  massFactor =  (44.0096 / 1e6) * (12.011 / 44.0096) 
  ## conversion factor with default from mumol CO2 to g C
  res = REddyProc:::compute_daily_mean(x, x_sd, nRecInDay, 1, massFactor)
  expect_equal(res$x, rep(x0,5)*massFactor)
  expect_equal(res$x_sd, rep(x0_sd,5)*massFactor) # correlated
})

test_that("sPlotDailySums",{
  skip_on_cran()
  df = cbind(EddyProc.C$sDATA, EddyProc.C$sTEMP)
  REddyProc:::sEddyProc_sPlotDailySumsY("NEE", Year=1998, data=df, dts=48)
  #
  EddyProc.C$sPlotDailySums("NEE", Dir = tempdir())
})


