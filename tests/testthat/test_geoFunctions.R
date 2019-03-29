#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for fConvertTimeToPosix functions +++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM, TW
#require(testthat)
context("geoFunctions")
# Furher context: fCheckColNames, fCheckColNumeric, fCheckOutsideRange


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
test_that("fConvertVisibleWm2toPhotons",{
  x <- 1
  ans <- fConvertVisibleWm2toPhotons(x)
  # regression test
  expect_equal(as.numeric(ans), 4.6)
  if (requireNamespace("bigleaf")) {
    ans2 <- bigleaf::Rg.to.PPFD(x, frac_PAR = 1)
    expect_equal(ans2, as.numeric(ans))
  }
})

test_that("fCalcVPDfromRHandTair",{
  rH <- 0.6
  Tair <- seq(0,30, by = 5)
  ans <- fCalcVPDfromRHandTair(rH*100, Tair)
  # regression test
  ans0 <- structure(c(
    2.44312, 3.49158018917769, 4.9175430846641, 6.83133500819716,
    9.36799160283133, 12.6910034282132, 16.9963020454789)
    , varnames = "VPD", units = "hPa")
  expect_equal(ans, ans0)
  if (requireNamespace("bigleaf")) {
    #ans2 <- bigleaf::rH.to.VPD(rH, Tair, 'Alduchov_1996') # in kPa instead of hPa
    ans2 <- bigleaf::rH.to.VPD(rH, Tair, 'Allen_1998') # in kPa instead of hPa
    expect_equal(ans2*10, as.numeric(ans0), tolerance = 1e-2)
  }
})

test_that("fCalcETfromLE",{
  LE <- c(200)
  Tair <- seq(0,30, by = 5)
  ans <- fCalcETfromLE(LE, Tair)
  # regression test
  ans0 <- structure(c(
    4.43872184696281, 4.45976351408243, 4.48100562656575,
    4.50245106231911, 4.52410275460673, 4.54596369338821, 4.56803692669492
  )
  , varnames = "ET", units = "mmol_m-2_s-1")
  expect_equal(ans, ans0)
  if (requireNamespace("bigleaf")) {
    ans2 <- bigleaf::LE.to.ET(LE, Tair) #kg/m2/s
    ans2mmol <- bigleaf::kg.to.mol(ans2) * 1000
    expect_equal(ans2mmol, as.numeric(ans0), tolerance = 1e-3)
  }
})


test_that("Regression test fCalcPotRadiation",{
  hour <- seq(8,16, by = 1)
  potRadSolar <- fCalcPotRadiation(160, hour, 39.94, -5.77, TimeZone = +1)
  expect_warning(potRadLocal <- fCalcPotRadiation(
    160, hour, 39.94, -5.77, TimeZone = +1, useSolartime.b = FALSE))
  expSolar <- structure(c(
    484.152670743821, 717.876981534078, 925.130678985721,
    1091.78976612035, 1206.4967015669, 1261.43439723686, 1252.85893995917,
    1181.35473297567, 1051.79466982602), varnames = "PotRad", units = "W_m-2")
  expLocal <- structure(c(
    797.589402859243, 991.498827921097, 1140.29076299454,
    1233.82528359462, 1265.72816671554, 1233.82528359462, 1140.29076299454,
    991.498827921097, 797.589402859243), varnames = "PotRad", units = "W_m-2")
  expect_that( potRadSolar, equals(expSolar) )
  expect_that( potRadLocal, equals(expLocal) )
})



test_that("fCalcPotRadiation: warn on non-matching day-hour length",{
  hour <- seq(8,16, by = 0.1)
  expect_warning( potRadSolar <- fCalcPotRadiation(
    160:161, hour, 39.94, -5.77, TimeZone = +1) )
})

test_that("fCalcVPDfromRHandTair",{
  T <- 32.2
  relHum <- 63.8
  VPD <- fCalcVPDfromRHandTair(relHum, T)
  expectedVPD <- 17.43059
  expect_equal( as.vector(VPD), expectedVPD, tolerance = 1.e-4 )
})



