#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for fConvertTimeToPosix functions +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM, TW
#require(testthat)
context("geoFunctions")
# Furher context: fCheckColNames, fCheckColNumeric, fCheckOutsideRange


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Regression test fCalcPotRadiation",{
			hour <- seq(8,16, by=1)
			potRadSolar <- fCalcPotRadiation(160, hour, 39.94, -5.77, TimeZone=+1)
			expect_warning(potRadLocal <- fCalcPotRadiation(160, hour, 39.94, -5.77, TimeZone=+1, useSolartime.b = FALSE))
		expSolar <- structure(c(484.152670743821, 717.876981534078, 925.130678985721, 
						1091.78976612035, 1206.4967015669, 1261.43439723686, 1252.85893995917, 
						1181.35473297567, 1051.79466982602), varnames = "PotRad", units = "W_m-2")
		expLocal <- structure(c(797.589402859243, 991.498827921097, 1140.29076299454, 
						1233.82528359462, 1265.72816671554, 1233.82528359462, 1140.29076299454, 
						991.498827921097, 797.589402859243), varnames = "PotRad", units = "W_m-2")
	expect_that( potRadSolar, equals(expSolar) )
	expect_that( potRadLocal, equals(expLocal) )
})

test_that("fCalcPotRadiation: warn on non-matching day-hour length",{
			hour <- seq(8,16, by=0.1)
			expect_warning( potRadSolar <- fCalcPotRadiation(160:161, hour, 39.94, -5.77, TimeZone=+1) )
		})

test_that("fCalcVPDfromRHandTair",{
			T <- 32.2
			relHum <- 63.8
			VPD <- fCalcVPDfromRHandTair(relHum, T)
			expectedVPD <- 17.43059
			expect_equal( as.vector(VPD), expectedVPD, tolerance=1.e-4 )
		})



