#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for sEddyProc functions regarding partioning +++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: TW
#require(testthat)
context("sEddyProc-Class partitioning")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!exists(".binUstar") ) .binUstar <- REddyProc:::.binUstar

# Example is accessible if package is installed, otherwise need to load
# it from data directory below package root
if (!exists("Example_DETha98")) load("data/Example_DETha98.RData")
EddyData.F <- Example_DETha98

#Include POSIX time stamp column
EddyDataWithPosix.F <- suppressMessages(fConvertTimeToPosix(
  EddyData.F, 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour')) %>%
  filterLongRuns(("NEE"))

EddyHour.C <- suppressMessages(sEddyProc$new(
  'DE-Tha', EddyDataWithPosix.F[c(F,T),][1:(24*3*30),]
  , c('NEE','Rg', 'Tair', 'VPD'), DTS = 24))
suppressMessages(EddyHour.C$sMDSGapFill('Tair', isVerbose = F))
suppressMessages(EddyHour.C$sMDSGapFill('NEE', isVerbose = F))


test_that("fOptimSingleE0",{
			Temp_degK.V.n <- structure(c(279.45, 279.25, 278.95, 278.35, 278.35, 278.55, 279.15,
							279.55, 279.95, 279.25, 278.85, 278.65, 277.75, 277.75, 277.35,
							277.55, 276.35, 275.15, 274.65, 276.45, 276.25, 276.25, 276.35,
							276.15, 275.15, 275.35, 275.25, 275.05, 275.15, 275.65, 276.45,
							276.65, 277.25, 277.75, 282.55, 282.05, 282.05, 282.15, 277.25,
							277.85, 278.45, 278.65, 279.15, 279.45, 279.95, 279.45, 279.55,
							280.05, 280.15, 281.15, 281.15, 280.95, 280.95, 281.15, 281.35,
							281.45, 281.15, 281.45, 281.55, 281.55, 281.35, 281.25, 281.35,
							281.55, 282.25, 282.45, 282.55, 282.25, 282.25, 282.25, 282.15,
							281.95, 281.75, 281.55, 281.45, 281.15, 280.65, 280.95, 280.55,
							280.95, 280.55, 280.45, 280.75, 280.85, 280.25, 283.55, 283.25,
							283.55, 283.25, 283.55, 284.45, 284.75, 285.65, 285.05, 284.15,
							282.55, 281.85, 278.05, 280.15, 280.95, 278.95, 278.95, 279.05,
							279.35, 279.25, 278.95, 279.05, 279.35, 278.95, 278.25, 277.55,
							276.85, 276.65, 277.15, 277.15, 277.25, 276.95, 276.85, 276.95,
							277.15, 277.15, 277.05, 276.85, 277.75, 277.75, 277.95, 277.85,
							279.25, 279.35, 279.05, 278.25, 278.35), varnames = "Temp_K", units = "degK")
			NEEnight.V.n <- c(-0.6, 4.21, 0.09, 0.01, 2.24, 3.61, 2.56, 4.06, 1.32, 5.91,
					3, -0.43, 2.82, 0.12, 3.01, -0.27, 1.45, 2.14, -0.99, -0.93,
					1.49, 0.33, -7.02, 4.25, 1.95, 0.6, 2.36, -0.01, 0.74, 0.03,
					1.53, 0.18, 0.11, 3.08, 1.41, 1.29, 1.04, -9.18, -0.14, -15.24,
					1.43, 2.62, -1.14, 1.6, 0.55, -2.15, 1.61, 0.44, 1.96, 0.76,
					1.05, 1.4, 0.88, 2.35, -0.84, 2.45, 0.67, 0.52, 1.07, 2.8, 1.25,
					0.45, -1.91, 2.64, 2.7, 1.39, 2.68, 3.11, 0.29, 1.86, 2.42, 0.44,
					0.94, 2.08, 2.56, 2.95, 0.75, 1.16, 2.86, 1.562, 1.73, -0.13,
					3.76, 1.22, 1.432, 1.09, -3.14, -0.84, 1.97, 2.59, -0.47, -0.85,
					1.3, 1.52, 3.23, 1.01, 0.99, 0.66, 0.16, 0.15, 1.08, 1.09, 0.45,
					0.32, 0.8, 1.01, -0.41, 2.52, 0.36, 0.33, 0.57, -0.06, 0.82,
					8.82, -0.04, -3.96, -0.53, 2.75, 1.45, -0.39, -2.06, 0.03, 0.18,
					-1.21, 2.44, 0.49, 0.05, -0.17, 2.93, 4.77, 1.85, -0.35)
			res <- REddyProc:::fOptimSingleE0(
			  NEEnight.V.n, Temp_degK.V.n, TRef = 273.15 + 15
#			  , recoverOnError = TRUE
			)
			expect_true( is.numeric(res) )
			expect_true( length(res) > 1 )
			expect_true( all(is.finite(res) ))
      #
			if (requireNamespace("minpack.lm", quietly = TRUE)) {
				resL <- REddyProc:::fOptimSingleE0_Lev(
				  NEEnight.V.n, Temp_degK.V.n, TRef = 273.15 + 15, recoverOnError = TRUE)
			}
			expect_true( is.numeric(res) )
			expect_true( length(res) > 1 )
			expect_true( all(is.finite(res) ))
		})

test_that("fRegrE0fromShortTerm",{
			TempVar.V.n <- c(4.4, 4.7, 4.2, 4.8, 4.6, 4.1, 4.3, 4.8, 4.2, 4.9, 5, 4.9, 4.7,
					4.8, 5.2, 5.2, 5.4, 6, 6.4, 6.8, 6.1, 5.7, 5.5, 4.6, 4.6, 4.2,
					4.4, 3.2, 2, 1.5, 1.4, 1, 1.1, 2.1, 2.8, 3.3, 3.3, 3.3, 3.3,
					3.1, 3.1, 3.2, 3, 2, 2.2, 2.1, 1.9, 2, 2.5, 3.3, 3.5, 4.1, 4.6,
					5.1, 6.3, 7.8, 8.3, 8.6, 9.7, 9.8, 9.7, 10, 9.4, 8.9, 8.9, 9,
					4.1, 4.7, 5.3, 5.5, 6, 6.3, 6.8, 6.3, 6.4, 6.9, 7, 7.1, 7.5,
					7.8, 8.2, 8.5, 8, 8.1, 7.7, 8, 8, 8, 7.8, 7.8, 8, 8.2, 8.3, 8,
					8.3, 8.4, 8.4, 8.2, 8.1, 8.2, 8.4, 8.4, 8.5, 8.9, 9.2, 9.2, 9.2,
					9.2, 9.2, 9.2, 9.1, 9.3, 9.4, 9.1, 9.1, 9.1, 9, 8.8, 8.6, 8.4,
					8.3, 8, 7.5, 7.8, 7.4, 7.6, 8.3, 8.9, 9.8, 10.4, 11, 11.3, 11,
					9.7, 7.8, 7.3, 7.4, 7.3, 7.6, 7.7, 7.7, 6.9, 7.1, 7.5, 7.1, 7.2,
					8.2, 7.6, 7.6, 7.9, 9.4, 11.4, 13.4, 14.3, 15, 15.1, 13.4, 11.6,
					10.4, 10.1, 10.4, 10.1, 9.8, 10.4, 11.3, 11.6, 12.5, 11.9, 11,
					10.1, 9.4, 8.7, 8.8, 8.7, 10.1, 12.1, 13.6, 14.7, 11.9, 11.4,
					9.8)
			set.seed(0815)
			NightFlux.V.n <- do.call(c, lapply( c(238,320,296), function(E0){
			  fLloydTaylor(10, E0, TempVar.V.n + 273.15) + rnorm(length(TempVar.V.n ))}))
			#plot(NightFlux.V.n ~ 	rep(TempVar.V.n,3) )
			DayCounter.V.i <- rep(1:300, each = 5) [ 1:length(NightFlux.V.n)]
			#trace(fRegrE0fromShortTerm,recover)		#untrace(fRegrE0fromShortTerm)
			E0 <- REddyProc:::fRegrE0fromShortTerm(
			  NightFlux.V.n, rep(TempVar.V.n,3), DayCounter.V.i, TRef = 273.15 + 15 )
			expect_equal( E0, 300, tolerance = 15, scale = 1 )
			#
		})

test_that("sMRFluxPartition Standard",{
  EddyHour.C$sSetLocationInfo(
    LatDeg = 51, LongDeg = 7, TimeZoneHour = 1)
  EddyHour.C$sMRFluxPartition()
  expect_that( EddyHour.C$sTEMP$E_0[1], equals(133.7, tolerance = .1))
})


test_that("Using fixed E0",{
  E0 <- 120
  #EddyHour.C$trace("sMRFluxPartition", recover ) # EddyHour.C$untrace("sMRFluxPartition")
  EddyHour.C$sTEMP$E_0 <- NULL
  EddyHour.C$sSetLocationInfo(LatDeg = 51.0, LongDeg = 13.6, TimeZoneHour = 1)
  # warning on duplicted columns with former test
  #suppressWarnings(
    EddyHour.C$sMRFluxPartition( debug = list(fixedE0 = E0) )
    #)   # calling sRegrE0fromShortTerm
  expect_equal( EddyHour.C$sTEMP$E_0[1], E0, tolerance = 1e-6)
  #colnames(EddyHour.C$sTEMP)
})

test_that("Tuning the temperature range",{
  #EddyHour.C$trace("sMRFluxPartition", recover ) # EddyHour.C$untrace("sMRFluxPartition")
  EddyHour.C$sTEMP$E_0 <- NULL
  EddyHour.C$sSetLocationInfo(LatDeg = 51.0, LongDeg = 13.6, TimeZoneHour = 1)
  # warning on duplicted columns with former test
  #suppressWarnings(
  EddyHour.C$sMRFluxPartition(
    parsE0Regression = list(TempRange = 3))
  #)   # calling sRegrE0fromShortTerm
  # regression test
  E0 <- 138
  expect_equal( EddyHour.C$sTEMP$E_0[1], E0, tolerance = 1e-2)
  #colnames(EddyHour.C$sTEMP)
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


