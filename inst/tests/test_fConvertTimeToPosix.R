#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for fConvertTimeToPosix functions +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM, TW
#require(testthat)
context("fConvertTimeToPosix")
# Furher context: fCheckColNames, fCheckColNumeric, fCheckOutsideRange

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Date.F.x <- data.frame( FluxnetYear.n=1995, Year.n=1995, FluxnetDoY.n=365, DoY.n=365, FluxnetHourDec.n=23.5, HourDec.n=23.5, Month.n=12, Day.n=31, Hour.n=23, Min.n=30, Description.s="Normal", stringsAsFactors=F)
Date.F.x <- rbind(Date.F.x, c(1995, 1996, 365,   1, 24.0,  0.0,  1,  1,  0,  0,"24h-correction to first hour of next day"))
Date.F.x <- rbind(Date.F.x, c(1995, 1996, 366,   1,  0.0,  0.0,  1,  1,  0,  0,"366d-correction to next year"))
Date.F.x <- rbind(Date.F.x, c(1996, 1996,   1,   1,  0.0,  0.0,  1,  1,  0,  0, "Normal but not used in Fluxnet files (time stamp midnight of previous year)"))
Date.F.x <- rbind(Date.F.x, c(1996, 1996,   1,   1,  0.5,  0.5,  1,  1,  0, 30, "Normal"))
Date.F.x <- rbind(Date.F.x, c(1996, 1996, 365, 365, 23.5, 23.5, 12, 30, 23, 30, "Normal"))
Date.F.x <- rbind(Date.F.x, c(1996, 1996, 365, 366, 24.0,  0.0, 12, 31,  0,  0, "Leap year, 24h-correction to first hour of next day"))
Date.F.x <- rbind(Date.F.x, c(1996, 1996, 366, 366,  0.0,  0.0, 12, 31,  0,  0, "Leap year, normal (no correction)"))
Date.F.x <- rbind(Date.F.x, c(1996, 1996, 366, 366, 23.5, 23.5, 12, 31, 23, 30, "Leap year, normal (no correction)"))
Date.F.x <- rbind(Date.F.x, c(1996, 1997, 366,   1, 24.0,  0.0,  1,  1,  0,  0, "Leap year, 24h-correction to first hour of next day"))
Date.F.x <- rbind(Date.F.x, c(1996, 1997, 367,   1,  0.0,  0.0,  1,  1,  0,  0, "Leap year - 366d-correction to next year"))
Date.F.x <- rbind(Date.F.x, c(1997, 1997,    1,  1,  0.0,  0.0,  1,  1,  0,  0, "Normal but not used in Fluxnet files (time stamp midnight of previous year)"))
Date.F.x <- rbind(Date.F.x, c(1997, 1997,    1,  1,  0.5,  0.5,  1,  1,  0, 30, "Normal"))
Date.F.x <- rbind(Date.F.x, c(2000, 2000,    1,  1,  0.5,  0.5,  1,  1,  0, 30, "Normal"))
Date.F.x <- data.frame(sapply(Date.F.x[,1:(ncol(Date.F.x)-1)], as.numeric), Description.s=Date.F.x[,ncol(Date.F.x)])
# str(Date.F.x)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Time Format YDH",{
	expect_warning(ResYDH.F <- fConvertTimeToPosix(Date.F.x, "YDH", Year.s="FluxnetYear.n", Day.s="FluxnetDoY.n", Hour.s="FluxnetHourDec.n" ))
	TimeYDH.p <- as.POSIXlt( ResYDH.F$DateTime ) #see DateTimeClass
	expect_that( 1900+TimeYDH.p$year, equals(Date.F.x$Year.n) )
	expect_that( 1+TimeYDH.p$yday, equals(Date.F.x$DoY.n) )
	expect_that( TimeYDH.p$hour, equals(Date.F.x$Hour.n) )
	expect_that( TimeYDH.p$min, equals(Date.F.x$Min.n) )
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Time Format YMDH",{
  ResYMDH.F  <- fConvertTimeToPosix(Date.F.x, "YMDH", Year.s="Year.n", Day.s="Day.n", Month.s="Month.n", Hour.s="HourDec.n" )
  TimeYMDH.p <- as.POSIXlt( ResYMDH.F$DateTime )
  expect_that( 1900+TimeYMDH.p$year, equals(Date.F.x$Year.n) )
  expect_that( 1+TimeYMDH.p$yday, equals(Date.F.x$DoY.n) )
  expect_that( TimeYMDH.p$hour, equals(Date.F.x$Hour.n) )
  expect_that( TimeYMDH.p$min, equals(Date.F.x$Min.n) )
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Time Format YMDHM",{
  ResYMDHM.F  <- fConvertTimeToPosix(Date.F.x, "YMDHM", Year.s="Year.n", Day.s="Day.n", Month.s="Month.n", Hour.s="Hour.n", Min.s="Min.n" )
  TimeYMDHM.p <- as.POSIXlt( ResYMDHM.F$DateTime )
  expect_that( 1900+TimeYMDHM.p$year, equals(Date.F.x$Year.n) )
  expect_that( 1+TimeYMDHM.p$yday, equals(Date.F.x$DoY.n) )
  expect_that( TimeYMDHM.p$hour, equals(Date.F.x$Hour.n) )
  expect_that( TimeYMDHM.p$min, equals(Date.F.x$Min.n) )
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Time format name unvalid",{
  expect_error( 
    res <- fConvertTimeToPosix(Date.F.x, "XXX", Year.s="FluxnetYear.n", Day.s="FluxnetDoY.n", Hour.s="FluxnetHourDec.n" ) 
  )
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Time format values unvalid (fCheckOutsideRange)",{
  expect_warning( 
    res <- fConvertTimeToPosix(Date.F.x, "YMDH", Year.s="Year.n", Month.s="Month.n", Day.s="Day.n", Hour.s="FluxnetHourDec.n" ) 
    )
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Time format missing cols (fCheckColNames)",{
	expect_error( 
			res <- fConvertTimeToPosix(Date.F.x, "YDH", Year.s="FluxnetYear.n", Day.s="FluxnetDoY.n", Hour.s="hr" ) 
	)
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Time Format non-numeric cols (fCheckColNumeric)",{
			expect_error( 
					res <- fConvertTimeToPosix(Date.F.x, "YDH", Year.s="FluxnetYear.n", Day.s="FluxnetDoY.n", Hour.s="Description.s" ) 
			)
		})
