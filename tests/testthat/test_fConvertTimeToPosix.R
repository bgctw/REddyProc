#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for fConvertTimeToPosix functions +++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM, TW
#require(testthat)
context("fConvertTimeToPosix")
# Furher context: fCheckColNames, fCheckColNumeric, fCheckOutsideRange

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Date.F.x <- data.frame(
  FluxnetYear.n = 1995, Year.n = 1995, FluxnetDoY.n = 365, DoY.n = 365
  , FluxnetHourDec.n = 23.5, HourDec.n = 23.5, Month.n = 12, Day.n = 31
  , Hour.n = 23, Min.n = 30, Description.s = "Normal", stringsAsFactors = F)
Date.F.x <- rbind(Date.F.x, c(
  1995, 1996, 365,   1, 24.0,  0.0,  1,  1,  0,  0
  ,"24h-correction to first hour of next day"))
Date.F.x <- rbind(Date.F.x, c(
  1995, 1996, 366,   1,  0.0,  0.0,  1,  1,  0,  0
  ,"366d-correction to next year"))
Date.F.x <- rbind(Date.F.x, c(
  1996, 1996,   1,   1,  0.0,  0.0,  1,  1,  0,  0
  , "Normal but not used in Fluxnet files (time stamp midnight of previous year)"))
Date.F.x <- rbind(Date.F.x, c(
  1996, 1996,   1,   1,  0.5,  0.5,  1,  1,  0, 30
  , "Normal"))
Date.F.x <- rbind(Date.F.x, c(
  1996, 1996, 365, 365, 23.5, 23.5, 12, 30, 23, 30
  , "Normal"))
Date.F.x <- rbind(Date.F.x, c(
  1996, 1996, 365, 366, 24.0,  0.0, 12, 31,  0,  0
  , "Leap year, 24h-correction to first hour of next day"))
Date.F.x <- rbind(Date.F.x, c(
  1996, 1996, 366, 366,  0.0,  0.0, 12, 31,  0,  0
  , "Leap year, normal (no correction)"))
Date.F.x <- rbind(Date.F.x, c(
  1996, 1996, 366, 366, 23.5, 23.5, 12, 31, 23, 30
  , "Leap year, normal (no correction)"))
Date.F.x <- rbind(Date.F.x, c(
  1996, 1997, 366,   1, 24.0,  0.0,  1,  1,  0,  0
  , "Leap year, 24h-correction to first hour of next day"))
Date.F.x <- rbind(Date.F.x, c(
  1996, 1997, 367,   1,  0.0,  0.0,  1,  1,  0,  0
  , "Leap year - 366d-correction to next year"))
Date.F.x <- rbind(Date.F.x, c(
  1997, 1997,    1,  1,  0.0,  0.0,  1,  1,  0,  0
  , "Normal but not used in Fluxnet files (time stamp midnight of previous year)"))
Date.F.x <- rbind(Date.F.x, c(
  1997, 1997,    1,  1,  0.5,  0.5,  1,  1,  0, 30
  , "Normal"))
Date.F.x <- rbind(Date.F.x, c(
  2000, 2000,    1,  1,  0.5,  0.5,  1,  1,  0, 30
  , "Normal"))
Date.F.x <- data.frame(
  sapply(Date.F.x[,1:(ncol(Date.F.x) - 1)], as.numeric)
  , Description.s = Date.F.x[,ncol(Date.F.x)])
# str(Date.F.x)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("leap year",{
  y <- c(1899,1900,1901) # 1900 is no leap year because it devides by 100
  ans <- REddyProc:::is_leap_year(y)
  expect_equal(c(FALSE, FALSE, FALSE), ans)
  y <- c(1999,2000,2001) # 2100 additionally devides by 400
  ans <- REddyProc:::is_leap_year(y)
  expect_equal(c(FALSE, TRUE, FALSE), ans)
})

test_that("leap year of date",{
  d <- ISOdate(c(1999,2000,NA),1,1)
  ans <- REddyProc:::is_leap_year_of_date(d)
  expect_equal(c(FALSE, TRUE, NA), ans)
})

test_that("Time Format YDH leap year with hour hour 0",{
  df <- setNames(rbind(Date.F.x[FALSE,], list(
    1995, 1996, 366,   1,  0.0,  0.0,  1,  1,  0,  0
    ,"366d-correction to next year")), names(Date.F.x))
  ResYDH.F <- fConvertTimeToPosix(df, "YDH", Year = "FluxnetYear.n", Day = "FluxnetDoY.n"
  , Hour = "FluxnetHourDec.n" )
  TimeYDH.p <- as.POSIXlt( ResYDH.F$DateTime ) #see DateTimeClass
  expect_that( 1900 + TimeYDH.p$year, equals(df$Year.n) )
  expect_that( 1 + TimeYDH.p$yday, equals(df$DoY.n) )
  expect_that( TimeYDH.p$hour, equals(df$Hour.n) )
  expect_that( TimeYDH.p$min, equals(df$Min.n) )
})

test_that("Time Format YDH leap year with wrong day",{
  df <- setNames(rbind(Date.F.x[FALSE,], list(
    1995, 1996, 366,   1,  2,  2,  1,  1,  0,  0
    ,"day 366 in non-leap year hour 2")), names(Date.F.x))
  expect_error(
    expect_warning(
    ResYDH.F <- fConvertTimeToPosix(df, "YDH", Year = "FluxnetYear.n", Day = "FluxnetDoY.n"
                                    , Hour = "FluxnetHourDec.n" ),
    "day of year"),
    "timestamp"
  )
})

test_that("Time Format YDH",{
	expect_warning(
	  ResYDH.F <- fConvertTimeToPosix(
	  Date.F.x, "YDH", Year = "FluxnetYear.n", Day = "FluxnetDoY.n"
	  , Hour = "FluxnetHourDec.n" )
	  )
	TimeYDH.p <- as.POSIXlt( ResYDH.F$DateTime ) #see DateTimeClass
	expect_that( 1900 + TimeYDH.p$year, equals(Date.F.x$Year.n) )
	expect_that( 1 + TimeYDH.p$yday, equals(Date.F.x$DoY.n) )
	expect_that( TimeYDH.p$hour, equals(Date.F.x$Hour.n) )
	expect_that( TimeYDH.p$min, equals(Date.F.x$Min.n) )
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Time Format YMDH",{
  ResYMDH.F  <- fConvertTimeToPosix(
    Date.F.x, "YMDH", Year = "Year.n", Day = "Day.n", Month = "Month.n"
    , Hour = "HourDec.n" )
  TimeYMDH.p <- as.POSIXlt( ResYMDH.F$DateTime )
  expect_that( 1900 + TimeYMDH.p$year, equals(Date.F.x$Year.n) )
  expect_that( 1 + TimeYMDH.p$yday, equals(Date.F.x$DoY.n) )
  expect_that( TimeYMDH.p$hour, equals(Date.F.x$Hour.n) )
  expect_that( TimeYMDH.p$min, equals(Date.F.x$Min.n) )
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Time Format YMDHM",{
  ResYMDHM.F  <- fConvertTimeToPosix(
    Date.F.x, "YMDHM", Year = "Year.n", Day = "Day.n", Month = "Month.n"
    , Hour = "Hour.n", Min = "Min.n" )
  TimeYMDHM.p <- as.POSIXlt( ResYMDHM.F$DateTime )
  expect_that( 1900 + TimeYMDHM.p$year, equals(Date.F.x$Year.n) )
  expect_that( 1 + TimeYMDHM.p$yday, equals(Date.F.x$DoY.n) )
  expect_that( TimeYMDHM.p$hour, equals(Date.F.x$Hour.n) )
  expect_that( TimeYMDHM.p$min, equals(Date.F.x$Min.n) )
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Time format name unvalid",{
  expect_error(
    res <- fConvertTimeToPosix(
      Date.F.x, "XXX", Year = "FluxnetYear.n", Day = "FluxnetDoY.n"
      , Hour = "FluxnetHourDec.n" )
  )
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Time format values unvalid (fCheckOutsideRange)",{
  expect_warning(
    res <- fConvertTimeToPosix(
      Date.F.x, "YMDH", Year = "Year.n", Month = "Month.n", Day = "Day.n"
      , Hour = "FluxnetHourDec.n" )
    )
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Time format missing cols (fCheckColNames)",{
	expect_error(
			res <- fConvertTimeToPosix(
			  Date.F.x, "YDH", Year = "FluxnetYear.n", Day = "FluxnetDoY.n", Hour = "hr" )
	)
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Time Format non-numeric cols (fCheckColNumeric)",{
			expect_error(
					res <- fConvertTimeToPosix(
					  Date.F.x, "YDH", Year = "FluxnetYear.n", Day = "FluxnetDoY.n"
					  , Hour = "Description.s" )
			)
		})

test_that("reading Berkeley",{
  res  <- fConvertTimeToPosix(
    Date.F.x, "YMDH", Year = "Year.n", Day = "Day.n", Month = "Month.n"
    , Hour = "HourDec.n" )
  TimeYMDH.p <- as.POSIXlt( res$DateTime )
  res$berkeleyDate <- POSIXctToBerkeleyJulianDate(res$DateTime)
  time2 <- BerkeleyJulianDateToPOSIXct(res$berkeleyDate)
  expect_equal( as.numeric(time2), as.numeric(res$DateTime))
})

test_that("reading Berkeley",{
  res  <- fConvertTimeToPosix(
    Date.F.x, "YMDH", Year = "Year.n", Day = "Day.n", Month = "Month.n"
    , Hour = "HourDec.n" )
  TimeYMDH.p <- as.POSIXlt( res$DateTime )
  res$berkeleyDate <- POSIXctToBerkeleyJulianDate(res$DateTime)
  time2 <- BerkeleyJulianDateToPOSIXct(res$berkeleyDate)
  expect_equal( as.numeric(time2), as.numeric(res$DateTime))
})

