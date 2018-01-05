#require(testthat)
context("donttest")

test_that("run interactively",{
		a <- 1L
		donttest(a <- 2L)
		expected <- if (interactive()) 2L else 1L
		expect_equal( a, expected )
	})



