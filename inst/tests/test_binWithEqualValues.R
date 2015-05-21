#require(testthat)
context(".binWithEqualValues")
# Furher context: fCheckColNames, fCheckColNumeric, fCheckOutsideRange

test_that(".whichValueGreaterEqual",{
			x <- 1:10
			expect_that( .whichValueGreaterEqual(x, 5L), equals(5L) )
			expect_that( .whichValueGreaterEqual(x, 11L), equals(NA_integer_) )
			expect_that( .whichValueGreaterEqual(x, 5L, 3L), equals(5L) )
			expect_that( .whichValueGreaterEqual(x, 7L, 3L), equals(7L) )
			expect_that( .whichValueGreaterEqual(x, 3L, 3L), equals(3L) )
			expect_that( .whichValueGreaterEqual(x, 1L), equals(1L) )
		})

test_that(".binWithEqualValues",{
			expect_that( .binWithEqualValues(1:10, 3L), equals(c(1,1,1,2,2,2,3,3,3,3)) )
			expect_that( .binWithEqualValues(c(rep(1,4),2:7), 3L), equals(c(rep(1,4),2,2,3,3,3,3)) ) # one 2 replaced by 1
			expect_that( .binWithEqualValues(c(rep(1,4),2,2,3,3,3), 3L), equals(c(rep(1,4),2,2,3,3,3)) ) # one 2 replaced by 1
			expect_that( .binWithEqualValues(c(rep(1,7),2:4), 3L), equals(c(rep(1,7),3,3,3)) ) # group 2 disappeard
		})



.tmp.benchmark <- function(){
	
}


