#require(testthat)
context(".binWithEqualValues")
# Furher context: fCheckColNames, fCheckColNumeric, fCheckOutsideRange

if( !exists(".whichValueGreaterEqual") ) .whichValueGreaterEqual <- REddyProc:::.whichValueGreaterEqual
if( !exists(".binWithEqualValuesBalanced") ) .binWithEqualValuesBalanced <- REddyProc:::.binWithEqualValuesBalanced
if( !exists(".binWithEqualValuesMinRec") ) .binWithEqualValuesMinRec <- REddyProc:::.binWithEqualValuesMinRec

test_that(".whichValueGreaterEqual",{
			x <- 1:10
			expect_that( .whichValueGreaterEqual(x, 5L), equals(5L) )
			expect_that( .whichValueGreaterEqual(x, 11L), equals(NA_integer_) )
			expect_that( .whichValueGreaterEqual(x, 5L, 3L), equals(5L) )
			expect_that( .whichValueGreaterEqual(x, 7L, 3L), equals(7L) )
			expect_that( .whichValueGreaterEqual(x, 3L, 3L), equals(3L) )
			expect_that( .whichValueGreaterEqual(x, 1L), equals(1L) )
			expect_that( .whichValueGreaterEqual(x, 10, 10L), equals(10L) )
			expect_that( .whichValueGreaterEqual(x, 10, 11L), equals(NA_integer_) )
		})

test_that(".binWithEqualValuesBalanced",{
			expect_that( .binWithEqualValuesBalanced(1:10, 3L), equals(c(1,1,1,2,2,2,3,3,3,3)) )
			expect_that( .binWithEqualValuesBalanced(c(rep(1,4),2:7), 3L), equals(c(rep(1,4),2,2,3,3,3,3)) ) # one 2 replaced by 1
			expect_that( .binWithEqualValuesBalanced(c(rep(1,4),2,2,3,3,3), 3L), equals(c(rep(1,4),2,2,3,3,3)) ) # one 2 replaced by 1
			expect_that( .binWithEqualValuesBalanced(c(rep(1,7),2:4), 3L), equals(c(rep(1,7),3,3,3)) ) # group 2 disappeard
		})

test_that(".binWithEqualValuesMinRec",{
			expect_that( .binWithEqualValuesMinRec(1:10, 3L), is_equivalent_to(c(1,1,1,2,2,2,3,3,3,3)) )	# foruth class less than 3 values, sorted to 3 
			#trace(.binWithEqualValues, recover)	#untrace(.binWithEqualValues)	
			expect_that( .binWithEqualValuesMinRec(c(rep(1,4),2:7), 3L), is_equivalent_to(c(rep(1,4),2,2,2,3,3,3)) ) # class 1 extends and others are shifted
			expect_that( .binWithEqualValuesMinRec(c(rep(1,4),2,2,3,3,3,4), 3L), is_equivalent_to(c(rep(1,4),rep(2,6))) ) # second class includes 3 
			expect_that( .binWithEqualValuesMinRec(c(rep(1,7),2:4), 3L), is_equivalent_to(c(rep(1,7),2,2,2)) ) # first group extends 
		})




.tmp.benchmark <- function(){
	
}


