.tmp.f <- function(){
	# moved to src folder
	# when changing something remember calling Rcpp::compileAttributes()
	require(inline)
	.whichValueGreaterEqualC <- cxxfunction(
			# see .whichValueGreaterEqual in EddyUStrFilterDP
			signature(x="integer",
					threshold="integer",
					iStart="integer"),
			body = '
					IntegerVector _x(x);
					int _threshold  = as<int>(threshold);
					int i = as<int>(iStart)-1L;		// note indexing in C starts from 0
					while( (i < _x.size()) & (_x[i] < _threshold) ) i++;
					if( i == _x.size() )  return( IntegerVector::create(NA_INTEGER) ); // no break found
					return( IntegerVector::create(i+1L) );
					', plugin="Rcpp")
}

.tmp.f <- function(){
	#require(Rcpp)
	sourceCpp("src/HelloWorld.cpp")
	sourceCpp("src/whichValueGreaterEqualC.cpp")
	x <- 1:10
	expect_that( whichValueGreaterEqualC(x, 5L, 3L), equals(5L) )
}
