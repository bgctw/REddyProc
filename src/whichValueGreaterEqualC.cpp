#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector whichValueGreaterEqualC(IntegerVector x, IntegerVector threshold, IntegerVector iStart) {
	int _threshold  = as<int>(threshold);
	int i = (as<int>(iStart)) - 1L;		// note indexing in C starts from 0
	// using && (logical and), rather than & (bitwise and)
	// in addition use ++i, because i++ causes problems with R 4.x
	while( (i < x.size()) && (x[i] < _threshold) ) ++i;
	//wrong: while( (i < x.size()) & (x[i] < _threshold) ) i++;
	if( i < x.size() ) {
	  // return index, but counting from 1
	  return( IntegerVector::create(i+1L) );
	} else {
	  // no break found
	  return( IntegerVector::create(NA_INTEGER) );
	}
}
