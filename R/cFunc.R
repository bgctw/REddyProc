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

