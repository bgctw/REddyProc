#include <Rcpp.h>
using namespace Rcpp;

// Rcpp::compileAttributes()

double square(double x){ return(x*x); }

// [[Rcpp::export]]
NumericVector RHLightResponseCostC(NumericVector theta, NumericVector flux, NumericVector sdFlux, NumericVector betaPrior, NumericVector sdBetaPrior
		, NumericVector Rg, NumericVector VPD, NumericVector Temp, NumericVector E0, NumericVector VPD0, LogicalVector fixVPD
		) {
	double _kVPD  = theta[0L];	// index starting from zero
	double _beta0  = theta[1L];
	double _alfa  = theta[2L];
	double _Rref  = theta[3L];
	double _VPD0 = VPD0[0L];
	double _E0 = E0[0L];
	bool _fixVPD = fixVPD[0L];
	double _betaPrior = betaPrior[0L];
	double _sdBetaPrior = sdBetaPrior[0L];
	int _nRec=flux.size();
	//
	double _Amax, _Reco, _GPP;
	double _misFitPrior, _misFitObs, _RSS;
	double _NEP;
	//NumericVector NEP(_nRec);
	_misFitPrior = square( (_beta0 - _betaPrior)/(_sdBetaPrior) );
	_misFitObs = 0;
	for( int i=0; i < _nRec; i++){
		_Amax = _beta0;
		if( !_fixVPD && (VPD[i] > _VPD0)){
			_Amax = _beta0 * exp( -_kVPD*(VPD[i]-_VPD0));
		}
		_Reco = _Rref*exp(_E0*(1/((273.15+10)-227.13)-1/(Temp[i]+273.15-227.13)));
		_GPP = (_Amax*_alfa*Rg[i])/(_alfa*Rg[i]+_Amax);
		//NEP[i] = _GPP -_Reco;
		_NEP = _GPP -_Reco;
		_misFitObs += square( (_NEP-flux[i])/sdFlux[i] );
	}
	//return( NumericVector::create(_beta0, VPD[0], _VPD0, _Rref, _E0, Temp[0L], NEP[0]) );
	//return NEP;
	_RSS = _misFitObs + _misFitPrior*_nRec;
	return( NumericVector::create(_RSS) );
}
