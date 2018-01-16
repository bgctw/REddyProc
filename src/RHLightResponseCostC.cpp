#include <Rcpp.h>
using namespace Rcpp;

// Rcpp::compileAttributes()

double square(double x){ return(x*x); }

// [[Rcpp::export]]
NumericVector RHLightResponseCostC(NumericVector theta, NumericVector flux, NumericVector sdFlux, NumericVector parameterPrior, NumericVector sdParameterPrior
		, NumericVector Rg, NumericVector VPD, NumericVector Temp, NumericVector VPD0, LogicalVector fixVPD
		) {
	if( theta.size() != 5 ) throw std::range_error("theta must have 5 entries.");
	double _kVPD  = theta[0];	// index starting from zero
	double _beta0  = theta[1];
	double _alfa  = theta[2];
	double _Rref  = theta[3];
	double _E0 = theta[4];
	if( !VPD0.size() || !fixVPD.size() )
		throw std::range_error("VPD0 and fixVPD must have one entry.");
	double _VPD0 = VPD0[0];
	LogicalVector fixVPDSized = fixVPD;
	if( fixVPD.size() != VPD.size() ){
		if( 1 == fixVPD.size() ){
			const bool _fixVPD = fixVPD[0];
      // const std::size_t _VPDsize = VPD.size();
      // https://github.com/RcppCore/Rcpp/issues/756
      #ifdef __linux__
        const std::size_t _VPDsize = VPD.size();
      #elif _WIN32
        const int _VPDsize = VPD.size();
      #else
        const std::size_t _VPDsize = VPD.size();
      #endif
			fixVPDSized = LogicalVector( _VPDsize, _fixVPD );
		}else throw std::range_error("fixVPD must be of length 1 or length of VPD.");
	}
	int _nRec=flux.size();
	//
	double _Amax, _Reco, _GPP;
	double _misFitPrior, _misFitObs, _RSS;
	double _NEP;
	//NumericVector NEP(_nRec);
	if( parameterPrior.size() != sdParameterPrior.size() )
		throw std::range_error("length of parameterPrior must match the length of sdParameterPrior.");
	_misFitPrior = 0;
	for( int i=0; i<parameterPrior.size(); i++){
		if( !R_IsNA(sdParameterPrior[i]) )
			_misFitPrior += square( (theta[i] - parameterPrior[i])/(sdParameterPrior[i]) );
	}
	_misFitObs = 0;
	if( sdFlux.size() != _nRec || Rg.size() != _nRec ||Temp.size() != _nRec ||VPD.size() != _nRec )
		throw std::range_error("flux, sdFlux, Rg, Temp, VPD must be of the same length.");
	for( int i=0; i < _nRec; i++){
		_Amax = _beta0;
		if( !fixVPDSized[i] && (VPD[i] > _VPD0)){
			_Amax = _beta0 * exp( -_kVPD*(VPD[i]-_VPD0));
		}
		//_Reco = _Rref*exp(_E0*(1/((273.15+10)-227.13)-1/(Temp[i]+273.15-227.13)));
		_Reco = _Rref*exp(_E0*(1/((273.15+15)-227.13)-1/(Temp[i]+273.15-227.13)));
		_GPP = (_Amax*_alfa*Rg[i])/(_alfa*Rg[i]+_Amax);
		//NEP[i] = _GPP -_Reco;
		_NEP = _GPP -_Reco;
		_misFitObs += square( (_NEP-flux[i])/sdFlux[i] );
	}
	//return( NumericVector::create(_beta0, VPD[0], _VPD0, _Rref, _E0, Temp[0], NEP[0]) );
	//return NEP;
	_RSS = _misFitObs + _misFitPrior;
	return( NumericVector::create(_RSS) );
}
