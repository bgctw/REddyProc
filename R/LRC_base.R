LightResponseCurveFitter <- setRefClass('LightResponseCurveFitter'
## R5 reference parent class for describing the NEP ~ PAR relationship
##author<<
## TW, MM
)

LightResponseCurve_getParameterNames <- function(
	### return the parameter names used by this Light Response Curve Funciton
){
	stop("Abstract method. Need to define in derived LRC class.")
	##value<< string vector of parameter names. Positions are important.
}
LightResponseCurveFitter$methods(getParameterNames = LightResponseCurve_getParameterNames)

LightResponseCurve_fitLRC <- function(
		### Optimize rectangular hyperbolic light response curve against data in one window and estimate uncertainty
		dsDay				##<< data.frame with columns NEE, Rg, Temp_C, VPD, and no NAs in NEE
		, E0				##<< temperature sensitivity of respiration
		, sdE0				##<< standard deviation of E_0.n
		, RRefNight			##<< basal respiration estimated from night time data 
		, controlGLPart=partGLControl()	##<< further default parameters (see \code{\link{partGLControl}})
		, lastGoodParameters			##<< numeric vector of last good theta
){
	##author<< TW, MM
	##seealso<< \code{\link{partGLFitLRCWindows}}
	#	
	#Definition of initial guess theta, theta2 and theta3. Three initial guess vectors are defined according to Lasslop et al., 2010
	parNames <- .self$getParameterNames()  	# hook method from derived classes
	nPar <- length(parNames)
	##details<<
	## Optimization is performed for three initial parameter sets that differ by beta0 (*1.3, *0.8).
	## From those three, the optimization result is selected that yielded the lowest misfit.
	## Starting values are: k=0, beta=interpercentileRange(0.03,0.97) of respiration, alpha=0.1, R_ref 
	## from nightTime estimate.
	## E0 is fixed to the night-time estimate, but varied for estimating parameter uncertainty.
	parameterPrior <-  .self$getPriorLocation( dsDay$NEE, RRefNight=RRefNight, E0=E0)
	thetaInitials <- .self$getParameterInitials(parameterPrior)
	#
	##seealso<< \code{\link{parmGLOptimLRCBounds}}
	resOpt3 <- apply( thetaInitials, 1, function(theta0){
				resOpt <- .self$optimLRCBounds(theta0, parameterPrior, dsDay=dsDay, ctrl=controlGLPart, lastGoodParameters.V.n=lastGoodParameters )
			})
	iValid <- which(sapply(resOpt3,function(resOpt){ is.finite(resOpt$theta[1]) }))
	resOpt3Valid <- resOpt3[iValid]
	optSSE <- sapply(resOpt3Valid, "[[", "value")
	getNAResult <- function(){ list(
				thetaOpt=structure( rep(NA_real_, nPar), names=colnames(thetaInitials) )
				,iOpt=integer(0)		##<< index of parameters that have been optimized
				,thetaInitialGuess=			##<< the initial guess from data
						thetaInitials[1,]	
				,covParms=matrix(NA_real_, nPar, nPar, dimnames=list(colnames(thetaInitials),colnames(thetaInitials)))
		)}		
	if( sum(!is.na(optSSE)) == 0L ){
		return( getNAResult() )
	} else {
		resOpt <- resOpt3Valid[[iBest <- which.min(optSSE)]] # select the one with the least cost
		thetaOpt<-resOpt$theta
		if(controlGLPart$nBootUncertainty == 0L) {
			##details<< If \code{controlGLPart.l$nBootUncertainty == 0L} then the covariance matrix of the 
			## parameters is estimated by the Hessian of the LRC curve at optimum.
			## Then, the additional uncertainty and covariance with uncertaint E0 is neglected.
			#seParmsHess <- seParmsHess0 <- sqrt(abs(diag(solve(resOpt$hessian))))
			covParmsLRC <- if( (resOpt$hessian[1L,1L] < 1e-8) ){
						# case where k=0 and not varying: cov(k,:) = cov(:,) = 0
						covParmsLRC <- structure( diag(0,nrow=nrow(resOpt$hessian)), dimnames=dimnames(resOpt$hessian))
						covParmsLRC[-1L,-1L] <- solve(resOpt$hessian[-1L,-1L])
						covParmsLRC
					} else {
						solve(resOpt$hessian)
					}
			covParms <- structure( diag(0,nrow=length(resOpt$theta)), dimnames=list(parNames,parNames))
			covParms[5L,5L] <- sdE0^2
			covParms[resOpt$iOpt,resOpt$iOpt] <- covParmsLRC  
			if( any(diag(covParms) < 0)) thetaOpt[] <- NA	# no real if covariance negative
		} else {
			##details<< 
			## If \code{controlGLPart.l$nBootUncertainty > 0L} then the covariance matrix of the 
			## parameters is estimated by a bootstrap of the data.
			## In each draw, E0 is drawn from N ~ (E_0, sdE_0).
			# #seealso<< \code{\link{.bootStrapLRCFit}}
			resBoot  <- .bootStrapLRCFit(resOpt$theta, resOpt$iOpt, dsDay, sdE0, parameterPrior, controlGLPart, LRC=.self)
			#resBoot  <- .bootStrapLRCFit(resOpt$theta, resOpt$iOpt, dsDay, sdE_0.n, parameterPrior, controlGLPart.l=within(controlGLPart.l,nBootUncertainty <- 30L))
			iFiniteRows <- which( is.finite(resBoot[,1L]))
			if( length(iFiniteRows)  < 0.8*nrow(resBoot))
				return( getNAResult() )
			covParms <- cov(resBoot[iFiniteRows,])
			#better not to average parameters
			#opt.parms.V <- apply(resBoot, 2, median, na.rm=TRUE)
			#se.parms.V <- apply(resBoot, 2, sd, na.rm=TRUE)
		}
	}
	##value<< a list, If none of the optimizations from different starting conditions converged,
	## the parameters are NA
	ans <- list(
			thetaOpt=thetaOpt			##<< numeric vector of optimized parameters including 
			## the fixed ones and including E0
			,iOpt=c(resOpt$iOpt,5L)		##<< index of parameters that have been optimized
			## , here including E0, which has been optimized prior to this function.
			,thetaInitialGuess=			##<< the initial guess from data
					thetaInitials[1,]	
			,covParms=covParms			##<< numeric matrix of the covariance matrix of parameters, including E0
	)
} 
LightResponseCurveFitter$methods(fitLRC = LightResponseCurve_fitLRC)


LightResponseCurve_optimLRCOnAdjustedPrior = function(
		###<< Lower bound flux uncertainty and adjust prior uncertainty before calling optimLRC 
		theta  					##<< numeric vector of starting values
		, iOpt					##<< integer vector: positions of subset of parameters that are optimized
		, dsDay					##<< dataframe of NEE, sdNEE and predictors Rg, VPD and Temp
		, parameterPrior		##<< numeric vector of prior parameter estimates (corresponding to theta) # TODO rename to thetaPrior
		, ctrl					##<< list of further controls
		, ...					##<< further arguments to \code{\link{optimLRC}} (passed to \code{\link{computeCost}})
){
	if( !all(is.finite(theta))) stop("need to provide finite starting values.")
	##details<<
	## Only those records are used for optimization where both NEE and sdNEE are finite.
	dsDayFinite <- dsDay[ is.finite(dsDay$NEE) & is.finite(dsDay$sdNEE), ]
	##details<<
	## Optimization of LRC parameters takes into account the uncertainty of the flux values.
	## In order to avoid very strong leverage, values with a very low uncertainty (< median) are assigned
	## the median of the uncertainty.
	## This procedure downweighs records with a high uncertainty, but does not apply a large leverage for
	## records with a very low uncertainty. Avoid this correction by suppyling setting \code{ctrl$isBoundLowerNEEUncertainty = FALSE}
	Fc_unc <- if( isTRUE(ctrl$isBoundLowerNEEUncertainty) ){
				pmax( dsDayFinite$sdNEE, quantile(dsDayFinite$sdNEE, 0.3) ) #twutz: avoid excessive weights by small uncertainties (of 1/unc^2)
			} else {
				dsDayFinite$sdNEE
			}
	#plot( Fc_unc ~ dsDayFinite$sdNEE)
	medianRelFluxUncertainty <- abs(median(Fc_unc/dsDayFinite$NEE))
	##details<<
	## The uncertainty of the prior, that maybe derived from fluxes)  is allowed to 
	## adapt to the uncertainty of the fluxes.
	## This is done in \code{link{LightResponseCurve_getPriorScale}}
	sdParameterPrior <- .self$getPriorScale( parameterPrior, medianRelFluxUncertainty, nrow(dsDayFinite), ctrl=ctrl )
	sdParameterPrior[-iOpt] <- NA
	isUsingHessian <- (ctrl$nBootUncertainty==0L)
	.self$optimLRC( theta 
			,iOpt=iOpt
			,flux = -dsDayFinite$NEE 
			,sdFlux = Fc_unc	  
			,sdParameterPrior = sdParameterPrior
			,parameterPrior = parameterPrior
			,Rg = dsDayFinite$Rg 
			,VPD = dsDayFinite$VPD
			,Temp=dsDayFinite$Temp
			, isUsingHessian=isUsingHessian
			, ctrl=ctrl
	)  
	##value<< result of \code{\link{optimLRC}}
}
LightResponseCurveFitter$methods(optimLRCOnAdjustedPrior = LightResponseCurve_optimLRCOnAdjustedPrior)


LightResponseCurve_getPriorLocation <- function(
		### return the prior distribution of parameters
		NEEDay 		##<< numeric vector of daytime NEE
		,RRefNight	##<< numeric scalar of basal respiration estimated from night-time data
		,E0			##<< numeric scalar of night-time estimate of temperature sensitivity
){
	stop("Abstract method. Need to define in derived LRC class.")
	##value<< a numeric vector with prior estimates of the parameters
	parameterPrior <- c(
			k=0
			#,beta=as.vector(abs(quantile(NEEDay, 0.03)-quantile(NEEDay, 0.97)))
			,beta=as.vector(abs(quantile(NEEDay, 0.03, na.rm=TRUE)-quantile(NEEDay, 0.97, na.rm=TRUE)))
			,alpha=0.1
			#,R_ref=mean(NEENight.V.n, na.rm=T)
			,R_ref=if( is.finite(RRefNight) ) as.vector(RRefNight) else stop("must provide finite RRefNight") #mean(NEENight.V.n, na.rm=T)
			,E_0=as.vector(E0)
			,conv=0.2
	)   
} 		
LightResponseCurveFitter$methods( getPriorLocation = LightResponseCurve_getPriorLocation)


LightResponseCurve_getPriorScale <- function(
		### return the prior distribution of parameters
		thetaPrior 	##<< numeric vector of location of priors
		,medianRelFluxUncertainty	##<< numeric scalar: median across the relative uncertainty of the flux values, i.e. sdNEE/NEE
		,nRec		##<< integer scalar: number of finite observations
		, ctrl		##<< list of further controls, with entry \code{isLasslopPriorsApplied}	
){
	stop("Abstract method. Need to define in derived LRC class.")
	##value<< a numeric vector with prior estimates of the parameters
	sdParameterPrior <- if(ctrl$isLasslopPriorsApplied){
				c(k=50, beta=600, alpha=10, Rb=80, E0=NA, conv=20)
			} else {
				##details<< 
				## The beta parameter is quite well defined. Hence use a prior with a standard deviation.
				## The specific results are sometimes a bit sensitive to the uncertainty of the beta prior. 
				## This uncertainty is set corresponding to 10 times the median relative flux uncertainty.
				## The prior is weighted n times the observations in the cost.
				## Hence, overall it is using a weight of 1/10 of the weight of all observations.
				sdBetaPrior <- 10*medianRelFluxUncertainty*thetaPrior[2]/sqrt(nRec)
				c(k=NA, beta=as.vector(sdBetaPrior), alpha=NA, Rb=NA, E0=NA, conv=NA)
			}
} 
LightResponseCurveFitter$methods(getPriorScale = LightResponseCurve_getPriorScale)

LightResponseCurve_getParameterInitials <- function(
		### return the prior distribution of parameters
		thetaPrior 	##<< numeric vector prior estimate of parameters
){
	stop("Abstract method. Need to define in derived LRC class.")
	# only beta0 is varied between different intial guesses
	##value<< a numeric matrix (3,nPar) of initial values for fitting parameters
	thetaInitials <- matrix( rep(thetaPrior,each=3), 3,nPar, dimnames=list(NULL,names(thetaPrior)))
	thetaInitials [2,2] <- parameterPrior[2]*1.3
	thetaInitials [3,2] <- parameterPrior[2]*0.8
	thetaInitials 
}
LightResponseCurveFitter$methods( getParameterInitials = LightResponseCurve_getParameterInitials)

LightResponseCurve_optimLRCBounds <- function(
		### Optimize parameters of light response curve and refit with some fixed parameters if fitted parameters are outside bounds
		theta0			##<< initial parameter estimate
		,parameterPrior	##<< prior estimate of model parameters
		, ...			##<< further parameters to \code{.optimLRC}, such as \code{dsDay}
		,lastGoodParameters.V.n ##<< parameters vector of last successful fit
		, ctrl					##<< list of further controls
){
	stop("Abstract method. Need to define in derived LRC class.")
	##author<< TW, MM
	##value<< list result of optimization as of \code{.optimLRC} with entries 
	## \item{theta}{ numeric parameter vector that includes the fixed components}
	## \item{iOpt}{ integer vector of indices of the vector that have been optimized}
}
LightResponseCurveFitter$methods(optimLRCBounds = LightResponseCurve_optimLRCBounds)


LightResponseCurve_optimLRC <- function(
		### call the optimization function
		theta				##<< numeric vector: starting parameters
		,iOpt				##<< integer vector: positions of paramters to optimize
		,sdParameterPrior	##<< numeric vector: prior uncertainty
		,isUsingHessian=isUsingHessian	##<< boolean scalar, whether to compute Hessian at the optimum
		,...				##<< further arguments to \code{\link{computeCost}}
){
	stop("Abstract method. Need to define in derived LRC class.")
	## list of result of \code{\link{optim}} amended with list
	ans <- list(
			theta = theta	##<< numeric vector: optimized parameter vector including the fixed components
			,iOpt = iOpt	##<< integer vector: position of parameters that have been optimized
	)
	c(resOptim, ans)
} 
LightResponseCurveFitter$methods(optimLRC = LightResponseCurve_optimLRC)


LightResponseCurve_computeCost <- function(
		### Computing residual sum of sqares for predictions vs. data of NEE
		thetaOpt   ##<< parameter vecotr with components of theta0 that are optimized 
		,theta		##<< parameter vector with positions as in argument of \code{\link{partGL_RHLightResponse}} 
		,iOpt		##<< position in theta that are optimized 
		,flux=NA 	##<< numeric: NEP (-NEE) or GPP time series [umolCO2/m2/s], should not contain NA
		,sdFlux=NA 	##<< numeric: standard deviation of Flux [umolCO2/m2/s], should not contain NA
		,parameterPrior		##<< numeric vector along theta: prior estimate of parameter (range of values)
		,sdParameterPrior	##<< standard deviation of parameterPrior
		,...			##<< other arguments to \code{\link{partGL_RHLightResponse}}
		,VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010
		,fixVPD = FALSE   	##<< fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered
) {
	theta[iOpt] <- thetaOpt
	#print(theta)
	resPred <- .self$predictLRC(theta, ..., VPD0=VPD0, fixVPD=fixVPD)
	NEP_mod <- resPred$NEP
	#if(is.na(mean(NEP_mod))==TRUE) {
	#  recover()
	#}
	misFitPrior <- (((theta - parameterPrior))/(sdParameterPrior))^2
	misFitObs <- sum(((NEP_mod-flux)/sdFlux)^2)
	RSS <- misFitObs + sum(misFitPrior, na.rm=TRUE)
	#if( !is.finite(RSS) ) recover()	# debugging the fit
	RSS
}
LightResponseCurveFitter$methods( computeCost = LightResponseCurve_computeCost )

LightResponseCurve_predictLRC <- function(
		### Nonrectangular Rectungular Hyperbolic Light Response function: (Gilmanov et al., 2003)
		theta   ##<< theta [numeric] -> parameter vector (theta[1]=kVPD (k), theta[2]=beta0 (beta), theta[3]=alfa, theta[4]=Rref (rb), theta[5]=E0, theta[6]=conv)
		##<< E0: Temperature sensitivity ("activation energy") in Kelvin (degK) #get("testparams", envir=environment(foo)
		,Rg   	##<< ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation
		,VPD 	##<< VPD [numeric] -> Vapor Pressure Deficit [hPa]
		,Temp 	##<< Temp [degC] -> Temperature [degC] 
		#,E0 	##<< Temperature sensitivity ("activation energy") in Kelvin (degK) #get("testparams", envir=environment(foo)
		,VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010
		,fixVPD = FALSE   	##<< fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered
){
	stop("Abstract method. Need to define in derived LRC class.")
	##details<<
	## If theta is a matrix, a different row of parameters is used for different entries of other inputs
	##value<< a data.frame of length of Rg of computed  
	ans <- list(
			NEP=NEP		##<< Net ecosystem production (-NEE), vector of length(Rg)
			,Reco=Reco	##<< Ecosystem respiration 
			,GPP=GPP	##<< Gross primary production
	)
}
LightResponseCurveFitter$methods(	predictLRC = LightResponseCurve_predictLRC)

LightResponseCurve_computeLRCGradient <- function(
		### Gradient of \code{\link{partGL_RHLightResponse}}
		theta   ##<< theta [numeric] -> parameter vector (theta[1]=kVPD (k), theta[2]=beta0 (beta), theta[3]=alfa, theta[4]=Rref (rb), theta[4]=E0, theta[5]=conv)
		##<< E0: Temperature sensitivity ("activation energy") in Kelvin (degK)
		,Rg   	##<< ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation
		,VPD 	##<< VPD [numeric] -> Vapor Pressure Deficit [hPa]
		,Temp 	##<< Temp [degC] -> Temperature [degC] 
		#,E0 	##<< Temperature sensitivity ("activation energy") in Kelvin (degK) #get("testparams", envir=environment(foo)
		,VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010
		,fixVPD = FALSE   	##<< fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered
) {
	stop("Abstract method. Need to define in derived LRC class.")
	## list with gradient matrices (length(Rg), nPar) for quantities 
	ans <- list(
			NEP=gradNEP
			,Reco=gradReco
			,GPP=gradGPP
	)
}
LightResponseCurveFitter$methods(	computeLRCGradient = LightResponseCurve_computeLRCGradient)





