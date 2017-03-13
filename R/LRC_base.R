LightResponseCurveFitter <- setRefClass('LightResponseCurveFitter'
## R5 reference parent class for describing the NEP ~ PAR relationship
##author<<
## TW, MM
)

LightResponseCurveFitter_getParameterNames <- function(
	### return the parameter names used by this Light Response Curve Funciton
){
	# if this method is adjusted in subclass, then also adjust getPriorLocation, getPriorScale
	##value<< string vector of parameter names. Positions are important.
	c(k="k"						##<< VPD effect
	, beta="beta"				##<< saturation of GPP at high radiation
	, alpha="alpha"				##<< initial slope
	, RRef="RRef"				##<< basal respiration 
	, E0="E0")					##<< temperature sensitivity estimated from night-time data
}
LightResponseCurveFitter$methods(getParameterNames = LightResponseCurveFitter_getParameterNames)

LightResponseCurveFitter_fitLRC <- function(
		### Optimize rectangular hyperbolic light response curve against data in one window and estimate uncertainty
		dsDay				##<< data.frame with columns NEE, Rg, Temp_C, VPD, and no NAs in NEE
		, E0				##<< temperature sensitivity of respiration
		, sdE0				##<< standard deviation of E_0.n
		, RRefNight			##<< basal respiration estimated from night time data 
		, controlGLPart=partGLControl()	##<< further default parameters (see \code{\link{partGLControl}})
		, lastGoodParameters=rep(NA_real_,7L)	##<< numeric vector returned by last reasonable fit
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
				resOpt <- .self$optimLRCBounds(theta0, parameterPrior, dsDay=dsDay, ctrl=controlGLPart, lastGoodParameters=lastGoodParameters )
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
LightResponseCurveFitter$methods(fitLRC = LightResponseCurveFitter_fitLRC)


LightResponseCurveFitter_optimLRCOnAdjustedPrior = function(
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
	## This is done in \code{link{LightResponseCurveFitter_getPriorScale}}
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
LightResponseCurveFitter$methods(optimLRCOnAdjustedPrior = LightResponseCurveFitter_optimLRCOnAdjustedPrior)


LightResponseCurveFitter_getPriorLocation <- function(
		### return the prior distribution of parameters
		NEEDay 		##<< numeric vector of daytime NEE
		,RRefNight	##<< numeric scalar of basal respiration estimated from night-time data
		,E0			##<< numeric scalar of night-time estimate of temperature sensitivity
){
	##value<< a numeric vector with prior estimates of the parameters
	parameterPrior <- c(
			k=0.05
			#,beta=as.vector(abs(quantile(NEEDay, 0.03)-quantile(NEEDay, 0.97)))
			,beta=as.vector(abs(quantile(NEEDay, 0.03, na.rm=TRUE)-quantile(NEEDay, 0.97, na.rm=TRUE)))
			,alpha=0.1
			#,RRef=mean(NEENight.V.n, na.rm=T)
			,RRef=if( is.finite(RRefNight) ) as.vector(RRefNight) else stop("must provide finite RRefNight") #mean(NEENight.V.n, na.rm=T)
			,E0=as.vector(E0)
	)   
} 		
LightResponseCurveFitter$methods( getPriorLocation = LightResponseCurveFitter_getPriorLocation)


LightResponseCurveFitter_getPriorScale <- function(
		### return the prior distribution of parameters
		thetaPrior 	##<< numeric vector of location of priors
		,medianRelFluxUncertainty	##<< numeric scalar: median across the relative uncertainty of the flux values, i.e. sdNEE/NEE
		,nRec		##<< integer scalar: number of finite observations
		, ctrl		##<< list of further controls, with entry \code{isLasslopPriorsApplied}	
){
	##value<< a numeric vector with prior estimates of the parameters
	sdParameterPrior <- if(ctrl$isLasslopPriorsApplied){
				c(k=50, beta=600, alpha=10, RRef=80, E0=NA)	#twutz: changed to no prior for logitconv
			} else {
				##details<< 
				## The beta parameter is quite well defined. Hence use a prior with a standard deviation.
				## The specific results are sometimes a bit sensitive to the uncertainty of the beta prior. 
				## This uncertainty is set corresponding to 10 times the median relative flux uncertainty.
				## The prior is weighted n times the observations in the cost.
				## Hence, overall it is using a weight of 1/10 of the weight of all observations.
				sdBetaPrior <- 10*medianRelFluxUncertainty*thetaPrior[2]/sqrt(nRec)
				c(k=NA, beta=as.vector(sdBetaPrior), alpha=NA, RRef=NA, E0=NA)
			}
} 
LightResponseCurveFitter$methods(getPriorScale = LightResponseCurveFitter_getPriorScale)

LightResponseCurveFitter_getParameterInitials <- function(
		### return the prior distribution of parameters
		thetaPrior 	##<< numeric vector prior estimate of parameters
){
	# only beta (second parameter)  is varied between different intial guesses
	##value<< a numeric matrix (3,nPar) of initial values for fitting parameters
	thetaInitials <- matrix( rep(thetaPrior,each=3), 3, length(thetaPrior), dimnames=list(NULL,names(thetaPrior)))
	thetaInitials [2,2] <- thetaPrior[2]*1.3
	thetaInitials [3,2] <- thetaPrior[2]*0.8
	thetaInitials 
}
LightResponseCurveFitter$methods( getParameterInitials = LightResponseCurveFitter_getParameterInitials)

LightResponseCurveFitter_optimLRCBounds <- function(
		### Optimize parameters of light response curve and refit with some fixed parameters if fitted parameters are outside bounds
		theta0			##<< initial parameter estimate
		,parameterPrior	##<< prior estimate of model parameters
		, ...			##<< further parameters to \code{.optimLRC}, such as \code{dsDay}
		,lastGoodParameters ##<< parameters vector of last successful fit
		, ctrl					##<< list of further controls
){
	##author<< TW, MM
	##seealso<< \code{\link{partGLFitLRC}}
	if( !is.finite(lastGoodParameters[3L]) ) lastGoodParameters[3L] <- 0.22	# twutz 161014: default alpha 	
	isUsingFixedVPD <- FALSE
	isUsingFixedAlpha <- FALSE
	getIOpt <- .self$getOptimizedParameterPositions
	resOpt <- resOpt0 <- .self$optimLRCOnAdjustedPrior(theta0, iOpt=getIOpt(isUsingFixedVPD, isUsingFixedAlpha), parameterPrior = parameterPrior, ctrl, ... )
	##details<<
	## If parameters alpha or k are outside bounds (Table A1 in Lasslop 2010), refit with some parameters fixed 
	## to values from fit of previous window.
	theta0Adj <- theta0	# intial parameter estimate with some parameters adjusted to bounds
	#dsDay <- list(ctrl, ...)$dsDay
#	# #details<< Sometimes the VPD-effect parameter is fitted to match the noise.
#	# # Hence, only fit with the VPD-parameter if predictions at low PAR are not much effected.
#	.tmp.inspectFixedVPDEffect <- function(){
#		resOptFixVPD <- .self$optimLRCOnAdjustedPrior(theta0, iOpt=getIOpt(isUsingFixedVPD=TRUE, isUsingFixedAlpha), parameterPrior = parameterPrior, ctrl, ... )
#		p <- theta0
#		p <- resOpt$theta
#		pFix <- resOptFixVPD$theta
#		dsDay <- list(...)$dsDay
#		dsDayLowPar <- dsDay
#		dsDayLowPar <- dsDay[dsDay$Rg <= 200,,drop=FALSE]
#		dsDayLowPar <- dsDayLowPar[order(dsDayLowPar$Rg),]
#		pred <- .self$predictLRC(p, Rg=dsDayLowPar$Rg, VPD=dsDayLowPar$VPD, Temp=dsDayLowPar$Temp)
#		predFix <- .self$predictLRC(pFix, Rg=dsDayLowPar$Rg, VPD=dsDayLowPar$VPD, Temp=dsDayLowPar$Temp)
#		plot( -NEE ~ Rg, dsDayLowPar)		# NEE negative?
#		lines(pred$NEP  ~ dsDayLowPar$Rg)
#		lines(predFix$NEP  ~ dsDayLowPar$Rg, col="blue")
#		lines(pred$GPP  ~ dsDayLowPar$Rg)
#		lines(predFix$GPP  ~ dsDayLowPar$Rg, col="blue")
#		# actually the GPP at low PAR is not much affects. The difference is in explaining the variability by
#		# respiration-T of modified GPP-VPD. Here, the VPD-based RRef is closer to the night-time estimated.
#	}
	if ((resOpt$theta[1L] < 0) ){
		isUsingFixedVPD <- TRUE
		theta0Adj[1L] <- 0
		resOpt <- .self$optimLRCOnAdjustedPrior(theta0Adj, iOpt=getIOpt(isUsingFixedVPD, isUsingFixedAlpha), parameterPrior = parameterPrior, ctrl, ... )
		# check alpha, if less than zero estimate parameters with fixed alpha of last window 
		if ( (resOpt$theta[3L] > 0.22) && is.finite(lastGoodParameters[3L]) ){
			isUsingFixedAlpha <- TRUE
			theta0Adj[3L] <- lastGoodParameters[3L] 
			resOpt <- .self$optimLRCOnAdjustedPrior(theta0Adj, iOpt=getIOpt(isUsingFixedVPD, isUsingFixedAlpha), parameterPrior = parameterPrior, ctrl, ... )
		}
	} else {
		# check alpha, if gt 0.22 estimate parameters with fixed alpha of last window
		# if not last window exists, let alpha > 0.22
		if ( (resOpt$theta[3L] > 0.22) && is.finite(lastGoodParameters[3L]) ){
			isUsingFixedAlpha <- TRUE
			theta0Adj[3L] <- lastGoodParameters[3L]
			resOpt <- .self$optimLRCOnAdjustedPrior(theta0Adj, iOpt=getIOpt(isUsingFixedVPD, isUsingFixedAlpha), parameterPrior = parameterPrior, ctrl, ... )
			# check k, if less than zero estimate parameters without VPD effect and with fixed alpha of last window 
			if (resOpt$theta[1L] < 0){
				isUsingFixedVPD <- TRUE
				theta0Adj[1L] <- 0
				resOpt <- .self$optimLRCOnAdjustedPrior(theta0Adj, iOpt=getIOpt(isUsingFixedVPD, isUsingFixedAlpha), parameterPrior = parameterPrior, ctrl, ... )
			}
		}
	} 
	##details<<
	## No parameters are reported if alpha<0 or RRef < 0 or beta0 < 0 or beta0 > 250 
	# positions in theta0: "k"     "beta0" "alpha"  "RRef"    "E0"
	if( !is.na(resOpt$theta[1L]) && ((resOpt$theta[3L] < 0) || (resOpt$theta[4L] < 0) || (resOpt$theta[2L] < 0) || (resOpt$theta[2L] >= 250)) ){
		# TODO estimate RRef from daytime data?
		#LloydT_E0fix
		#stop("case with alpha or beta < 0")
		resOpt$theta[] <- NA
	}
	##details<<
	## No parameters are reported if beta0 > 4*initialEstimate, to avoid cases where data is far away from saturation. 
	if( isTRUE(as.vector(resOpt$theta[2L] > 4*parameterPrior[2L])) ){
		resOpt$theta[] <- NA
	}
	##value<< list result of optimization as of \code{.optimLRC} with entries 
	## \item{theta}{ numeric parameter vector that includes the fixed components}
	## \item{iOpt}{ integer vector of indices of the vector that have been optimized}
	resOpt
}
LightResponseCurveFitter$methods(optimLRCBounds = LightResponseCurveFitter_optimLRCBounds)

LightResponseCurveFitter_getOptimizedParameterPositions <- function(
		### get the positions of the parameters to optimize for given conditions on fixing alpha or VPD effect 
		isUsingFixedVPD			##<< boolean scalar: if TRUE, VPD effect set to zero and is not optimized
		, isUsingFixedAlpha		##<< boolean scalar: if TRUE, initial slope is fixed and is not optimized
){
	##details<< If subclasses extend the parameter vector, they need to overide this method.
	iOpt <- 
			if( !isUsingFixedVPD & !isUsingFixedAlpha ) c(1:4) else
			if(  isUsingFixedVPD & !isUsingFixedAlpha ) 2:4 else
			if( !isUsingFixedVPD &  isUsingFixedAlpha ) c(1L,2L,4L) else
			if(  isUsingFixedVPD &  isUsingFixedAlpha ) c(2L,4L)
	##value<< integer vector of positions in parameter vector 
	iOpt
}
LightResponseCurveFitter$methods(getOptimizedParameterPositions = LightResponseCurveFitter_getOptimizedParameterPositions)


LightResponseCurveFitter_isParameterInBounds <- function(
		### Check if estimated parameter vector is within reasonable bounds
		theta					##<< estimate of parameter
		,sdTheta				##<< estimate of uncertainty of the parameter
		, RRefNight				##<< numeric scalar: night-time based estimate of basal respiration
		, ctrl					##<< list of further controls
){
	##author<< TW, MM
	#
	# check the Beta bounds that depend on uncertainty, set to NA fit
	if(isTRUE(as.vector( (theta[2] > 100) && (sdParms[2] >= theta[2]) ))) return(FALSE)
	# check that RRef estimated from daytime is not both:
	# larger than twice the estimate from nighttime and more than 0.7 in absolute terms  
	# else this indicates a bad fit
	# this is additional to Table A1 in Lasslop 2010
	if( (theta[4L] > 2*RRefNight) 		&& 
			((theta[4L]-RRefNight) > 0.7) 
			){
		return(FALSE)
	}
	##value<< FALSE if parameters are outisde reasonable bounds, TRUE otherwise 
	return(TRUE)
}
LightResponseCurveFitter$methods(isParameterInBounds = LightResponseCurveFitter_isParameterInBounds)


LightResponseCurveFitter_optimLRC <- function(
		### call the optimization function
		theta				##<< numeric vector: starting parameters
		,iOpt				##<< integer vector: positions of paramters to optimize
		,sdParameterPrior	##<< numeric vector: prior uncertainty
		, ...				##<< further arguments to the cost function
		, ctrl				##<< list of further controls
		, isUsingHessian	##<< scalar boolean: set to TRUE to compute Hessian at optimum
){
	# do a first fitting with a strong prior to avoid local side minima, only afterwards use fit with a weaker prior
	# strong prior only modified beta and alpha (2nd, and 3rd parameters)
	thetaOrig <- theta
	sdStrongPrior <- sdParameterPrior; sdStrongPrior[2] <- sdParameterPrior[2]/10; sdStrongPrior[3] <- 0.5
	#
	resOptimStrongPrior <- optim(thetaOrig[iOpt], .self$computeCost
			#tmp <- .partGLRHLightResponseCost( theta[iOpt], 
			,theta=thetaOrig
			,iOpt=iOpt
			,sdParameterPrior = sdStrongPrior
			, ...
			,control=list(reltol=ctrl$LRCFitConvergenceTolerance)
			,method="BFGS", hessian=isUsingHessian)
	
	thetaOrig[iOpt] <- resOptimStrongPrior$par	
	#
	resOptim <- optim(thetaOrig[iOpt], .self$computeCost
			#resOptim <- optim(theta, .partGLRHLightResponseCost
			#tmp <- .partGLRHLightResponseCost( theta[iOpt] 
			,theta=thetaOrig
			,iOpt=iOpt
			,sdParameterPrior = sdParameterPrior
			, ...
			,control=list(reltol=ctrl$LRCFitConvergenceTolerance)
			,method="BFGS", hessian=isUsingHessian)
	##value<<
	## list of restult of \code{\link{optim}} amended with list
	thetaOpt <- theta; thetaOpt[iOpt] <- resOptim$par
	ans <- list(
			theta = thetaOpt	##<< numeric vector: optimized parameter vector including the fixed components
			,iOpt = iOpt		##<< integer vector: position of parameters that have been optimized
	)
	c(resOptim, ans)
} 
LightResponseCurveFitter$methods(optimLRC = LightResponseCurveFitter_optimLRC)


LightResponseCurveFitter_computeCost <- function(
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
LightResponseCurveFitter$methods( computeCost = LightResponseCurveFitter_computeCost )

LightResponseCurveFitter_predictLRC <- function(
		### Light Response Function
		theta   ##<< numeric vector of parameters
		,Rg   	##<< ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation
		,VPD 	##<< VPD [numeric] -> Vapor Pressure Deficit [hPa]
		,Temp 	##<< Temp [degC] -> Temperature [degC] 
		,VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010
		,fixVPD = FALSE   	##<< fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered
		,TRef=15			##<< numeric scalar of Temperature (degree Celsius) for reference respiration RRef
){
	##details<<
	## Predict ecosystem fluxes (Reco, GPP, NEP=GPP-Reco) for given parameters and environmental conditions.
	## 
	## Currently ther are several subclasses that provide alternatives: 
	## \begin{itemize}
	## \item Rectangular: \code{\link{RectangularLRCFitter_predictLRC}}
	## \item Nonrectangular: \code{\link{NonrectangularLRCFitter_predictLRC}}
	## \item Rectangular: \code{\link{LogisticSigmoidLRCFitter_predictLRC}}
	## \end{itemize}
	#
	##details<<
	## The VPD effect is included according to Lasslop et al., 2010.
	## This function generalizes the \code{\link{RectangularLRCFitter_predictLRC}} by adding a convexity parameter.
	##details<<
	## If theta is a matrix, a different row of parameters is used for different entries of other inputs
	if( is.matrix(theta) ){
		kVPD<-theta[,1]
		beta<-theta[,2]
		alpha<-theta[,3]
		RRef<-theta[,4]
		E0<-theta[,5]
	} else {
		kVPD<-theta[1]
		beta<-theta[2]
		alpha<-theta[3]
		RRef<-theta[4]
		E0<-theta[5]
	}
	Amax <- if( isTRUE(fixVPD) ) beta else {
				ifelse(VPD > VPD0, beta*exp(-kVPD*(VPD-VPD0)), beta)
			} 
	Reco<-RRef*exp(E0*(1/((273.15+TRef)-227.13)-1/(Temp+273.15-227.13)))
	GPP <- .self$predictGPP(Rg, Amax=Amax, alpha=alpha)
	NEP <- GPP - Reco
	## a data.frame of length of Rg of computed  
	ans <- list(
			NEP=NEP		##<< Net ecosystem production (-NEE), vector of length(Rg)
			,Reco=Reco	##<< Ecosystem respiration 
			,GPP=GPP	##<< Gross primary production
	)
}
LightResponseCurveFitter$methods(	predictLRC = LightResponseCurveFitter_predictLRC)

LightResponseCurveFitter_predictGPP  <- function(
		### Logistic Sigmoid Light Response function for GPP
		Rg   	##<< ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation
		,...	##<< further parameters to the LRC

) {
	stop("Abstract method. Need to define in derived LRC class.")
	##value<<
}
LightResponseCurveFitter$methods( predictGPP = LightResponseCurveFitter_predictGPP)


LightResponseCurveFitter_computeLRCGradient <- function(
		### Gradient of \code{\link{LightResponseCurveFitter_predictLRC}}
		theta 	##<< theta [numeric] -> parameter vector (theta[1]=k (k), theta[2]=beta (beta), theta[3]=alpha, theta[4]=RRef (rb), theta[4]=E0)
		,Rg   	##<< ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation
		,VPD 	##<< VPD [numeric] -> Vapor Pressure Deficit [hPa]
		,Temp 	##<< Temp [degC] -> Temperature [degC] 
		,VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010
		,fixVPD = FALSE   	##<< fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered
		,TRef=15			##<< numeric scalar of Temperature (degree Celsius) for reference respiration RRef
) {
	if( is.matrix(theta) ){
		k<-theta[,1]
		beta<-theta[,2]
		alpha<-theta[,3]
		RRef<-theta[,4]
		E0<-theta[,5]
	} else {
		k<-theta[1]
		beta<-theta[2]
		alpha<-theta[3]
		RRef<-theta[4]
		E0<-theta[5]
	}
	Amax <- if( isTRUE(fixVPD) ) beta else {
				ifelse(VPD > VPD0, beta*exp(-k*(VPD-VPD0)), beta)
			}
	#ex <- expression( beta*exp(-k*(VPD-VPD0)) ); deriv(ex,c("beta","k"))
	dAmax_dkVPD <- if( isTRUE(fixVPD) ) 0 else {
				ifelse(VPD > VPD0, beta*-(VPD-VPD0)*exp(-k*(VPD-VPD0)), 0)
			} 
	dAmax_dbeta0 <- if( isTRUE(fixVPD) ) 0 else {
				ifelse(VPD > VPD0, exp(-k*(VPD-VPD0)), 1)
			} 
	#Reco<-RRef*exp(E0*(1/((273.15+10)-227.13)-1/(Temp+273.15-227.13)))
	#ex <- expression( RRef*exp(E0*(1/((273.15+TRef)-227.13)-1/(Temp+273.15-227.13))) ); deriv(ex,c("RRef","E0"))
	.expr7 <- 1/(273.15 + TRef - 227.13) - 1/(Temp + 273.15 - 227.13)
	.expr9 <- exp(E0 * .expr7)
	gradReco <- matrix(0, ncol=2L, nrow=length(.expr9), dimnames=list(NULL,c("RRef","E0")))
	gradReco[,"RRef"] <- dReco_dRRef <- .expr9
	gradReco[,"E0"] <- dReco_dE0 <- RRef * (.expr9 * .expr7)
	#
	gradGPP <- array(0, c(nrow(gradReco), 3L), list(NULL, c("k","beta","alpha")))
	dGPP_dAMax <- .self$computeGPPGradient(Rg, Amax, alpha)
	gradGPP[, "beta"] <- dGPP_dAMax[,1] * dAmax_dbeta0 
	gradGPP[, "k"] <- dGPP_dAMax[,1] * dAmax_dkVPD 
	gradGPP[, "alpha"] <- dGPP_dAMax[,2]
	#NEP <- GPP - Reco
	gradNEP <- cbind(gradGPP, -gradReco)
	## list with gradient matrices. For each record (length(Rg)), c("k","beta","alpha","RRef")
	ans <- list(
			NEP=gradNEP
			,Reco=gradReco
			,GPP=gradGPP
	)
}
LightResponseCurveFitter$methods(computeLRCGradient = LightResponseCurveFitter_computeLRCGradient)

