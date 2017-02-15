



partGLFitLRC_NRHRF <- function(
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
	namesPars <- c("k","beta0", "alfa", "Rb","E0", "conv")  
	nPar <- length(namesPars)
	##details<<
	## Optimization is performed for three initial parameter sets that differ by beta0 (*1.3, *0.8).
	## From those three, the optimization result is selected that yielded the lowest misfit.
	## Starting values are: k=0, beta=interpercentileRange(0.03,0.97) of respiration, alpha=0.1, R_ref 
	## from nightTime estimate.
	## E0 is fixed to the night-time estimate, but varied for estimating parameter uncertainty.
	theta.V.n<-matrix(NA, 3,nPar, dimnames=list(NULL,namesPars))
	parameterPrior <- theta.V.n[1,] <- theta.V.n[2,] <- theta.V.n[3,] <- c(
			k=0
			#,beta=as.vector(abs(quantile(dsDay$NEE, 0.03)-quantile(dsDay$NEE, 0.97)))
			,beta=as.vector(abs(quantile(dsDay$NEE, 0.03, na.rm=TRUE)-quantile(dsDay$NEE, 0.97, na.rm=TRUE)))
			,alpha=0.1
			#,R_ref=mean(NEENight.V.n, na.rm=T)
			,R_ref=if( is.finite(RRefNight) ) as.vector(RRefNight) else stop("must provide finite RRefNight") #mean(NEENight.V.n, na.rm=T)
			,E_0=as.vector(E0)
			,conv=0.2
	)   #theta [numeric] -> parameter vector (theta[1]=kVPD, theta[2]-beta0, theta[3]=alfa, theta[4]=Rref)
	#twutz: beta is quite well defined, so try not changing it too much
	theta.V.n[2,2] <- parameterPrior[2]*1.3
	theta.V.n[3,2] <- parameterPrior[2]*0.8
	#
	##seealso<< \code{\link{parmGLOptimLRCBounds}}
	resOpt3 <- apply( theta.V.n, 1, function(theta0){
				resOpt <- parmGLOptimLRCBounds(theta0, parameterPrior, dsDay=dsDay, ctrl=controlGLPart, lastGoodParameters.V.n=lastGoodParameters )
			})
	iValid <- which(sapply(resOpt3,function(resOpt){ is.finite(resOpt$theta[1]) }))
	resOpt3Valid <- resOpt3[iValid]
	optSSE <- sapply(resOpt3Valid, "[[", "value")
	if( sum(!is.na(optSSE)) == 0L ){
		# none of the intial fits yielded valid result, create NA-return values
		opt.parms.V <- structure( rep(NA_real_, nPar), names=colnames(theta.V.n) )
		covParms <- matrix(NA_real_, nPar, nPar, dimnames=list(colnames(theta.V.n),colnames(theta.V.n)))
		resOpt <- list(iOpt = integer(0))	
	} else {
		resOpt <- resOpt3Valid[[iBest <- which.min(optSSE)]] # select the one with the least cost
		opt.parms.V<-resOpt$theta
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
			covParms <- structure( diag(0,nrow=length(resOpt$theta)), dimnames=list(namesPars,namesPars))
			covParms[5L,5L] <- sdE0^2
			covParms[resOpt$iOpt,resOpt$iOpt] <- covParmsLRC  
			if( any(diag(covParms) < 0)) opt.parms.V[] <- NA	# no real if covariance negative
		} else {
			##details<< 
			## If \code{controlGLPart.l$nBootUncertainty > 0L} then the covariance matrix of the 
			## parameters is estimated by a bootstrap of the data.
			## In each draw, E0 is drawn from N ~ (E_0, sdE_0).
			# #seealso<< \code{\link{.bootStrapLRCFit}}
			resBoot  <- .bootStrapLRCFit(resOpt$theta, resOpt$iOpt, dsDay, sdE0, parameterPrior, controlGLPart)
			#resBoot  <- .bootStrapLRCFit(resOpt$theta, resOpt$iOpt, dsDay, sdE_0.n, parameterPrior, controlGLPart.l=within(controlGLPart.l,nBootUncertainty <- 30L))
			covParms <- cov(resBoot)
			#better not to average parameters
			#opt.parms.V <- apply(resBoot, 2, median, na.rm=TRUE)
			#se.parms.V <- apply(resBoot, 2, sd, na.rm=TRUE)
		}
	}
	##value<< a list, If none of the optimizations from different starting conditions converged,
	## the parameters are NA
	ans <- list(
			opt.parms.V=opt.parms.V		##<< numeric vector of optimized parameters including 
			## the fixed ones and including E0
			,iOpt=c(resOpt$iOpt,5L)		##<< index of parameters that have been optimized
			## , here including E0, which has been optimized prior to this function.
			,initialGuess.parms.V.n=theta.V.n[1,]	##<< the initial guess from data
			,covParms=covParms			##<< numeric matrix of the covariance matrix of parameters, including E0
	)
}



.optimNRHRF <- function(
		###<< one fit of the nonrectangular light response curve with a subset of parameters depending on which are fixed
		theta  					##<< numeric vector of starting values
		, isUsingFixedVPD=FALSE, isUsingFixedAlpha=FALSE	##<< selection of which parameters to optimize
		, dsDay					##<< dataframe of NEE, sdNEE and predictors Rg, VPD and Temp
		#, E_0.n				##<< temperature sensitivity
		, parameterPrior		##<< prior parameter estimates
		, ctrl					##<< list of further controls
){
	if( !all(is.finite(theta))) stop("need to provide finite starting values.")
	##details<<
	## The nonrectangular has similar parameters as the rectangular, but adds a convexity parameter called conv (at the 6th position)
	## Only those records are used for optimization where both NEE and sdNEE are finite.
	dsDayFinite <- dsDay[ is.finite(dsDay$NEE) & is.finite(dsDay$sdNEE), ]
	##details<<
	## Optimization of NRHRF  curve parameters takes into account the uncertainty of the flux values.
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
	##details<< 
	## The beta parameter is quite well defined. Hence use a prior with a standard deviation.
	## The specific results are sometimes a bit sensitive to the uncertainty of the beta prior. 
	## This uncertainty is set corresponding to 10 times the median relative flux uncertainty.
	## The prior is weighted n times the observations in the cost.
	## Hence, overall it is using a weight of 1/10 of the weight of all observations.
	sdParameterPrior <- if(ctrl$isLasslopPriorsApplied){
				c(k=50, beta=600, alpha=10, Rb=80, E0=NA, conv=20)
			} else {
				medianRelFluxUncertainty <- abs(median(Fc_unc/dsDayFinite$NEE))
				sdBetaPrior <- 10*medianRelFluxUncertainty*parameterPrior[2]/sqrt(nrow(dsDayFinite))
				c(k=NA, beta=as.vector(sdBetaPrior), alpha=NA, Rb=NA, E0=NA, conv=NA)
			}
	isUsingHessian <- (ctrl$nBootUncertainty==0L)
	iOpt <- 
			if( !isUsingFixedVPD & !isUsingFixedAlpha ) c(1:4) else
			if(  isUsingFixedVPD & !isUsingFixedAlpha ) 2:4 else
			if( !isUsingFixedVPD &  isUsingFixedAlpha ) c(1L,2L,4L) else
			if(  isUsingFixedVPD &  isUsingFixedAlpha ) c(2L,4L) 
	iOpt <- c(iOpt,6)	# add the convexity parameter
	sdParameterPrior[-iOpt] <- NA
	#
	#Define lower and upper boundaries parameters
	lower_theta<-c(0,0,0,0,0, 0.0001)
	upper_theta<-c(Inf,Inf,Inf,Inf,Inf,1)
	#browser()
	# encountered the case where fit ran into a local optimum at very low (even negative) beta
	# hence first do a fit with log(beta) or a fit with very strong prior on beta and alpha
	# and relax priors afterwards.
	#	thetaLog <- theta; thetaLog[2] <- log(theta[2])
	#	parameterPriorLog <- parameterPrior; parameterPriorLog[2] <- log(parameterPrior[2])
	#	# for the log-Fit assume a strong prior on beta to avoid local minimum at beta=0
	#	sdParameterPriorLog <- sdParameterPrior  #; sdParameterPriorLog[2] <- log(parameterPrior[2]) - log(parameterPrior[2] - sdParameterPrior[2])
	#	resOptimLog <- optim(thetaLog[iOpt], .partGLRHLightResponseCostLogBeta
	#			#tmp <- .partGLRHLightResponseCost( theta[iOpt], 
	#			,theta=thetaLog
	#			,iOpt=iOpt
	#			,flux = -dsDayFinite$NEE 
	#			,sdFlux = Fc_unc	  
	#			,parameterPrior = parameterPriorLog
	#			,sdParameterPrior = sdParameterPriorLog
	#			,Rg = dsDayFinite$Rg 
	#			,VPD = dsDayFinite$VPD
	#			,Temp=dsDayFinite$Temp
	#			#,E0 = E_0.n
	#			,control=list(reltol=ctrl$LRCFitConvergenceTolerance)
	#			,method="BFGS", hessian=isUsingHessian)
	#	thetaOrig <- theta; thetaOrig[iOpt] <- resOptimLog$par; thetaOrig[2] <- exp(thetaOrig[2])	
	#
	thetaOrig <- theta
	# do a first fitting with a strong prior to avoid local side minima, only afterwards use fit with a weaker prior
	sdStrongPrior <- sdParameterPrior; sdStrongPrior[2] <- sdParameterPrior[2]/10; sdStrongPrior[3] <- 0.5
	#
	resOptimStrongPrior <- optim(thetaOrig[iOpt], .partGLNRHRFCost
			#tmp <- .partGLRHLightResponseCost( theta[iOpt], 
			,theta=thetaOrig
			,iOpt=iOpt
			,flux = -dsDayFinite$NEE 
			,sdFlux = Fc_unc	  
			,parameterPrior = parameterPrior
			,sdParameterPrior = sdStrongPrior
			,Rg = dsDayFinite$Rg 
			,VPD = dsDayFinite$VPD
			,Temp=dsDayFinite$Temp
			#,E0 = E_0.n
			,control=list(factr=ctrl$nLRCFitConvergenceTolerance)
			,method="L-BFGS-B"
			,lower = lower_theta[iOpt], upper = upper_theta[iOpt]
			,hessian=isUsingHessian)  #MIRCO Changed
	
	thetaOrig[iOpt] <- resOptimStrongPrior$par	
	#
	resOptim <- optim(thetaOrig[iOpt], .partGLNRHRFCost
			#resOptim <- optim(theta, .partGLRHLightResponseCost
			#tmp <- .partGLRHLightResponseCost( theta[iOpt] 
			,theta=thetaOrig
			,iOpt=iOpt
			,flux = -dsDayFinite$NEE 
			,sdFlux = Fc_unc	  
			,parameterPrior = parameterPrior
			,sdParameterPrior = sdParameterPrior
			,Rg = dsDayFinite$Rg 
			,VPD = dsDayFinite$VPD
			,Temp=dsDayFinite$Temp
			#,E0 = E_0.n
			,control=list(factr=ctrl$nLRCFitConvergenceTolerance)
			,method="L-BFGS-B"
			,lower = lower_theta[iOpt], upper = upper_theta[iOpt]
			,hessian=isUsingHessian)  #MIRCO CHANGED
	##value<<
	## list of restult of \code{\link{optim}} amended with list
	thetaOpt <- theta; thetaOpt[iOpt] <- resOptim$par
	ans <- list(
			theta = thetaOpt	##<< numeric vector: optimized parameter vector including the fixed components
			,iOpt = iOpt		##<< integer vector: position of parameters that have been optimized
	)
	c(resOptim, ans)
}


partGL_NRHLightResponse <- function(
		### Nonrectangular Rectungular Hyperbolic Light Response function: (Gilmanov et al., 2003)
		theta   ##<< theta [numeric] -> parameter vector (theta[1]=kVPD (k), theta[2]=beta0 (beta), theta[3]=alfa, theta[4]=Rref (rb), theta[5]=E0, theta[6]=conv)
		##<< E0: Temperature sensitivity ("activation energy") in Kelvin (degK) #get("testparams", envir=environment(foo)
		,Rg   	##<< ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation
		,VPD 	##<< VPD [numeric] -> Vapor Pressure Deficit [hPa]
		,Temp 	##<< Temp [degC] -> Temperature [degC] 
		#,E0 	##<< Temperature sensitivity ("activation energy") in Kelvin (degK) #get("testparams", envir=environment(foo)
		,VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010
		,fixVPD = FALSE   	##<< fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered
) {
	##details<<
	## with fixes E_0.n for the estimation of Rd
	## VPD effect included according to Lasslop et al., 2010
	##details<<
	## If theta is a matrix, a different row of parameters is used for different entries of other inputs
	if( is.matrix(theta) ){
		kVPD<-theta[,1]
		beta0<-theta[,2]
		alfa<-theta[,3]
		Rref<-theta[,4]
		E0<-theta[,5]
		conv<-theta[,6]
	} else {
		kVPD<-theta[1]
		beta0<-theta[2]
		alfa<-theta[3]
		Rref<-theta[4]
		E0<-theta[5]
		conv<-theta[6]
	}
	Amax <- if( isTRUE(fixVPD) ) beta0 else {
				ifelse(VPD > VPD0, beta0*exp(-kVPD*(VPD-VPD0)), beta0)
			} 
	#browser()
	#Reco<-Rref*exp(E0*(1/((273.15+10)-227.13)-1/(Temp+273.15-227.13)))
	#print(Temp)
	#print(VPD)
	#print(Rg)
	Reco<-Rref*exp(E0*(1/((273.15+15)-227.13)-1/(Temp+273.15-227.13))) 
	
	zRoot<-((alfa*Rg+Amax)^2)-(4*alfa*Rg*conv*Amax)
	zRoot[which(zRoot<0)]<-0
	GPP<-(1/(2*conv))*(alfa*Rg+Amax-sqrt(zRoot))
	#browser()
	NEP <- GPP - Reco
	#print(NEP)
	#print(GPP)
	## a data.frame of length of Rg of computed  
	ans <- list(
			NEP=NEP		##<< Net ecosystem production (-NEE), vector of length(Rg)
			,Reco=Reco	##<< Ecosystem respiration 
			,GPP=GPP	##<< Gross primary production
	)
}


partGL_NRHLightResponseGrad <- function(
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
	#TODO: test and correct
	if( is.matrix(theta) ){
		kVPD<-theta[,1]
		beta0<-theta[,2]
		alfa<-theta[,3]
		Rref<-theta[,4]
		E0<-theta[,5]
		conv<-theta[,6]
	} else {
		kVPD<-theta[1]
		beta0<-theta[2]
		alfa<-theta[3]
		Rref<-theta[4]
		E0<-theta[5]
		conv<-theta[6]
	}
	Amax <- if( isTRUE(fixVPD) ) beta0 else {
				ifelse(VPD > VPD0, beta0*exp(-kVPD*(VPD-VPD0)), beta0)
			}
	#ex <- expression( beta0*exp(-kVPD*(VPD-VPD0)) ); deriv(ex,c("beta0","kVPD"))
	dAmax_dkVPD <- if( isTRUE(fixVPD) ) 0 else {
				ifelse(VPD > VPD0, beta0*-(VPD-VPD0)*exp(-kVPD*(VPD-VPD0)), 0)
			} 
	dAmax_dbeta0 <- if( isTRUE(fixVPD) ) 0 else {
				ifelse(VPD > VPD0, exp(-kVPD*(VPD-VPD0)), 1)
			} 
	#Reco<-Rref*exp(E0*(1/((273.15+10)-227.13)-1/(Temp+273.15-227.13)))
	#ex <- expression( Rref*exp(E0*(1/((273.15+10)-227.13)-1/(Temp+273.15-227.13))) ); deriv(ex,c("Rref","E0"))
	#.expr7 <- 1/(273.15 + 10 - 227.13) - 1/(Temp + 273.15 - 227.13)
	.expr7 <- 1/(273.15 + 15 - 227.13) - 1/(Temp + 273.15 - 227.13)
	.expr9 <- exp(E0 * .expr7)
	gradReco <- matrix(0, ncol=2L, nrow=length(.expr9), dimnames=list(NULL,c("Rref","E0")))
	gradReco[,"Rref"] <- dReco_dRRef <- .expr9
	gradReco[,"E0"] <- dReco_dE0 <- Rref * (.expr9 * .expr7)
	#GPP <- (Amax*alfa*Rg)/(alfa*Rg+Amax)
	#ex <- expression( (Amax*alfa*Rg)/(alfa*Rg+Amax) ); deriv(ex,c("Amax","alfa"))
	.expr2 <- Amax * alfa * Rg
	.expr3 <- alfa * Rg
	.expr4 <- .expr3 + Amax
	.expr7 <- .expr4^2
	.value <- .expr2/.expr4
	gradGPP <- array(0, c(length(.value), 3L), list(NULL, c("kVPD","beta0","alfa")))
	dGPP_dAMax <- .expr3/.expr4 - .expr2/.expr7
	gradGPP[, "beta0"] <- dGPP_dAMax * dAmax_dbeta0 
	gradGPP[, "kVPD"] <- dGPP_dAMax * dAmax_dkVPD 
	gradGPP[, "alfa"] <- Amax * Rg/.expr4 - .expr2 * Rg/.expr7
	#NEP <- GPP - Reco
	gradNEP <- cbind(gradGPP, -gradReco)
	## list with gradient matrices. For each record (length(Rg)), c("kVPD","beta0","alfa","Rref")
	ans <- list(
			NEP=gradNEP
			,Reco=gradReco
			,GPP=gradGPP
	)
}

.partGLNRHRFCost <- function(
		### Computing residual sum of sqares for predictions vs. data of NEE
		thetaOpt   ##<< parameter vecotr with components of theta0 that are optimized 
		,theta		##<< parameter vector with positions as in argument of \code{\link{partGL_RHLightResponse}} 
		,iOpt		##<< position in theta0 that are optimized 
		,flux=NA 	##<< numeric: NEP (-NEE) or GPP time series [umolCO2/m2/s], should not contain NA
		,sdFlux=NA 	##<< numeric: standard deviation of Flux [umolCO2/m2/s], should not contain NA
		,parameterPrior		##<< numeric vector along theta: prior estimate of parameter (range of values)
		,sdParameterPrior	##<< standard deviation of parameterPrior
		,...			##<< other arguments to \code{\link{partGL_RHLightResponse}}
		,VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010
		,fixVPD = FALSE   	##<< fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered
		,useCVersion=FALSE	##<< set to FALSE  to use R instead of fast C version, e.g. for debugging
# also tried to pass dsDay data.frame instead of all the variables separately, but accessing data.frame
# columns in the cost function was a severe penalty (here double execution time)
) {
	theta[iOpt] <- thetaOpt
	#if( FALSE ){
	if( useCVersion){
		# For Thomas: we need to implement this in C for NRHRF right?
		#RHLightResponseCostC(theta, flux, sdFlux, parameterPrior, sdParameterPrior, ..., VPD0=VPD0, fixVPD=fixVPD)		
	} else {
		#browser()
		#print(theta)
		resPred <- partGL_NRHLightResponse(theta, ..., VPD0=VPD0, fixVPD=fixVPD)
		NEP_mod <- resPred$NEP
		#if(is.na(mean(NEP_mod))==TRUE) {
		#  browser()
		#}
		misFitPrior <- (((theta - parameterPrior))/(sdParameterPrior))^2
		misFitObs <- sum(((NEP_mod-flux)/sdFlux)^2)
		RSS <- misFitObs + sum(misFitPrior, na.rm=TRUE)
		#if( !is.finite(RSS) ) recover()	# debugging the fit
		RSS
	}
}



