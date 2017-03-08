

NonrectangularLRCFitter <- setRefClass('NonrectangularLRCFitter', contains='LightResponseCurveFitter'
## R5 reference class for the Rectangular Light response curve
##author<<
## TW, MM
)	

NonrectangularLRCFitter_getParameterNames <- function(
### return the parameter names used by this Light Response Curve Funciton
){
	##value<< string vector of parameter names. Positions are important.
	c(k="k"						##<< TODO				
			, beta="beta"				##<< saturation of GPP at high radiation
			, alpha="alpha"				##<< initial slope
			, RRef="RRef"				##<< basal respiration 
			, E0="E0"					##<< temperature sensitivity estimated from night-time data
			, logitconf="logitconv")	##<< logit-transformed convexity parameter. The valua at original scale is obtained by conv = 1/(1+exp(-logitconv))
}
NonrectangularLRCFitter$methods(getParameterNames = NonrectangularLRCFitter_getParameterNames)


NonrectangularLRCFitter$methods(
getPriorLocation = function(
		### return the prior distribution of parameters
		NEEDay 		##<< numeric vector of daytime NEE
		,RRefNight	##<< numeric scalar of basal respiration estimated from night-time data
		,E0			##<< numeric scalar of night-time estimate of temperature sensitivity
){
	##value<< a numeric vector with prior estimates of the parameters
	parameterPrior <- c(
			k=0
			#,beta=as.vector(abs(quantile(NEEDay, 0.03)-quantile(NEEDay, 0.97)))
			,beta=as.vector(abs(quantile(NEEDay, 0.03, na.rm=TRUE)-quantile(NEEDay, 0.97, na.rm=TRUE)))
			,alpha=0.1
			#,RRef=mean(NEENight.V.n, na.rm=T)
			,RRef=if( is.finite(RRefNight) ) as.vector(RRefNight) else stop("must provide finite RRefNight") #mean(NEENight.V.n, na.rm=T)
			,E0=as.vector(E0)
			,logitconv=logit(0.2)
	)   
})

NonrectangularLRCFitter$methods(
getPriorScale = function(
		### return the prior distribution of parameters
		thetaPrior 	##<< numeric vector of location of priors
		,medianRelFluxUncertainty	##<< numeric scalar: median across the relative uncertainty of the flux values, i.e. sdNEE/NEE
		,nRec		##<< integer scalar: number of finite observations
		, ctrl		##<< list of further controls, with entry \code{isLasslopPriorsApplied}	
){
	##value<< a numeric vector with prior estimates of the parameters
	sdParameterPrior <- if(ctrl$isLasslopPriorsApplied){
				c(k=50, beta=600, alpha=10, RRef=80, E0=NA, logitconv=NA)	#twutz: changed to no prior for logitconv
			} else {
				##details<< 
				## The beta parameter is quite well defined. Hence use a prior with a standard deviation.
				## The specific results are sometimes a bit sensitive to the uncertainty of the beta prior. 
				## This uncertainty is set corresponding to 10 times the median relative flux uncertainty.
				## The prior is weighted n times the observations in the cost.
				## Hence, overall it is using a weight of 1/10 of the weight of all observations.
				sdBetaPrior <- 10*medianRelFluxUncertainty*thetaPrior[2]/sqrt(nRec)
				c(k=NA, beta=as.vector(sdBetaPrior), alpha=NA, RRef=NA, E0=NA, logitconv=NA)
			}
})

NonrectangularLRCFitter$methods(
getParameterInitials = function(
		### return the prior distribution of parameters
		thetaPrior 	##<< numeric vector prior estimate of parameters
){
	# only beta0 is varied between different intial guesses
	##value<< a numeric matrix (3,nPar) of initial values for fitting parameters
	thetaInitials <- matrix( rep(thetaPrior,each=3), 3, length(thetaPrior), dimnames=list(NULL,names(thetaPrior)))
	thetaInitials [2,2] <- parameterPrior[2]*1.3
	thetaInitials [3,2] <- parameterPrior[2]*0.8
	thetaInitials 
})

NonrectangularLRCFitter_optimLRCBounds <- function(
		### Optimize parameters of light response curve and refit with some fixed parameters if fitted parameters are outside bounds
		theta0			##<< initial parameter estimate
		,parameterPrior	##<< prior estimate of model parameters
		, ...			##<< further parameters to \code{.optimLRC}, such as \code{dsDay}
		,lastGoodParameters.V.n ##<< parameters vector of last successful fit
		, ctrl					##<< list of further controls
){
	##author<< TW, MM
	##seealso<< \code{\link{partGLFitLRC}}
	# optimLRC <- if( ctrl$NRHRfunction ) .optimNRHRF else .optimLRC # now called on method with supplying LRC
	if( !is.finite(lastGoodParameters.V.n[3L]) ) lastGoodParameters.V.n[3L] <- 0.22	# twutz 161014: default alpha 	
	isUsingFixedVPD <- FALSE
	isUsingFixedAlpha <- FALSE
	getIOpt <- function( isUsingFixedVPD, isUsingFixedAlpha){
		iOpt <- 
				if( !isUsingFixedVPD & !isUsingFixedAlpha ) c(1:4) else
				if(  isUsingFixedVPD & !isUsingFixedAlpha ) 2:4 else
				if( !isUsingFixedVPD &  isUsingFixedAlpha ) c(1L,2L,4L) else
				if(  isUsingFixedVPD &  isUsingFixedAlpha ) c(2L,4L) 
		iOpt <- c(iOpt,6)	# add the convexity parameter
	}
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
	if ((resOpt$theta[1L] < 0) || (FALSE) || (FALSE)){
		isUsingFixedAlpha <- TRUE
		theta0Adj[1L] <- 0
		resOpt <- .self$optimLRCOnAdjustedPrior(theta0Adj, iOpt=getIOpt(isUsingFixedVPD, isUsingFixedAlpha), parameterPrior = parameterPrior, ctrl, ... )
		# check alpha, if less than zero estimate parameters with fixed alpha of last window 
		if ( (resOpt$theta[3L] > 0.22) && is.finite(lastGoodParameters.V.n[3L]) ){
			isUsingFixedVPD <- TRUE
			theta0Adj[3L] <- lastGoodParameters.V.n[3L] 
			resOpt <- .self$optimLRCOnAdjustedPrior(theta0Adj, iOpt=getIOpt(isUsingFixedVPD, isUsingFixedAlpha), parameterPrior = parameterPrior, ctrl, ... )
		}
	} else {
		# check alpha, if gt 0.22 estimate parameters with fixed alpha of last window
		# if not last window exists, let alpha > 0.22
		if ( (resOpt$theta[3L] > 0.22) && is.finite(lastGoodParameters.V.n[3L]) ){
			isUsingFixedVPD <- TRUE
			theta0Adj[3L] <- lastGoodParameters.V.n[3L]
			resOpt <- .self$optimLRCOnAdjustedPrior(theta0Adj, iOpt=getIOpt(isUsingFixedVPD, isUsingFixedAlpha), parameterPrior = parameterPrior, ctrl, ... )
			# check k, if less than zero estimate parameters without VPD effect and with fixed alpha of last window 
			if (resOpt$theta[1L] < 0){
				isUsingFixedAlpha <- TRUE
				theta0Adj[1L] <- 0
				resOpt <- .self$optimLRCOnAdjustedPrior(theta0Adj, iOpt=getIOpt(isUsingFixedVPD, isUsingFixedAlpha), parameterPrior = parameterPrior, ctrl, ... )
			}
		}
	} 
	##details<<
	## No parameters are reported if alpha<0 or RRef < 0 or beta0 < 0 or beta0 > 250 
	# positions in theta0: "k"     "beta0" "alfa"  "RRef"    "E0"
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
NonrectangularLRCFitter$methods( optimLRCBounds = NonrectangularLRCFitter_optimLRCBounds )

NonrectangularLRCFitter_isParameterInBounds <- function(
		### Check if estimated parameter vector is within reasonable bounds
		theta					##<< numeric vector of estimated parameters
		,sdTheta				##<< numeric vector of estimated standard deviation of the parameters
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
NonrectangularLRCFitter$methods(isParameterInBounds = NonrectangularLRCFitter_isParameterInBounds)


NonrectangularLRCFitter_optimLRC <- function(
		###<< calling the optimization function
		theta  					##<< numeric vector of starting values
		, iOpt					##<< integer vector of positions of parameters being optimized
		, sdParameterPrior		##<< numeric vector of scale of parameter priors
		, ...					##<< further arguments to the cost function
		, ctrl					##<< list of further controls
		, isUsingHessian		##<< scalar boolean: set to TRUE to compute Hessian at optimum
){
	#Define lower and upper boundaries parameters
	thetaOrig <- theta
	# do a first fitting with a strong prior to avoid local side minima, only afterwards use fit with a weaker prior
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
NonrectangularLRCFitter$methods( optimLRC = NonrectangularLRCFitter_optimLRC)

# computeCost inherited

NonrectangularLRCFitter_predictLRC <- function(
		### Nonrectangular Rectungular Hyperbolic Light Response function: (Gilmanov et al., 2003)
		theta   ##<< theta [numeric] -> parameter vector (theta[1]=kVPD (k), theta[2]=beta0 (beta), theta[3]=alfa, theta[4]=Rref (rb), theta[5]=E0, theta[6]=logitconv)
		##<< E0: Temperature sensitivity ("activation energy") in Kelvin (degK) #get("testparams", envir=environment(foo)
		,Rg   	##<< ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation
		,VPD 	##<< VPD [numeric] -> Vapor Pressure Deficit [hPa]
		,Temp 	##<< Temp [degC] -> Temperature [degC] 
		#,E0 	##<< Temperature sensitivity ("activation energy") in Kelvin (degK) #get("testparams", envir=environment(foo)
		,VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010
		,fixVPD = FALSE   	##<< fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered
) {
	##details<<
	## with fixes E0.n for the estimation of Rd
	## VPD effect included according to Lasslop et al., 2010
	##details<<
	## If theta is a matrix, a different row of parameters is used for different entries of other inputs
	if( is.matrix(theta) ){
		kVPD<-theta[,1]
		beta0<-theta[,2]
		alfa<-theta[,3]
		Rref<-theta[,4]
		E0<-theta[,5]
		logitconv<-theta[,6]
	} else {
		kVPD<-theta[1]
		beta0<-theta[2]
		alfa<-theta[3]
		Rref<-theta[4]
		E0<-theta[5]
		logitconv<-theta[6]
	}
	conv <- invlogit(logitconv)
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
NonrectangularLRCFitter$methods( predictLRC = NonrectangularLRCFitter_predictLRC)

NonrectangularLRCFitter_computeLRCGradient <- function(
		### Gradient of \code{\link{partGL_RHLightResponse}}
		theta   ##<< theta [numeric] -> parameter vector (theta[1]=kVPD (k), theta[2]=beta0 (beta), theta[3]=alfa, theta[4]=Rref (rb), theta[4]=E0, theta[5]=logitconv)
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
		logitconv<-theta[,6]
	} else {
		kVPD<-theta[1]
		beta0<-theta[2]
		alfa<-theta[3]
		Rref<-theta[4]
		E0<-theta[5]
		logitconv<-theta[6]
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
NonrectangularLRCFitter$methods( computeLRCGradient = NonrectangularLRCFitter_computeLRCGradient)








