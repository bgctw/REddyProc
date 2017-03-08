

RectangularLRCFitter <- setRefClass('RectangularLRCFitter', contains='NonrectangularLRCFitter'
## R5 reference class for the Rectangular Light response curve
##author<<
## TW, MM
)	

RectangularLRCFitter$methods(
getParameterNames = function(
### return the parameter names used by this Light Response Curve Funciton
){
	##value<< Same as \code{\link{NonrectangularLRCFitter_getParameterNames}},
	## unless omitting the convexity parameter.
	ans <- callSuper()
	ans[1:5]	# omit the conv parameter
})


RectangularLRCFitter$methods(
getPriorLocation = function(
		### return the prior distribution of parameters
		NEEDay 		##<< numeric vector of daytime NEE
		,RRefNight	##<< numeric scalar of basal respiration estimated from night-time data
		,E0			##<< numeric scalar of night-time estimate of temperature sensitivity
){
	##value<< a numeric vector with prior estimates of the parameters
	ans <- callSuper(NEEDay, RRefNight, E0)
	ans <- ans[1:5]	# omit conv parameter
})

RectangularLRCFitter$methods(
getPriorScale = function(
		### return the prior distribution of parameters
		thetaPrior 	##<< numeric vector of location of priors
		,medianRelFluxUncertainty	##<< numeric scalar: median across the relative uncertainty of the flux values, i.e. sdNEE/NEE
		,nRec		##<< integer scalar: number of finite observations
		, ctrl		##<< list of further controls, with entry \code{isLasslopPriorsApplied}	
){
	##value<< a numeric vector with prior estimates of the parameters
	ans <- callSuper(thetaPrior, medianRelFluxUncertainty, nRec, ctrl)
	ans <- ans[1:5]	# omit conv parameter
})

# getParameterInitials inherited

RectangularLRCFitter_optimLRCBounds <- function(
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
	# differs from Nonrectangular by setting iOpt
	if( !is.finite(lastGoodParameters.V.n[3L]) ) lastGoodParameters.V.n[3L] <- 0.22	# twutz 161014: default alpha 	
	isUsingFixedAlpha <- FALSE
	isUsingVixedVPD <- FALSE
	getIOpt <- function( isUsingFixedVPD, isUsingFixedAlpha){
		iOpt <- 
				if( !isUsingFixedVPD & !isUsingFixedAlpha ) c(1:4) else
				if(  isUsingFixedVPD & !isUsingFixedAlpha ) 2:4 else
				if( !isUsingFixedVPD &  isUsingFixedAlpha ) c(1L,2L,4L) else
				if(  isUsingFixedVPD &  isUsingFixedAlpha ) c(2L,4L) 
		#iOpt <- c(iOpt,6)	# add the convexity parameter
	}
	resOpt <- resOpt0 <- LRC$optimLRCOnAdjustedPrior(theta0, iOpt=getIOpt(isUsingVixedVPD, isUsingFixedAlpha), parameterPrior = parameterPrior, ctrl=ctrl, ... )
	# positions in theta0: "k"     "beta0" "alfa"  "Rb"    "E0"
	# IF kVPD parameter less or equal zero then estimate the parameters withouth VPD effect
	##details<<
	## If parameters alpha or k are outside bounds (Table A1 in Lasslop 2010), refit with some parameters fixed 
	## to values from fit of previous window.
	theta0Adj <- theta0	# intial parameter estimate with some parameters adjusted to bounds
	#dsDay <- list(ctrl, ...)$dsDay
	if (resOpt$theta[1L] < 0){
		isUsingVixedVPD <- TRUE
		theta0Adj[1L] <- 0
		resOpt <- LRC$optimLRCOnAdjustedPrior(theta0Adj, iOpt=getIOpt(isUsingVixedVPD, isUsingFixedAlpha), parameterPrior = parameterPrior, ctrl=ctrl, ... )
		# check alpha, if less than zero estimate parameters with fixed alpha of last window 
		if ( (resOpt$theta[3L] > 0.22) && is.finite(lastGoodParameters.V.n[3L]) ){
			isUsingFixedAlpha <- TRUE
			theta0Adj[3L] <- lastGoodParameters.V.n[3L] 
			resOpt <- LRC$optimLRCOnAdjustedPrior(theta0Adj, iOpt=getIOpt(isUsingVixedVPD, isUsingFixedAlpha), parameterPrior = parameterPrior, ctrl, ... )
		}
	} else {
		# check alpha, if gt 0.22 estimate parameters with fixed alpha of last window
		# if not last window exists, let alpha > 0.22
		if ( (resOpt$theta[3L] > 0.22) && is.finite(lastGoodParameters.V.n[3L]) ){
			isUsingFixedAlpha <- TRUE
			theta0Adj[3L] <- lastGoodParameters.V.n[3L]
			resOpt <- LRC$optimLRCOnAdjustedPrior(theta0Adj, iOpt=getIOpt(isUsingVixedVPD, isUsingFixedAlpha), parameterPrior = parameterPrior, ctrl, ... )
			# check k, if less than zero estimate parameters without VPD effect and with fixed alpha of last window 
			if (resOpt$theta[1L] < 0){
				isUsingVixedVPD <- TRUE
				theta0Adj[1L] <- 0
				resOpt <- LRC$optimLRCOnAdjustedPrior(theta0Adj, iOpt=getIOpt(isUsingVixedVPD, isUsingFixedAlpha), parameterPrior = parameterPrior, ctrl, ... )
			}
		}
	} 
	##details<<
	## No parameters are reported if alpha<0 or Rb < 0 or beta0 < 0 or beta0 > 250 
	# positions in theta0: "k"     "beta0" "alfa"  "Rb"    "E0"
	if( !is.na(resOpt$theta[1L]) && ((resOpt$theta[3L] < 0) || (resOpt$theta[4L] < 0) || (resOpt$theta[2L] < 0) || (resOpt$theta[2L] >= 250)) ){
		# TODO estimate Rb from daytime data?
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
RectangularLRCFitter$methods( optimLRCBounds = RectangularLRCFitter_optimLRCBounds ) 

# isParameterInBounds inherited from Nonrectangluar

# optimLRD inherited from Nonrectangular

# computeCost inherited

RectangularLRCFitter_predictLRC <- function(
		###Rectungular Hyperbolic Light Response function: (Xu & Baldocchi, 2004; Falge et al., 2001; Lasslop et al., 2010)
		theta 	##<< theta [numeric] -> parameter vector (theta[1]=kVPD (k), theta[2]=beta0 (beta), theta[3]=alfa, theta[4]=Rref (rb), theta[5]=E0)
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
	} else {
		kVPD<-theta[1]
		beta0<-theta[2]
		alfa<-theta[3]
		Rref<-theta[4]
		E0<-theta[5]
	}
	Amax <- if( isTRUE(fixVPD) ) beta0 else {
				ifelse(VPD > VPD0, beta0*exp(-kVPD*(VPD-VPD0)), beta0)
			} 
	#Reco<-Rref*exp(E0*(1/((273.15+10)-227.13)-1/(Temp+273.15-227.13)))	# formerly at 10degC
	Reco<-Rref*exp(E0*(1/((273.15+15)-227.13)-1/(Temp+273.15-227.13)))
	GPP <- (Amax*alfa*Rg)/(alfa*Rg+Amax)
	NEP <- GPP - Reco
	## a data.frame of length of Rg of computed  
	ans <- list(
			NEP=NEP		##<< Net ecosystem production (-NEE), vector of length(Rg)
			,Reco=Reco	##<< Ecosystem respiration 
			,GPP=GPP	##<< Gross primary production
	)
}

RectangularLRCFitter$methods( predictLRC = RectangularLRCFitter_predictLRC )

RectangularLRCFitter_computeLRCGradient <- function(
		### Gradient of \code{\link{partGL_RHLightResponse}}
		theta 	##<< theta [numeric] -> parameter vector (theta[1]=kVPD (k), theta[2]=beta0 (beta), theta[3]=alfa, theta[4]=Rref (rb), theta[4]=E0)
		##<< E0: Temperature sensitivity ("activation energy") in Kelvin (degK)
		,Rg   	##<< ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation
		,VPD 	##<< VPD [numeric] -> Vapor Pressure Deficit [hPa]
		,Temp 	##<< Temp [degC] -> Temperature [degC] 
		#,E0 	##<< Temperature sensitivity ("activation energy") in Kelvin (degK) #get("testparams", envir=environment(foo)
		,VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010
		,fixVPD = FALSE   	##<< fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered
) {
	if( is.matrix(theta) ){
		kVPD<-theta[,1]
		beta0<-theta[,2]
		alfa<-theta[,3]
		Rref<-theta[,4]
		E0<-theta[,5]
	} else {
		kVPD<-theta[1]
		beta0<-theta[2]
		alfa<-theta[3]
		Rref<-theta[4]
		E0<-theta[5]
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

RectangularLRCFitter$methods(computeLRCGradient = RectangularLRCFitter_computeLRCGradient)


RectangularLRCFitterCVersion <- setRefClass('RectangularLRCFitterCVersion', contains='RectangularLRCFitter'
	### overiding computeCost of \code{\link{RectangularLRCFitter}} to a C-version (\code{\link{RectangularLRCFitter_C_computeCost}}).
)

RectangularLRCFitterCVersion_computeCost <- function(
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
	RHLightResponseCostC(theta, flux, sdFlux, parameterPrior, sdParameterPrior, ..., VPD0=VPD0, fixVPD=fixVPD)
}
RectangularLRCFitterCVersion$methods( computeCost = RectangularLRCFitterCVersion_computeCost )








