

NonrectangularLRCFitter <- setRefClass('NonrectangularLRCFitter', contains='LightResponseCurveFitter'
## R5 reference class for the nonrectangular Light response curve
##author<<
## TW, MM
)	

NonrectangularLRCFitter_getParameterNames <- function(
### return the parameter names used by this Light Response Curve Function
){
	##value<< string vector of parameter names. Positions are important.
	## Adds sixth parameter: logit-transformed convexity parameter: -> 0 rectangular, -> 1 step  
	ans <- callSuper()
	c(ans
	, logitconf="logitconv")	##<< logit-transformed convexity parameter. The value at original scale is obtained by conv = 1/(1+exp(-logitconv))
}
NonrectangularLRCFitter$methods(getParameterNames = NonrectangularLRCFitter_getParameterNames)


NonrectangularLRCFitter$methods(
getPriorLocation = function(
		### return the prior distribution of parameters
		NEEDay 		##<< numeric vector of daytime NEE
		,RRefNight	##<< numeric scalar of basal respiration estimated from night-time data
		,E0			##<< numeric scalar of night-time estimate of temperature sensitivity
){
	ans <- callSuper(NEEDay=NEEDay, RRefNight=RRefNight, E0=E0)
	##value<< a numeric vector with prior estimates of the parameters
	c(ans
	, logitconv=logit(0.75))	##<< logit-transformed convexity parameter. The valua at original scale is obtained by conv = 1/(1+exp(-logitconv))
})

NonrectangularLRCFitter$methods(
getPriorScale = function(
		### return the prior distribution of parameters
		thetaPrior 	##<< numeric vector of location of priors
		,medianRelFluxUncertainty	##<< numeric scalar: median across the relative uncertainty of the flux values, i.e. sdNEE/NEE
		,nRec		##<< integer scalar: number of finite observations
		, ctrl		##<< list of further controls, with entry \code{isLasslopPriorsApplied}	
){
	ans <- callSuper(thetaPrio=thetaPrior, medianRelFluxUncertainty=medianRelFluxUncertainty, nRec=nRec, ctrl=ctrl)
	##value<< adds NA prior for logitconv
	c(ans
		, logitconv=NA)	
})

# getParameterInitials inherited

NonrectangularLRCFitter_getOptimizedParameterPositions <- function( isUsingFixedVPD, isUsingFixedAlpha){
	iOpt <- callSuper(isUsingFixedVPD=isUsingFixedVPD, isUsingFixedAlpha=isUsingFixedAlpha)
	iOpt <- c(iOpt,6)	# add the convexity parameter
	iOpt
}
NonrectangularLRCFitter$methods(getOptimizedParameterPositions = NonrectangularLRCFitter_getOptimizedParameterPositions)

NonrectangularLRCFitter_predictLRC <- function(
		### Nonrectangular Hyperbolic Light Response function: (Gilmanov et al., 2003)
		theta   ##<< theta [numeric] see \code{\link{NonrectangularLRCFitter_getParameterNames}}
		,Rg   	##<< ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation
		,VPD 	##<< VPD [numeric] -> Vapor Pressure Deficit [hPa]
		,Temp 	##<< Temp [degC] -> Temperature [degC] 
		,VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010
		,fixVPD = FALSE   	##<< fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered
) {
	# extracting and calling precit with conf-parameter
	if( is.matrix(theta) ){
		kVPD<-theta[,1]
		beta<-theta[,2]
		alpha<-theta[,3]
		Rref<-theta[,4]
		E0<-theta[,5]
		logitconv<-theta[,6]
	} else {
		kVPD<-theta[1]
		beta<-theta[2]
		alpha<-theta[3]
		Rref<-theta[4]
		E0<-theta[5]
		logitconv<-theta[6]
	}
	conv <- invlogit(logitconv)
	Amax <- if( isTRUE(fixVPD) ) beta else {
				ifelse(VPD > VPD0, beta*exp(-kVPD*(VPD-VPD0)), beta)
			} 
	Reco<-Rref*exp(E0*(1/((273.15+15)-227.13)-1/(Temp+273.15-227.13)))
	GPP <- .self$predictGPP(Rg, Amax=Amax, alpha=alpha, conv=conv)
	NEP <- GPP - Reco
	## a data.frame of length of Rg of computed  
	ans <- list(
			NEP=NEP		##<< Net ecosystem production (-NEE), vector of length(Rg)
			,Reco=Reco	##<< Ecosystem respiration 
			,GPP=GPP	##<< Gross primary production
	)
}
NonrectangularLRCFitter$methods( predictLRC = NonrectangularLRCFitter_predictLRC)

NonrectangularLRCFitter_predictGPP  <- function(
		### Logistic Sigmoid Light Response function for GPP
		Rg   	##<< ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation
		,Amax	##<< numeric scalar or vector of length(Rg): beta parameter adjusted for VPD effect
		,alpha	##<< numeric scalar or vector of length(Rg): alpha parameter: initial slope
		,conv	##<< numeric scalar or vector of length(Rg): convexity parameter (see details)
){
	##details<<
	## This function generalizes the \code{\link{RectangularLRCFitter_predictLRC}} by adding a convexity parameter.
	## For conv -> 0 (logitconv -> -Inf): approaches the rectangluar hyperbolic.
	## for conv -> 1 (logitconv -> +Inf): approaches a step function.
	## Expected to have values abourn 0.7-0.9 (Moffat 2012)
	zRoot<-((alpha*Rg+Amax)^2)-(4*alpha*Rg*conv*Amax)
	zRoot[which(zRoot<0)]<-0
	GPP<-(1/(2*conv))*(alpha*Rg+Amax-sqrt(zRoot))
}
NonrectangularLRCFitter$methods( predictGPP = NonrectangularLRCFitter_predictGPP)


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
		beta<-theta[,2]
		alpha<-theta[,3]
		Rref<-theta[,4]
		E0<-theta[,5]
		logitconv<-theta[,6]
	} else {
		kVPD<-theta[1]
		beta<-theta[2]
		alpha<-theta[3]
		Rref<-theta[4]
		E0<-theta[5]
		logitconv<-theta[6]
	}
	Amax <- if( isTRUE(fixVPD) ) beta else {
				ifelse(VPD > VPD0, beta*exp(-kVPD*(VPD-VPD0)), beta)
			}
	#ex <- expression( beta0*exp(-kVPD*(VPD-VPD0)) ); deriv(ex,c("beta0","kVPD"))
	dAmax_dkVPD <- if( isTRUE(fixVPD) ) 0 else {
				ifelse(VPD > VPD0, beta*-(VPD-VPD0)*exp(-kVPD*(VPD-VPD0)), 0)
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
	.expr2 <- Amax * alpha * Rg
	.expr3 <- alpha * Rg
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








