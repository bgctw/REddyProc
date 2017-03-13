

LogisticSigmoidLRCFitter <- setRefClass('LogisticSigmoidLRCFitter', contains='LightResponseCurveFitter'
## R5 reference class for the Logistic sigmoid Light response curve
##author<<
## TW
)	

LogisticSigmoidLRCFitter_predictGPP  <- function(
		### Logistic Sigmoid Light Response function for GPP
		Rg   	##<< ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation
		,Amax	##<< numeric scalar or vector of length(Rg): beta parameter adjusted for VPD effect
		,alpha	##<< numeric scalar or vector of length(Rg): alpha parameter: initial slope
) {
	##value<< numeric vector of length(Rg) of GPP 
	GPP <- Amax * tanh(alpha*Rg/Amax) 
}
LogisticSigmoidLRCFitter$methods( predictGPP = LogisticSigmoidLRCFitter_predictGPP)

LogisticSigmoidLRCFitter_computeLRCGradient <- function(
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
		alpha<-theta[,3]
		Rref<-theta[,4]
		E0<-theta[,5]
	} else {
		kVPD<-theta[1]
		beta0<-theta[2]
		alpha<-theta[3]
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
LogisticSigmoidLRCFitter$methods( computeLRCGradient = LogisticSigmoidLRCFitter_computeLRCGradient)








