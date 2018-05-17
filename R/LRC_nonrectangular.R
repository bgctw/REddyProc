
#' R5 reference class for the nonrectangular Light response curve
#'
#' @import methods
#' @export NonrectangularLRCFitter
#' @exportClass NonrectangularLRCFitter
NonrectangularLRCFitter <- setRefClass('NonrectangularLRCFitter'
                                       , contains = 'LightResponseCurveFitter'
## R5 reference class for the nonrectangular Light response curve
##author<<
## TW, MM
)

#' @export
NonrectangularLRCFitter_getParameterNames <- function(
### return the parameter names used by this Light Response Curve Function
) {
	##value<< string vector of parameter names. Positions are important.
	## Adds sixth parameter, \code{logitconv} to the parameters
	## of \code{\link{LightResponseCurveFitter_getParameterNames}}
	ans <- callSuper()
	c(ans
	, logitconf = "logitconv")	##<< logit-transformed convexity parameter.
	## The value at original scale is obtained by
	## \code{conv = 1 / (1 + exp(-logitconv))}
	##seealso<< \code{\link{NonrectangularLRCFitter_predictGPP}}
}
NonrectangularLRCFitter$methods(getParameterNames =
                                  NonrectangularLRCFitter_getParameterNames)


NonrectangularLRCFitter$methods(
getPriorLocation = function(
		### return the prior distribution of parameters
		NEEDay 		##<< numeric vector of daytime NEE
		, RRefNight	##<< numeric scalar of basal respiration estimated
		  ## from night-time data
		, E0			##<< numeric scalar of night-time estimate of
		  ##temperature sensitivity
) {
	ans <- callSuper(NEEDay = NEEDay, RRefNight = RRefNight, E0 = E0)
	##value<< a numeric vector with prior estimates of the parameters
	c(ans
	, logitconv = logit(0.75))	##<< logit-transformed convexity parameter.
	  ## The valua at original scale is obtained by
	  ## \code{conv = 1 / (1 + exp(-logitconv))}
})

NonrectangularLRCFitter$methods(
getPriorScale = function(
		### return the prior distribution of parameters
		thetaPrior 	##<< numeric vector of location of priors
		, medianRelFluxUncertainty	##<< numeric scalar: median across the
		  ##relative uncertainty of the flux values, i.e. sdNEE / NEE
		, nRec		##<< integer scalar: number of finite observations
		, ctrl		##<< list of further controls, with entry
		  ##\code{isLasslopPriorsApplied}
) {
	ans <- callSuper(thetaPrio = thetaPrior, medianRelFluxUncertainty
	                 = medianRelFluxUncertainty, nRec = nRec, ctrl = ctrl)
	##value<< adds NA prior for logitconv
	c(ans
		, logitconv = NA)
})

# getParameterInitials inherited

NonrectangularLRCFitter_getOptimizedParameterPositions <- function(
  ### get the positions of the parameters to optimize for given Fixed
  isUsingFixedVPD			##<< boolean scalar: if TRUE,
    ## VPD effect set to zero and is not optimized
  , isUsingFixedAlpha		##<< boolean scalar: if TRUE,
    ## initial slope is fixed and is not optimized
) {
	iOpt <- callSuper(isUsingFixedVPD = isUsingFixedVPD
	                  , isUsingFixedAlpha = isUsingFixedAlpha)
	iOpt <- c(iOpt, 6)	# add the convexity parameter
	iOpt
}
NonrectangularLRCFitter$methods(getOptimizedParameterPositions
                  = NonrectangularLRCFitter_getOptimizedParameterPositions)

NonrectangularLRCFitter_predictLRC <- function(
	### Nonrectangular Hyperbolic Light Response function: (Gilmanov et al., 2003)
  theta   ##<< numeric vector of parameters
  , Rg   	##<< ppfd [numeric] -> photosynthetic flux density
    ## [umol / m2 / s] or Global Radiation
  , VPD 	##<< VPD [numeric] -> Vapor Pressure Deficit [hPa]
  , Temp 	##<< Temp [degC] -> Temperature [degC]
  , VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa
    ## according to Lasslop et al 2010
  , fixVPD = (k == 0)   	##<< boolean scalar or vector of nrow theta:
    ## fixVPD if TRUE the VPD effect is not considered and VPD is not part
    ## of the computation
  , TRef = 15			##<< numeric scalar of Temperature (degree Celsius) for
) {
	# extracting and calling precit with conf-parameter
	if (is.matrix(theta) ) {
		k <- theta[, 1]
		beta <- theta[, 2]
		alpha <- theta[, 3]
		RRef <- theta[, 4]
		E0 <- theta[, 5]
		logitconv <- theta[, 6]
	} else {
		k <- theta[1]
		beta <- theta[2]
		alpha <- theta[3]
		RRef <- theta[4]
		E0 <- theta[5]
		logitconv <- theta[6]
	}
	conv <- invlogit(logitconv)
	if (length(fixVPD) != length(VPD) )
		if (length(fixVPD) == 1L) fixVPD <- rep(fixVPD, length(VPD) ) else
			stop("Length of vector argument fixVPD must correspond to rows in theta.")
	Amax <- ifelse(fixVPD, beta,
			#ifelse(is.finite(VPD) & (VPD > VPD0), beta * exp(-k * (VPD-VPD0)), beta)
			ifelse((VPD > VPD0), beta * exp(-k * (VPD - VPD0)), beta)
	)
	Reco <- RRef * exp(E0 * (1 / ((273.15 + TRef) - 227.13)
	                         - 1 / (Temp + 273.15 - 227.13)))
	GPP <- .self$predictGPP(Rg, Amax = Amax, alpha = alpha, conv = conv)
	NEP <- GPP - Reco
	## a data.frame of length of Rg of computed
	ans <- list(
			NEP = NEP		##<< Net ecosystem production (-NEE), vector of length(Rg)
			, Reco = Reco	##<< Ecosystem respiration
			, GPP = GPP	##<< Gross primary production
	)
}
NonrectangularLRCFitter$methods(predictLRC = NonrectangularLRCFitter_predictLRC)

#' @export
NonrectangularLRCFitter_predictGPP  <- function(
		### Nonrectangular hyperbolic Light Response function for GPP
		Rg   	##<< ppfd [numeric] -> photosynthetic flux density
		## [mumol / m2 / s] or Global Radiation
		, Amax	##<< numeric scalar or vector of length(Rg):
		## beta parameter adjusted for VPD effect
		, alpha	##<< numeric scalar or vector of length(Rg):
		## alpha parameter: initial slope
		, conv	##<< numeric scalar or vector of length(Rg):
		## convexity parameter (see details)
) {
	##seealso<< \code{\link{LightResponseCurveFitter_predictGPP}}
	##details<<
	## This function generalizes the \code{\link{RectangularLRCFitter_predictGPP}}
	## by adding the convexity parameter \code{conv}.
	## For \code{conv -> 0 (logitconv -> -Inf)}: approaches the rectangular hyperbolic.
	## For \code{conv -> 1 (logitconv -> + Inf)}: approaches a step function.
	## Expected values of \code{conv} are about 0.7-0.9 (Moffat 2012).
	zRoot <- ((alpha * Rg + Amax)^2) - (4 * alpha * Rg * conv * Amax)
	zRoot[which(zRoot < 0)] <- 0
	##value<< numeric vector of length(Rg) of GPP
	GPP <- (1 / (2 * conv)) * (alpha * Rg + Amax - sqrt(zRoot))
}
NonrectangularLRCFitter$methods(predictGPP = NonrectangularLRCFitter_predictGPP)


NonrectangularLRCFitter_computeLRCGradient <- function(
		### Gradient of \code{\link{NonrectangularLRCFitter_predictLRC}}
		theta   ##<< theta [numeric] -> parameter vector (theta[1] = kVPD (k),
		## theta[2] = beta0 (beta), theta[3] = alpha, theta[4] = RRef (rb),
		## theta[4] = E0, theta[5] = logitconv)
		, Rg   	##<< ppfd [numeric] -> photosynthetic flux density
		## [umol / m2 / s] or Global Radiation
		, VPD 	##<< VPD [numeric] -> Vapor Pressure Deficit [hPa]
		, Temp 	##<< Temp [degC] -> Temperature [degC]
		, VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa
		## according to Lasslop et al 2010
		, fixVPD = (k == 0)   	##<< boolean scalar or vector of nrow(theta):
		## fixVPD if TRUE the VPD effect is not considered and VPD is not part
		## of the computation
		, TRef = 15			##<< numeric scalar of Temperature (degree Celsius)
		## for reference respiration RRef
) {
	##details<< differs from base by extracting \code{conv} parameter from \code{theta}
	## and adding gradient to logitconv (3rd parameter from computeGPPGradient)
	if (is.matrix(theta) ) {
		k <- theta[, 1]
		beta <- theta[, 2]
		alpha <- theta[, 3]
		RRef <- theta[, 4]
		E0 <- theta[, 5]
		logitconv <- theta[, 6]
	} else {
		k <- theta[1]
		beta <- theta[2]
		alpha <- theta[3]
		RRef <- theta[4]
		E0 <- theta[5]
		logitconv <- theta[6]
	}
	if (!is.finite(logitconv[1]) ) stop("need to provide finite logitconv in theta")
	if (length(fixVPD) != length(VPD) )
		if (length(fixVPD) == 1L) fixVPD <- rep(fixVPD, length(VPD) ) else
			stop("Length of vector argument fixVPD must correspond to rows in theta.")
	Amax <- ifelse(fixVPD, beta,
			#ifelse(is.finite(VPD) & (VPD > VPD0), beta * exp(-k * (VPD-VPD0)), beta)
			ifelse((VPD > VPD0), beta * exp(-k * (VPD - VPD0)), beta)
	)
	#ex <- expression(beta * exp(-k * (VPD-VPD0)) ); deriv(ex, c("beta", "k"))
	dAmax_dkVPD <- ifelse(fixVPD, 0,
			ifelse(VPD > VPD0, beta * -(VPD - VPD0) * exp(-k * (VPD - VPD0)), 0)
	)
	dAmax_dbeta0 <- ifelse(fixVPD, 0,
			ifelse(VPD > VPD0, exp(-k * (VPD - VPD0)), 1)
	)
	#Reco<- RRef * exp(E0 * (1 / ((273.15 + 10)-227.13)-1 / (Temp + 273.15-227.13)))
	#ex <- expression(RRef * exp(E0 * (1 / ((273.15 + TRef)-227.13)-1 / (Temp + 273.15-227.13))) ); deriv(ex, c("RRef", "E0"))
	# to prevent numeric instabilities, use do not let temperature go below 20degC
	#.expr7 <- 1 / (273.15 + TRef - 227.13) - 1 / (Temp + 273.15 - 227.13)
	.expr7 <- 1 / (273.15 + TRef - 227.13) - 1 / (pmax(-20, Temp) + 273.15 - 227.13)
	.expr9 <- exp(E0 * .expr7)
	gradReco <- matrix(0, ncol = 2L, nrow = length(.expr9), dimnames =
	                     list(NULL, c("RRef", "E0")))
	gradReco[, "RRef"] <- dReco_dRRef <- .expr9
	gradReco[, "E0"] <- dReco_dE0 <- RRef * (.expr9 * .expr7)
	#
	gradGPP <- array(0, c(nrow(gradReco), 4L), list(NULL,
	                                   c("k", "beta", "alpha", "logitconv")))
	dGPP_dAMax <- .self$computeGPPGradient(Rg, Amax, alpha, logitconv)
	gradGPP[, "beta"] <- dGPP_dAMax[, 1L] * dAmax_dbeta0
	gradGPP[, "k"] <- dGPP_dAMax[, 1L] * dAmax_dkVPD
	gradGPP[, "alpha"] <- dGPP_dAMax[, 2L]
	gradGPP[, "logitconv"] <- dGPP_dAMax[, 3L]
	#NEP <- GPP - Reco
	gradNEP <- cbind(gradGPP, -gradReco)
	##value<<
	## list with gradient matrices. For each record
	## (length(Rg)), c("k", "beta", "alpha", "RRef")
	ans <- list(
			NEP = gradNEP
			, Reco = gradReco
			, GPP = gradGPP
	)
}
NonrectangularLRCFitter$methods(computeLRCGradient =
                                  NonrectangularLRCFitter_computeLRCGradient)

.tmp.f <- function() {
	iNonFinite <- which(!is.finite(gradReco[, "E0"]))
}

NonrectangularLRCFitter_computeGPPGradient  <- function(
		### Logistic Sigmoid Light Response function for GPP
		Rg   	##<< ppfd [numeric] -> photosynthetic flux density
		## [umol / m2 / s] or Global Radiation
		, Amax	##<< numeric scalar or vector of length(Rg):
		## beta parameter adjusted for VPD effect
		, alpha	##<< numeric scalar or vector of length(Rg):
		## alpha parameter: initial slope
		, logitconv	##<< numeric scalar or vector of length(Rg):
		## logit of convexity paramter
) {
	zRoot <- ((alpha * Rg + Amax)^2) - (4 * alpha * Rg * invlogit(logitconv) * Amax)
	iNegRoot <- which(zRoot < 0)
	#GPP<- (1 / (2 * (1 / (1 + exp(-logitconv))) )) * (alpha * Rg + Amax-sqrt(zRoot))
	#GPP<-               (1 / (2 * (1 / (1 + exp(-logitconv))) )) * (alpha * Rg + Amax-sqrt(((alpha * Rg + Amax)^2)-(4 * alpha * Rg * (1 / (1 + exp(-logitconv))) * Amax)))
	#ex <- expression(  (1 / (2 * (1 / (1 + exp(-logitconv))) )) * (alpha * Rg + Amax-sqrt(((alpha * Rg + Amax)^2)-(4 * alpha * Rg * (1 / (1 + exp(-logitconv))) * Amax))) ); deriv(ex, c("Amax", "alpha", "logitconv"))
	.expr2 <- exp(-logitconv)
	.expr3 <- 1 + .expr2
	.expr4 <- 1 / .expr3
	.expr5 <- 2 * .expr4
	.expr6 <- 1 / .expr5
	.expr8 <- alpha * Rg + Amax
	.expr11 <- 4 * alpha * Rg
	.expr12 <- .expr11 * .expr4
	.expr14 <- .expr8^2 - .expr12 * Amax
	.expr16 <- .expr8 - sqrt(.expr14)
	.expr20 <- .expr14^-0.5
	.expr36 <- .expr2 / .expr3^2
	.value <- .expr6 * .expr16
	#plot(.value ~ GPP)
	.grad <- array(0, c(length(.value), 3L), list(NULL, c("Amax", "alpha", "logitconv")))
	.grad[, "Amax"] <- .expr6 * (1 - 0.5 * ((2 * .expr8 - .expr12) * .expr20))
	.grad[, "alpha"] <- .expr6 * (Rg - 0.5 * ((2 * (Rg * .expr8) -
						4 * Rg * .expr4 * Amax) * .expr20))
	.grad[, "logitconv"] <- .expr6 * (0.5 * (.expr11 * .expr36 *
					Amax * .expr20)) - 2 * .expr36 / .expr5^2 * .expr16
	if (length(iNegRoot) ) {
		#GPP<- (1 / (2 * (1 / (1 + exp(-logitconv))) )) * (alpha * Rg + Amax-0)
		#ex <- expression(  (1 / (2 * (1 / (1 + exp(-logitconv))) )) * (alpha * Rg + Amax-0) ); deriv(ex, c("Amax", "alpha", "logitconv"))
		#.expr2 <- exp(-logitconv)
		#.expr3 <- 1 + .expr2
		.expr5 <- 2 * (1 / .expr3)
		.expr6 <- 1 / .expr5
		.expr9 <- alpha * Rg + Amax - 0
		.value <- .expr6 * .expr9
		.gradNegRoot <- array(0, c(length(.value), 3L), list(NULL, c("Amax",
								"alpha", "logitconv")))
		.gradNegRoot[, "Amax"] <- .expr6
		.gradNegRoot[, "alpha"] <- .expr6 * Rg
		.gradNegRoot[, "logitconv"] <- -(2 * (.expr2 / .expr3^2) / .expr5^2 *
					.expr9)
		.grad[iNegRoot, ] <- .gradNegRoot[iNegRoot, ]
	}
	.grad

}
NonrectangularLRCFitter$methods(computeGPPGradient =
                                  NonrectangularLRCFitter_computeGPPGradient)








