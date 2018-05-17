#' R5 reference class for the Rectangular Light response curve
#'
#' @import methods
#' @export RectangularLRCFitter
#' @exportClass RectangularLRCFitter
RectangularLRCFitter <- setRefClass('RectangularLRCFitter'
                                    , contains = 'LightResponseCurveFitter'
## R5 reference class for the Rectangular Light response curve
##author<<
## TW, MM
)

#' @export
RectangularLRCFitter_predictGPP  <- function(
		### Rectangular hyperbolic Light Response function for GPP
		Rg   	##<< ppfd [numeric] -> photosynthetic flux density
		  ## [mumol / m2 / s] or Global Radiation
		, Amax	##<< vector of length(Rg): saturation (beta parameter) adjusted
		  ##for effect of VPD for each line of Rg
		, alpha	##<< numeric scalar or vector of length(Rg): alpha parameter:
		  ## slope at Rg = 0
) {
	#details<< \code{GPP <- (Amax * alpha * Rg) / (alpha * Rg + Amax)}
	##seealso<< \code{\link{LightResponseCurveFitter_predictGPP}}
	##value<< numeric vector of length(Rg) of GPP
	GPP <- (Amax * alpha * Rg) / (alpha * Rg + Amax)
}
RectangularLRCFitter$methods(predictGPP = RectangularLRCFitter_predictGPP)


RectangularLRCFitter_computeGPPGradient  <- function(
	### Logistic Sigmoid Light Response function for GPP
  Rg   	##<< ppfd [numeric] -> photosynthetic flux density
  ## [mumol / m2 / s] or Global Radiation
  , Amax	##<< vector of length(Rg): saturation (beta parameter) adjusted
  ##for effect of VPD for each line of Rg
  , alpha	##<< numeric scalar or vector of length(Rg): alpha parameter:
  ## slope at Rg = 0
) {
	##value<< numeric matrix (length(Rg), 2) of gradients of predicted GPP
	## to Amax and alpha
	#ex <- expression( (Amax * alpha * Rg) / (alpha * Rg + Amax) );
	# deriv(ex, c("Amax", "alpha"))
	.expr2 <- Amax * alpha * Rg
	.expr3 <- alpha * Rg
	.expr4 <- .expr3 + Amax
	.expr7 <- .expr4^2
	.value <- .expr2 / .expr4
	.grad <- array(0, c(length(.value), 2L), list(NULL, c("Amax", "alpha")))
	.grad[, 1L] <- .expr3 / .expr4 - .expr2 / .expr7
	.grad[, 2L] <- Amax * Rg / .expr4 - .expr2 * Rg / .expr7
	.grad
}
RectangularLRCFitter$methods(computeGPPGradient =
                               RectangularLRCFitter_computeGPPGradient)

#-------------------- CVersion
#' R5 reference class C-version for the Rectangular Light response curve
#'
#' @import methods
#' @export RectangularLRCFitterCVersion
#' @exportClass RectangularLRCFitterCVersion
RectangularLRCFitterCVersion <- setRefClass('RectangularLRCFitterCVersion',
                                            contains = 'RectangularLRCFitter'
	### overiding computeCost of \code{\link{RectangularLRCFitter}}
	### to a C-version (\code{\link{RectangularLRCFitter_C_computeCost}}).
)

RectangularLRCFitterCVersion_computeCost <- function(
  ### Computing residual sum of squares for predictions vs. data of NEE
  thetaOpt   ##<< parameter vector with components of theta0 that are optimized
  , theta		##<< parameter vector with positions as in argument of
    ## \code{\link{LightResponseCurveFitter_getParameterNames}}
  , iOpt		##<< position in theta that are optimized
  , flux 		##<< numeric: NEP (-NEE) or GPP time series [umolCO2 / m2 / s],
    ## should not contain NA
  , sdFlux 	##<< numeric: standard deviation of Flux [umolCO2 / m2 / s],
    ##should not contain NA
  , parameterPrior		##<< numeric vector along theta: prior estimate of
    ## parameter (range of values)
  , sdParameterPrior	##<< standard deviation of parameterPrior
    #, weightMisfitPar2000 = NA	##<< weight of misfit of difference between
    ## saturation and prediction at PAR = 2000
  , ...				##<< other arguments to
    ##\code{\link{LightResponseCurveFitter_predictLRC}}, such as VPD0, fixVPD
	, VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according
    ##to Lasslop et al 2010
	, fixVPD = (k == 0)   	##<< boolean scalar or vector of nrow theta:fixVPD
    ## if TRUE the VPD effect is not considered and VPD is not
    ## part of the computation
) {
	theta[iOpt] <- thetaOpt
	k <- theta[1]	# here k and fixVPD is only a scalar
	RHLightResponseCostC(
	  theta, flux, sdFlux, parameterPrior, sdParameterPrior
	  , ..., VPD0 = VPD0, fixVPD = fixVPD)
}
RectangularLRCFitterCVersion$methods(computeCost =
                                       RectangularLRCFitterCVersion_computeCost)








