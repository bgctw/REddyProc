#' R5 reference class for the Logistic sigmoid Light response curve
#'
#' @import methods
#' @export LogisticSigmoidLRCFitter
#' @exportClass LogisticSigmoidLRCFitter
LogisticSigmoidLRCFitter <- setRefClass('LogisticSigmoidLRCFitter'
                                        , contains = 'LightResponseCurveFitter'
## R5 reference class for the Logistic sigmoid Light response curve
##author<< TW
)

#' @export
LogisticSigmoidLRCFitter_predictGPP  <- function(
	### Logistic Sigmoid Light Response function for GPP
  Rg   	##<< ppfd [numeric] -> photosynthetic flux density
    ## [mumol / m2 / s] or Global Radiation
  , Amax	##<< vector of length(Rg): saturation (beta parameter) adjusted
    ##for effect of VPD for each line of Rg
  , alpha	##<< numeric scalar or vector of length(Rg): alpha parameter:
    ## slope at Rg = 0
) {
	##seealso<< \code{\link{LightResponseCurveFitter_predictGPP}}
	##value<< numeric vector of length(Rg) of GPP
	GPP <- Amax * tanh(alpha * Rg / Amax)
	##details<< \code{GPP <- Amax * tanh(alpha * Rg / Amax)}
}
LogisticSigmoidLRCFitter$methods(predictGPP =
                                   LogisticSigmoidLRCFitter_predictGPP)

LogisticSigmoidLRCFitter_computeGPPGradient  <- function(
  ### Logistic Sigmoid Light Response function for GPP
  Rg   	##<< ppfd [numeric] -> photosynthetic flux density
    ## [mumol / m2 / s] or Global Radiation
  , Amax	##<< vector of length(Rg): saturation (beta parameter) adjusted
    ##for effect of VPD for each line of Rg
  , alpha	##<< numeric scalar or vector of length(Rg): alpha parameter:
    ## slope at Rg = 0
) {
	##value<< numeric matrix (length(Rg), 2) of gradients of predicted
	## GPP to Amax and alpha
	#ex <- expression(  Amax * tanh(alpha * Rg / Amax) ); deriv(ex, c("Amax", "alpha"))
	.expr1 <- alpha * Rg
	.expr2 <- .expr1 / Amax
	.expr3 <- tanh(.expr2)
	.expr8 <- cosh(.expr2)^2
	.value <- Amax * .expr3
	.grad <- array(0, c(length(.value), 2L), list(NULL, c("Amax", "alpha")))
	.grad[, 1L] <- .expr3 - Amax * (.expr1 / Amax^2 / .expr8)
	.grad[, 2L] <- Amax * (Rg / Amax / .expr8)
	.grad
}
LogisticSigmoidLRCFitter$methods(computeGPPGradient =
                                   LogisticSigmoidLRCFitter_computeGPPGradient)
