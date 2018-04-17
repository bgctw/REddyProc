# random, percentile, density and quantile function of the logit-normal distribution
# and estimation of parameters from percentiles by Sum of Squares Newton optimization
#

logit <- function(
	### Transforming (0,1) to normal scale (-Inf Inf)
	p,...
){
	##details<<
	## function \eqn{ logit(p)= log \left( \frac{p}{1-p} \right) = log(p) - log(1-p) }
	##seealso<< \code{\link{invlogit}}
	qlogis(p,...)
}

invlogit <- function(
	### Transforming (-Inf,Inf) to original scale (0,1)
	q,...
){
	##details<<
	## function \eqn{f(z) = \frac{e^{z}}{e^{z} + 1} \! = \frac{1}{1 + e^{-z}} \!}
	##seealso<< \code{\link{logit}}
	plogis(q,...)
}

