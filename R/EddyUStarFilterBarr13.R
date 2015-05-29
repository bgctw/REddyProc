fitSeg1 <- function(
		### fit a segmented relationship according to eq 1b of Barr13
		x
		,y
		,n=length(x)
){
	xr <- -x
	lm1 <- lm(y~1)
	seg1 <- segmented(lm1, seg.Z=~xr, psi= xr[n%/%2])
	cf <- as.numeric(coef(seg1))
	##value<< numeric vector with entries
	c( 
			b0=cf[1]			##<< intercept of second part
			, b1=-cf[2]		##<< first slope (second slope is fixed to zero)
			, cp=-seg1$psi[2]	##<< estimated breakpoint
			, p=anova(lm(y~xr), seg1)[[6]][2]	##<< probability of F test that segmented model is better than a linear model
	)
}

.tmp.f <- function(){
	plot( y ~ xr)
	abline(lm1)
	lines(seg1)
}


fitSeg2 <- function(
		### fit a segmented relationship according to eq 1a of Barr13
		x
		,y
		,n=length(x)
){
	xr <- -x
	lm1 <- lm(y~xr)
	seg1 <- segmented(lm1, seg.Z=~xr, psi= xr[n%/%2])
	cf <- as.numeric(coef(seg1))
	##value<< numeric vector with entries
	c( a0=cf[1]			##<< intercept of second part
		, a1=-cf[3]		##<< first slope
		, a2=-cf[2]		##<< second slope
		, cp=-seg1$psi[2]	##<< estimated breakpoint
		, p=anova(lm1, seg1)[[6]][2]	##<< probability of F test that segmented model is better than linear model
	)
}
attr(fitSeg2,"ex") <- function(){
	n <- 11L
	x <- seq(0L,1L,length.out=n)
	noise <- rnorm(n, sd=0.1)
	y1 <- y2 <- y3 <- rep(1,n) + noise
	iSlope <- 1:(n/2L) 
	y2[iSlope] <- 0.5 + x[iSlope] + noise[iSlope]  
	y3[iSlope] <- 0.2 + (0.8/0.5)*x[iSlope] + noise[iSlope]
	y <- y2
	plot( y ~ x)
	cf2 <- fitSeg2(x,y)
	cf1 <- fitSeg1(x,y)
}

.tmp.f <- function(){
	trace(fitSeg1, recover)	#untrace(fitSeg1)
	tmp <- fitSeg1(dsiSortTclass[,UstarColName], dsiSortTclass[,NEEColName])
}

