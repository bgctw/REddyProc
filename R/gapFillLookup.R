# for Thomas to understand Antje's GapFilling Code without R5, which is hard to debug

gfGapFillLookupTable = function(
		###  Gap filling with Look-Up Table (LUT)
		##description<<
		## Look-Up Table (LUT) algorithm of up to five conditions within prescribed window size
		toFill				##<< numeric vector to be filled 
		,winExt=3L*48L		##<< scalar integer: number of records to extend window in both directions, Window size in records is then 2*winExtDays+1L
		,covM				##<< numeric data.frame or matrix with covariates in columns and no other columns
		,fTolerance=		##<< function that returns tolerance vector depending on target vector of covariates
				## It specifies for each entriy in target, on how much the covariates can deviate from it and still regarded as similar.
				## The order of entries must correspond to the columns in covM.
				function(target) return(tolerance)	
		,tolerance			##<< numeric vector: alternative way to specify a constant tolerance, returned by the default fTolerance function
		,isFillAll=FALSE	##<< logical scalar: set to TRUE to get fill statistics for all records instead of gaps only
		,isVerbose=TRUE     ##<< logical scalar: set to FALSE to avoid print status information to screen
		,minNSimilar=1L		##<< integer scalar: number of records of similar conditions to fill gap
		,isCovNAInTolerance=FALSE	##<< logical scalar: set to TRUE to assume that covariates with NA are within tolerance (default to assume its not)
){
	##author<< TW
	##details<<
	## For target record numbers statistics are computed across values of the variable during 
	## neighboring times with similar conditons.
	## This can be used to fill gaps by the mean of similar values, or to estimate the uncertainty of the values.
	## Neighboring times are specified by a window (argument \code{winExt})
	## Similar conditions are specified by providing covariate variables and for each a tolerance on 
	## how much they may deviate from covariate values of the target record.
	#
	# in order to vectorize operations across the different covariate-variables, 
	# transform so that each column represents all variables. 
	# Recycling of one vector of covariate variables allows to say e.g. matrix - vector
	covT <- t(as.matrix(covM))	
	#isFiniteCov <- is.finite(covT)
	nRec <- ncol(covT)
	nCov <- nrow(covT)
	isFiniteToFill <- is.finite(toFill)	# compute once and subset afterwards is faster than computing it for every window
	#toFillAll <- if( isFillAll ) numeric(nRec) else toFill
	iRecsGap <- if( isFillAll) 1:nRec else which(!isFiniteToFill)
	# make sure tol is of correct lenght, later accessed by tol[] <- 
	tol <- numeric(nCov)
	##value<< 
	## A matrix with with rows with statistics of values in similar conditions
	## index: rownumber of filled record, mean: mean across all similar records in period, fnum: number of similar records, fsd: standard deviation of the mean 
	gapStats <- matrix(NA_real_, nrow=length(iRecsGap), ncol=4L, dimnames=list(NULL,c('index','mean','fnum','fsd')))
	if( length(iRecsGap) == 0 ) return(gapStats[FALSE,] )
	iGap <- 1L	# loop variable cycling the positions in iRecsGap
	for( iGap in seq_along(iRecsGap) ) {
		# Set window size
		iRec   <- iRecsGap[iGap]
		iRecStart <- max(1, iRec - (winExt))
		iRecEnd   <- min(nRec, iRec + (winExt))
		seqWin <- (iRecStart:iRecEnd)
		target <- covT[,iRec]		# vector of covariates at target
		covWin <- covT[,seqWin]		# matrix of covariates in window
		# Set LUT fill condition
		deviationCov <- covWin - target
		tol[] <- fTolerance(target)		
		isInTolerance <- abs(deviationCov) < tol	# matrix (nCov x nRec) 
		isInTolerance[ is.na(isInTolerance) ] <- isCovNAInTolerance	# some assumption for NA values	of covariates
		isAllVarsInTolerance <- apply( isInTolerance, 2, all)	# for each record in window, if all variables are within tolerance
		# not itself the target record & finite value of fill variable & and covariates in tolerance range
		isSimilar <- (seqWin != iRec) & isFiniteToFill[seqWin] & isAllVarsInTolerance
		similarVals <- toFill[seqWin][isSimilar]
		# If enough available data, fill gap
		if( length(similarVals) > minNSimilar ){
			nSimilarVals <- length(similarVals)
			meanSimilarVals <- sum(similarVals)/nSimilarVals
			resMean <- similarVals-meanSimilarVals
			sdSimilarVals <- sqrt(sum(resMean*resMean)/(nSimilarVals-1L))
			#sdSimilarVals2 <- sd(similarVals)	# to check compuation
			gapStats[iGap,1:4] <- c(index=iRec
							,mean=meanSimilarVals
							,fnum=nSimilarVals
							,fsd=sdSimilarVals
						)
		}
		if( isVerbose && iGap%%100 == 0 ){
			message('.', appendLF=(iGap%%6000 == 0))
		}  
	}
	if( isVerbose ) message('', nrow(gapStats))
	ans <- gapStats[is.finite(gapStats[,1]),]	# skip the rows that were not filled because of too few data
	ans
}


gfCreateRgToleranceFunction <- function(
		### create a tolerance function for that adjusts tolerances for Rg column
		tolerance	##<< the vector of tolerances	
		,iRgColumns	##<< the position of radiation in target
){
	##details<< 
	## The order of entries in tolerance, and iRgColumns must match the order to covariates 
	## supplied to \code{\link{gapFillLUT}} 
	toleranceClosure = tolerance
	iRgColumnsClosure = iRgColumns
	fToleranceRg <- function(
			### Adjust tolerance of matching similar condionts of radiation column
			target			##<< vector of covariates of current gap
	){
		##details<<
		## Reduce tolerance of radiation column to [20, min(Rg_tolerance, Rg_target)] depending on the target radiation
		toleranceClosure[iRgColumnsClosure] <- 
				pmax(pmin(toleranceClosure[iRgColumnsClosure], abs(target[iRgColumnsClosure]), na.rm=T), 20, na.rm=T)
		##value<< tolerance given at creation of the function with entry for radiation columns
		toleranceClosure
	}
	
}


gfComputeQualityFlags <- function(
		winWidthDays	##<< scalar integer: full windows length in days
		,nCov			##<< scalar integer: number of covariates
){
	#Set window size and quality flag
	fmeth <- NA_integer_
	qualityFlag <- 1L;
	if( nCov >= 3L ){ #Three conditions
		fmeth <- 1
		#! Limit '14' congruent with MR PV-Wave, in paper different limit of '28' (stated as single window size of 14 days)
		if( winWidthDays > 14L ) qualityFlag <- 2L
		if( winWidthDays > 56L ) qualityFlag <- 3L
	}
	if( nCov == 1L ) { #One condition only
		fmeth <- 2
		if( winWidthDays > 14L ) qualityFlag <- 2
		if( winWidthDays > 28L ) qualityFlag <- 3          
	} 
	##value<< integer vector with components #TODO
	c( fmeth=fmeth
		, qualityFlag=qualityFlag
		, winWidthDays=winWidthDays 
	)
}
.tmp.f <- function(){
	qualityFlags <- .setQualityFlag( winWidthDays=winWidthDays, nCov=nCov )
	ans[,'fmeth'] <- qualityFlags[1]
	ans[,'fqc'] <- qualityFlags[2]
	ans[,'fwin'] <- winWidthDays
}



