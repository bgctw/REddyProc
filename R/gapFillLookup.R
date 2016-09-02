# generalizing Antje's GapFilling Code 
# - to arbitrary Covariates for LUT
# - to windows independent of days for LUT
# - to arbitrary time spacing (nRecInDay) in MDC 
# - to arbitraty criteria of covariates to judge if record is near target in LUT 
# - to have a non-R5 LUT function that can be applied also to other problems 


# by default checks on diff < tol, but with with specifying a differnent fIsInDistance
# also diff <= tol is possible.

# despite the generalization achieved a speedup of about 5

gfGapFillMDS <- function(
		### Successive Gap-Filling with increasing window sizes and omitting covariates based on Rg, VPD, Tair, and diurnal cycle
		ds					##<< data.frame with variables to fill and covariates
		,fillVarName="NEE"	##<< scalar string: name of variable to fill
		,covarVarNames = c("Rg","VPD","Tair")	##<< string vector of column names that hold covariates to determine similar conditions
		,tolerance=c(Rg=50, VPD=5, Tair=2.5)	##<< numeric vector of tolerances with named components for each entry in meteoVarNames
		#,filledVarName=paste0(fillVarName,"_f")	##<< scalar string: name of variable that holds the filled values
		,RgVarNames=c("Rg")	##<< string vector: column names for which radiation relative tolerance bounds is to be applied (see \code{\link{gfCreateRgToleranceFunction}})
		,nRecInDay=48L		##<< integer scalar: number of records within one day, default corresponds to half-hourly records
		,isFillAll=FALSE	##<< set to TRUE to compute statistics also for good quality records
		,isVerbose=TRUE		##<< set to FALSE to suppress progress messages
		,qfVarName='none'   ##<< scalar string: name of variable withe integer quality flag 
		,qfValue=NA_real_  	##<< integer scalar: Value of quality flag for _good_ (original) data, other data is set to missing
		,maxGapSize=nRecInDay*60L	##<< integer scalar: maximum number of records in a gap, that is still be filled, longer gaps are not filled	
		,...				##<< further arguments to \code{\link{gfGapFillLookupTable}}, such as \code{minNSimilar}
){
	##value<< data.frame of the same length as ds with variables <fillVarname>_f, 
	iTol <- match(covarVarNames, names(tolerance))
	if( any(is.na(iTol))) stop("no tolerance given for meteoVarNames ",paste(covarVarNames[is.na(iTol)],collapse=","))
	toFill <- fSetQF(ds, fillVarName,qfVarName, qfValue, 'sFillInit') # discard bad quality data as data with similar conditions
	iColsMeteo <- .checkMeteoConditions(ds,covarVarNames)
	colNamesMeteo <- names(ds)[iColsMeteo]
	fTol = gfCreateRgToleranceFunction(
			tolerance=tolerance[colNamesMeteo]
			, iRgColumns=na.omit(match(colNamesMeteo,RgVarNames)) )
	fTol1 = gfCreateRgToleranceFunction(
			tolerance=tolerance[colNamesMeteo[1L] ]
			, iRgColumns=na.omit(match(colNamesMeteo[1L], RgVarNames)) )
	Met.n <- length(iColsMeteo)
	##value<< a data.frame with columns of sta
	## \item{fmean}{mean of similar values}
	## \item{fnum}{number of similar values}
	## \item{fsd}{standard deviation of similar values}
	## \item{fmeth}{method of determining similar values; 1 = at least 3 covariates, 2 = at least one covariate, 3 = mean diurnal course}
	## \item{fqc}{quality flag;  0 = original, 1 = most reliable, 2 = medium, 3 = least reliable}
	## \item{filled}{if orignial value is finite, original value, else fmean}
	gapStats <- matrix(NA_real_, nrow=nrow(ds), ncol=5L, dimnames=list(NULL,c('fmean','fnum','fsd','fmeth','fqc')))
	isGap <- !is.finite(toFill)
	isToFill <- .excludeLongGaps(isGap, maxGapSize=maxGapSize )
	if( isTRUE(isFillAll)){
		isToFill <- isToFill || !isGap	
	} 
	iRecsGap <- which(isToFill & !is.finite(gapStats[,"fmean"]))
	if( length(iRecsGap) && (Met.n >= 3L) ){
		if( isVerbose ) message("LookUpTable of all covariates with +-7 days")
		# Step 1: Look-up table (method 1) with window size +-7 days
		ans <- gfGapFillLookupTable(toFill, 7L*nRecInDay, ds[,iColsMeteo], fTol
						,isVerbose=isVerbose, ..., iRecsGap = iRecsGap )
		gapStats[ans[,"index"], 1:3] <- ans[,2:4]
		gapStats[ans[,"index"], "fmeth"] <- 1L 
		gapStats[ans[,"index"], "fqc"] <- 1L
		iRecsGap <- which(isToFill & !is.finite(gapStats[,"fmean"]))
	}
	if( length(iRecsGap) && (Met.n >= 1L) ){
		# Step 2: Look-up table (method 1) with window size +-14 days
		if( isVerbose ) message("LookUpTable of all covariates with +-14 days")
		ans <- gfGapFillLookupTable(toFill, 14L*nRecInDay, ds[,iColsMeteo], fTol
				,isVerbose=isVerbose, ..., iRecsGap = iRecsGap )
		gapStats[ans[,"index"], 1:3] <- ans[,2:4]
		gapStats[ans[,"index"], "fmeth"] <- 1L
		gapStats[ans[,"index"], "fqc"] <- 2L
		iRecsGap <- which(isToFill & !is.finite(gapStats[,"fmean"]))
	}
	if( length(iRecsGap) && (Met.n >= 1L)  ){
		# Step 3: Look-up table, Rg only (method 2) with window size +-7 days,
		if( isVerbose ) message("LookUpTable of only first covariates with +-7 days")
		ans <- gfGapFillLookupTable(toFill, 7L*nRecInDay, ds[,iColsMeteo[1L] ], fTol1 
				,isVerbose=isVerbose, ..., iRecsGap = iRecsGap )
		gapStats[ans[,"index"], 1:3] <- ans[,2:4]
		gapStats[ans[,"index"], "fmeth"] <- 2L
		gapStats[ans[,"index"], "fqc"] <- 1L
		iRecsGap <- which(isToFill & !is.finite(gapStats[,"fmean"]))
	} 
	if( length(iRecsGap) ){
		# Step 4: Mean diurnal course (method 3) with window size 0 (same day)
		# need to include hours of this day
		if( isVerbose ) message("MeanDiurnalCourse +- half a day")
		ans <- gfGapFillMeanDiurnalCourse(toFill, winExtInDays=0, nRecInDay = nRecInDay
				,isVerbose=isVerbose, ..., iRecsGap = iRecsGap )
		gapStats[ans[,"index"], 1:3] <- ans[,2:4]
		gapStats[ans[,"index"], "fmeth"] <- 3L
		#if( lVAR_fwin.n <= 1 ) lVAR_fqc.n <- 1
		#if( lVAR_fwin.n >  1 & lVAR_fwin.n <= 5 ) lVAR_fqc.n <- 2  
		#if( lVAR_fwin.n >  5 ) lVAR_fqc.n <- 3	
		gapStats[ans[,"index"], "fqc"] <- 1L
		iRecsGap <- which(isToFill & !is.finite(gapStats[,"fmean"]))
	}
	# Step 5: Mean diurnal course (method 3) with window size +-1, +-2 days
	# +0.75: also include this day, i.e. +-1/2 day and adjacent hours
	for( winExtInDays in 1:2){
		if( !length(iRecsGap) ) break
		if( isVerbose ) message("MeanDiurnalCourse with +-",winExtInDays,"days")
		ans <- gfGapFillMeanDiurnalCourse(toFill, winExtInDays=winExtInDays, nRecInDay = nRecInDay
				,isVerbose=isVerbose, ..., iRecsGap = iRecsGap )
		gapStats[ans[,"index"], 1:3] <- ans[,2:4]
		gapStats[ans[,"index"], "fmeth"] <- 3L
		gapStats[ans[,"index"], "fqc"] <- 2L
		iRecsGap <- which(isToFill & !is.finite(gapStats[,"fmean"]))
	}
	# Step 6: Look-up table (method 1) with window size +-21, +-28, ..., +-70
	if( Met.n >= 3 ) for( winExtInDays in seq(21,70,7) ){
			if( !length(iRecsGap) ) break
			if( isVerbose ) message("LookUpTable of all covariates with +-",winExtInDays," days")
			ans <- gfGapFillLookupTable(toFill, nRecInDay*winExtInDays, ds[,iColsMeteo], fTol
					,isVerbose=isVerbose, ..., iRecsGap = iRecsGap )
			gapStats[ans[,"index"], 1:3] <- ans[,2:4]
			gapStats[ans[,"index"], "fmeth"] <- 1L 
			gapStats[ans[,"index"], "fqc"] <- if( winExtInDays*2L > 56L ) 3L else 2L
			iRecsGap <- which(isToFill & !is.finite(gapStats[,"fmean"]))
		}
	# Step 7: Look-up table (method 2, Rg only) with window size +-14, +-21, ..., +-70  
	if( Met.n >= 1) for( winExtInDays in seq(14,70,7) ){
			if( !length(iRecsGap) ) break
			if( isVerbose ) message("LookUpTable with only first covariates with +-",winExtInDays," days")
			ans <- gfGapFillLookupTable(toFill, winExtInDays*nRecInDay, ds[,iColsMeteo[1L] ], fTol1 
					,isVerbose=isVerbose, ..., iRecsGap = iRecsGap )
			gapStats[ans[,"index"], 1:3] <- ans[,2:4]
			gapStats[ans[,"index"], "fmeth"] <- 1L
			gapStats[ans[,"index"], "fqc"] <- 3L 
			iRecsGap <- which(isToFill & !is.finite(gapStats[,"fmean"]))
		} 
	# Step 8: Mean diurnal course (method 3) with window size +-7, +-14, ..., +-210 days 
	for( winExtInDays in seq(7,210,7) ){
		if( !length(iRecsGap) ) break
		if( isVerbose ) message("MeanDiurnalCourse with +-",winExtInDays,"days")
		ans <- gfGapFillMeanDiurnalCourse(toFill, winExtInDays=winExtInDays, nRecInDay = nRecInDay
				,isVerbose=isVerbose, ..., iRecsGap = iRecsGap )
		gapStats[ans[,"index"], 1:3] <- ans[,2:4]
		gapStats[ans[,"index"], "fmeth"] <- 3L
		gapStats[ans[,"index"], "fqc"] <- 3L
		iRecsGap <- which(isToFill & !is.finite(gapStats[,"fmean"]))
	} 
	gapStats <- as.data.frame(gapStats) 
	iFinite <- is.finite(toFill)
	gapStats$filled <- gapStats$fmean 
	# replace means of non-gaps by original values
	gapStats$filled[iFinite] <- toFill[iFinite]  
	gapStats$fqc[iFinite] <- 0
	gapStats
}

.checkMeteoConditions <- function(
		### Check availablility of meteorological data for LookUpTable
		ds 					##<< data.frame
		,meteoVarNames = c("Rg","VPD","Tair")	##<< string vector of column names that hold meteo conditions
){
	namesDs <- names(ds)
	iColsMeteo0 <- structure( match( meteoVarNames, namesDs), names=meteoVarNames)
	if( any(is.na(iColsMeteo0))) warning(
				"The following meteo columns are missing in provided dataset: "
				,paste(meteoVarNames[is.na(iColsMeteo0)],collapse=","))
	iColsMeteo1 <-na.omit(iColsMeteo0)
	hasRecords <- ( sapply( iColsMeteo1, function(iCol){ sum(!is.na(ds[,iCol])) } ) != 0 )
	if( any(!hasRecords) ) warning(
				"The following meteo columns are have only missings: "
				,paste(namesDs[iColsMeteo1][!hasRecords]),col=",")
	iColsMeteo <- iColsMeteo1[hasRecords]
	##value<< integer vector of column indices in  ds that hold valid meteorological conditions.
	## Issues warning for invalid columns among meteoVarNames
	iColsMeteo
}

.excludeLongGaps <- function(
		### mark long gaps as FALSE
		isGapToFill		##<< logical vector specifying with TRUE for a gap 
		, maxGapSize	##<< the maximum size of the gap
){
	cumGapSize <- cumsum(isGapToFill)
	gapSizes <- cumGapSize-cummax((!isGapToFill)*cumGapSize)
	#gapSizesExp <- fCalcLengthOfGaps(toFill); all(gapSizesExp == gapSizes)
	while ( max(gapSizes) > maxGapSize ) {
		iRecEndGap <- which(gapSizes == max(gapSizes))
		iRecStartGap <- iRecEndGap - max(gapSizes) + 1
		isGapToFill[iRecStartGap:iRecEndGap] <- FALSE
		gapSizes[iRecStartGap:iRecEndGap] <- -1 #Set to -1 since accounted for
		warning('sMDSGapFill::: The long gap between position ', iRecStartGap, ' and ', iRecEndGap, ' will not be filled!')
	}
	##value<< first argument with long gaps set to FALSE
	isGapToFill
}


gfGapFillLookupTable = function(
		###  Gap filling with Look-Up Table (LUT)
		##description<<
		## Look-Up Table (LUT) algorithm of up to five conditions within prescribed window size
		toFill				##<< numeric vector to be filled (with NA also for gaps that have been already filled with a narrower window) 
		,winExt=3L*48L		##<< scalar integer: number of records to extend window in both directions, Window size in records is then 2*winExtDays+1L
		,covM				##<< numeric data.frame or matrix with covariates in columns and no other columns
		,fTolerance=		##<< function that returns tolerance vector depending on target vector of covariates
				## It specifies for each entriy in target, on how much the covariates can deviate from it and still regarded as similar.
				## The order of entries must correspond to the columns in covM.
				function(target) return(tolerance)
		,fIsInTolerance=		##<< function to decide wheter covariates of a record are close enough to the target. 
				## Allows to use other criteria of combined covariatess than the default of all separately within tolerance.
				## The function must take the same parameters and return a vector for each column in covM as in \code{\link{gfIsAllWithinAbsoluteTolerance}}.
				gfIsAllWithinAbsoluteTolerance
		,tolerance			##<< numeric vector: alternative way to specify a constant tolerance, returned by the default fTolerance function
		,iRecsGap = which(!is.finite(toFill))	##<< integer vector of positions of gaps to fill.
			## Set to a subset if some of gaps were filled before.
			## Set to seq_along(toFill) to estimate uncertainty for all records.
		#,isFillAll=FALSE	##<< logical scalar: set to TRUE to get fill statistics for all records instead of gaps only
		,isVerbose=TRUE     ##<< logical scalar: set to FALSE to avoid print status information to screen
		,minNSimilar=2L		##<< integer scalar: number of records of similar conditions to fill gap
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
	# for each gap the start and end record index of the window
	iRecsStart <- pmax(1L, iRecsGap - winExt)	# little speedup by vectorizing instead of max in each loop
	iRecsEnd <- pmin(nRec, iRecsGap + winExt)
	# make sure tol is of correct lenght, later accessed by tol[] <-
	if( length(fTolerance(covT[,1L])) != nCov ) warning("provided vector of tolerances is not of length as number of provided covariates.")
	tol <- numeric(nCov)
	##value<< 
	## A matrix with with rows with statistics of values in similar conditions
	## index: rownumber of filled record, mean: mean across all similar records in period, fnum: number of similar records, fsd: standard deviation of the mean 
	gapStats <- matrix(NA_real_, nrow=length(iRecsGap), ncol=4L, dimnames=list(NULL,c('index','mean','fnum','fsd')))
	if( length(iRecsGap) == 0 ) return(gapStats)
	iGap <- 1L	# loop variable cycling the positions in iRecsGap
	for( iGap in seq_along(iRecsGap) ) {
		# Set window size
		iRec   <- iRecsGap[iGap]
		seqWin <- (iRecsStart[iGap]:iRecsEnd[iGap])
		target <- covT[,iRec]		# vector of covariates at target
		toFillInWindow <- toFill[seqWin] 
		covWin <- covT[,seqWin,drop=FALSE]		# matrix of covariates in window
		tol[] <- fTolerance(target)		
		isSimilar <- fIsInTolerance(covWin, target, tol)
		# not itself the target record & finite value of fill variable & and covariates in tolerance range
		# for uncertainty estimation, actually may include target record
		#isFiniteAndSimilar <- (seqWin != iRec) & isFiniteToFill[seqWin] & isSimilar
		isFiniteAndSimilar <- isFiniteToFill[seqWin] & isSimilar
		iRecSimilar <- seqWin[isFiniteAndSimilar]
		similarVals <- toFillInWindow[isFiniteAndSimilar]
		# If enough available data, fill gap
		nSimilarVals <- length(similarVals)
		if( nSimilarVals >= minNSimilar ){
			#if( iRec == 15167  ) recover()		
			#if( iRec == 888  ) recover()		
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
		if( isVerbose && iGap%%100 == 0 ) message('.', appendLF=(iGap%%6000 == 0))
	}
	ans <- gapStats[is.finite(gapStats[,1]),,drop=FALSE]	# skip the rows that could not filled (due to not enough data)
	if( isVerbose ){
		message('GapStats of ', nrow(ans),' of ',nrow(gapStats),' gap-records.')
	}   
	ans
}

gfIsAllWithinAbsoluteTolerance <- function(
		### test for each column that absolute differences are smaller than tolerance
		covWin		##<< numeric matrix with covariates in rows and records in columns
		, target	##<< numeric vector of covariates at target record
		, tol		##<< numeric vector of tolerances for each covariate
		, isCovNAInTolerance=FALSE	##<< logical scalar: set to TRUE to assume that covariates with NA are within tolerance (default to assume its not)
		, nCov=nrow(covWin)	##<< may provide for slight speedup	
){
	deviationCov <- covWin - target
	#isInTolerance <- abs(deviationCov) <= tol	# matrix (nCov x nRec) # better for constraining discrete values
	isInTolerance <- abs(deviationCov) < tol	# matrix (nCov x nRec) # for compatibility with older code
	isInTolerance[ is.na(isInTolerance) ] <- isCovNAInTolerance	# some assumption for NA values	of covariates
	isAllVarsInTolerance <- (colSums(isInTolerance) == nCov) # for each record in window, if all variables are within tolerance
	##value<< logical vector of length ncol(covWin) with TRUE if column is within tolerance
	isAllVarsInTolerance	
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


gfGapFillMeanDiurnalCourse = function(
		###  Gap filling with Mean Diurnal Course (MDC)
		toFill				##<< numeric vector to be filled
		, winExtInDays		##<< half-window width in days, full window will be 
		,...				##<< other arguments to \code{\link{gfGapFillLookupTable}}, such as \code{minNSimilar}, or\code{iRecsGap}
		,nRecInDay=48L		##<< integer scalar: number of records within one day, default corresponds to half-hourly records
		,toleranceInHours=1 ##<< numeric scalar: maximum difference in "hour since daystart" to select similar values 	
){
	##author<< TW
	##description<<
	## Mean Diurnal Course (MDC) algorithm based on average values within +/- one hour of adjacent days.
	#
	# days forward and backward extended + tolerance of hours
	winExt <- ceiling(winExtInDays*nRecInDay + toleranceInHours*(nRecInDay/24))
	# create covariate hourInDay and use it for LookupTable
	recHours <- (1:nRecInDay)*(24/nRecInDay)
	covM <- matrix(recHours, nrow=length(toFill), ncol=1, dimnames=list(NULL,"hourInDay"))
	##value<< 
	## result of \code{\link{gfGapFillLookupTable}}
	#ans <- gfGapFillLookupTable(toFill, winExt=winExt, ..., covM=covM, tolerance=toleranceInHours ) #+1e-5 to satisfy inequaly constraint <tol 
	ans <- gfGapFillLookupTable(toFill, winExt=winExt, ..., covM=covM, tolerance=toleranceInHours, fIsInTolerance=.gfHourOfDayWithinTolerance )  
	ans
}

.gfHourOfDayWithinTolerance <- function(
		### test for single column that absolute(differences modulo 24) is within tolerance
		covWin		##<< numeric matrix with covariates in rows and records in columns
		, target	##<< numeric vector of covariates at target record
		, tol		##<< numeric vector of tolerances for each covariate
){
	#only one covariate: time within day
	hourInDay <- covWin[1,]
	deviationCov1 <- abs(hourInDay - target) 
	# 23.5 is near 0.5
	offset <- if(target < 12) + 24 else -24
	deviationCov2 <- abs((target+offset) - hourInDay)
	isInTolerance <- (deviationCov1 <= tol) | (deviationCov2 <=tol)
	# assume hours in day without missing values
	#isInTolerance[ is.na(isInTolerance) ] <- isCovNAInTolerance	# some assumption for NA values	of covariates
	isInTolerance
}





