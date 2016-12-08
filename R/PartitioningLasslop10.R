partitionNEEGL=function(
		### Partitioning NEE fluxes into GP and Reco after daytime method of Lasslop et al. (2010)
		ds						##<< dataset with all the specified input columns and full days in equidistant times		
		,NEEVar.s=paste0('NEE',SuffixDash.s,'_f')      ##<< Variable of Net Ecosystem Exchange flux
		,QFNEEVar.s=paste0('NEE',SuffixDash.s,'_fqc')  ##<< Quality flag of variable
		,QFNEEValue.n=0         					   ##<< Value of quality flag for _good_ (original) data
		,NEESdVar.s=paste0('NEE',SuffixDash.s,'_fsd')       ##<< Variable of standard deviation of net ecosystem fluxes
		,TempVar.s=paste0('Tair_f')     ##<< Filled air or soil temperature variable (degC)
		,QFTempVar.s=paste0('Tair_fqc') ##<< Quality flag of filled temperature variable
		,QFTempValue.n=0       ##<< Value of temperature quality flag for _good_ (original) data
		,VPDVar.s=paste0('VPD_f')     ##<< Filled Vapor Pressure Deficit - VPD - (hPa)
		,QFVPDVar.s=paste0('VPD_fqc') ##<< Quality flag of filled VPD variable    
		,QFVPDValue.n=0        ##<< Value of VPD quality flag for _good_ (original) data
		,RadVar.s='Rg'         ##<< Unfilled (original) radiation variable
		,PotRadVar.s="PotRad_NEW"	##<< Variable name of potential radiation (W/m2)			   
		,Suffix.s = ""		   ##<< string inserted into column names before identifier (see \code{\link{sMDSGapFillAfterUstar}}).
		,controlGLPart=partGLControl()	##<< further default parameters, see \code{\link{partGLControl}}
		,isVerbose=TRUE			 ##<< set to FALSE to suppress output messages
		,nRecInDay=48L		 ##<< number of records within one day (for half-hourly data its 48)
)
##description<<
## daytime-based partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco)
##author<<
## MM, TW
##references<<
## Lasslop G, Reichstein M, Papale D, et al. (2010) Separation of net ecosystem exchange into assimilation and respiration using 
## a light response curve approach: critical issues and global evaluation. Global Change Biology, Volume 16, Issue 1, Pages 187208
{
	'Partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco) using Lasslop et al., 2010'
	# Check if specified columns exist in sDATA or sTEMP and if numeric and plausible. Then apply quality flag
	SuffixDash.s <- paste( (if(fCheckValString(Suffix.s)) "_" else ""), Suffix.s, sep="")
	'Partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco) using Lasslop et al., 2010'
	# Check if specified columns exist in sDATA or sTEMP and if numeric and plausible. Then apply quality flag
	fCheckColNames(ds, c(NEEVar.s, QFNEEVar.s, TempVar.s, QFTempVar.s, RadVar.s, PotRadVar.s, NEESdVar.s), 'sGLFluxPartition')
	fCheckColNum(ds, c(NEEVar.s, QFNEEVar.s, TempVar.s, QFTempVar.s, RadVar.s, PotRadVar.s, NEESdVar.s), 'sGLFluxPartition')
	fCheckColPlausibility(ds, c(NEEVar.s, QFNEEVar.s, TempVar.s, QFTempVar.s, RadVar.s, PotRadVar.s), 'sGLFluxPartition')
	Var.V.n <- fSetQF(ds, NEEVar.s, QFNEEVar.s, QFNEEValue.n, 'sGLFluxPartition')
	if( isVerbose ) message('Start daytime flux partitioning for variable ', NEEVar.s, ' with temperature ', TempVar.s, '.')
	##value<< data.frame with columns
	## \item{Reco_DT_<suffix>}{predicted ecosystem respiraiton: mumol CO2/m2/second}
	## \item{GPP_DT_<suffix>}{predicted gross primary production mumol CO2/m2/second}
	RecoDTVar.s <- paste0('Reco_DT',SuffixDash.s) 
	GPPDTVar.s <- paste0('GPP_DT',SuffixDash.s) 
	RecoDTSdVar.s <- paste0(RecoDTVar.s,"_SD") 
	GPPDTSdVar.s <- paste0(GPPDTVar.s,"_SD") 
	dsAns <- data.frame(
			FP_VARnight=rep(NA_real_,nrow(ds))	##<< NEE filtered for nighttime records (others NA)
			,FP_VARday=NA_real_		##<< NEE filtered for daytime recores (others NA)
			,NEW_FP_Temp=NA_real_	##<< temperature after filtering for quality flag degree Celsius
			,NEW_FP_VPD=NA_real_	##<< vapour pressure deficit after filtering for quality flag, hPa
			,FP_R_refNight=NA_real_		##<< basal respiration estimated from LRC of daytime window  (W/m2)
			,FP_R_ref=NA_real_		##<< basal respiration estimated from LRC of daytime window  (W/m2)
			,FP_E0=NA_real_			##<< temperature sensitivity estimated from nighttime NEE window  in Kelvin (degK) 
		#TODO Mirco: add descriptions to meaning of LRC parameters
			,FP_alpha=NA_real_			##<< 
			,FP_beta=NA_real_			##<< 
			,FP_k=NA_real_				##<<
			,FP_qc=NA_integer_			##<< quality flag: 0: good parameter fit, 1: some parameters out of range, required refit, 2: next parameter estimate is more than two weeks away
			,FP_dRecPar=NA_integer_		##<< records until or after closest record that has a parameter estimate associated
	)
	## \item{}{Light response curve parameters \code{FP_X} are estimated for windows, and are reported with the first record of the window}
	##end<<
	# Filter night time values only
	#! Note: Rg <= 4 congruent with Lasslop et al., 2010 to define Night for the calculation of E_0.n
	# Should be unfilled (original) radiation variable, therefore dataframe set to sDATA only
	isNight <- (ds[,RadVar.s] <= 4 & ds[[PotRadVar.s]] == 0)
	dsAns$FP_VARnight <- ifelse(isNight, Var.V.n, NA)
	attr(dsAns$FP_VARnight, 'varnames') <- paste(attr(Var.V.n, 'varnames'), '_night', sep='')
	attr(dsAns$FP_VARnight, 'units') <- attr(Var.V.n, 'units')
	# Filter day time values only
	#! Note: Rg > 4 congruent with Lasslop et al., 2010 to define Day for the calculation of paremeters of Light Response Curve 
	# Should be unfilled (original) radiation variable, therefore dataframe set to sDATA only
	isDay=(ds[,RadVar.s] > 4 & ds[[PotRadVar.s]] != 0)
	dsAns$FP_VARday <- ifelse(isDay, Var.V.n, NA)
	attr(dsAns$FP_VARday, 'varnames') <- paste(attr(Var.V.n, 'varnames'), '_day', sep='')
	attr(dsAns$FP_VARday, 'units') <- attr(Var.V.n, 'units')
	#! New code: Slightly different subset than PV-Wave due to time zone correction (avoids timezone offset between Rg and PotRad)
	# Apply quality flag for temperature and VPD
	# TODO: docu meteo filter, standard FALSE
	dsAns$NEW_FP_Temp <- if( isTRUE(controlGLPart$isFilterMeteoQualityFlag) ) fSetQF(ds, TempVar.s, QFTempVar.s, QFTempValue.n, 'partitionNEEGL') else ds[[TempVar.s]]
	dsAns$NEW_FP_VPD <- if( isTRUE(controlGLPart$isFilterMeteoQualityFlag) ) fSetQF(ds, VPDVar.s, QFVPDVar.s, QFVPDValue.n, 'partitionNEEGL') else ds[[VPDVar.s]]
	#Estimate Parameters of light response curve: R_ref, alpha, beta and k according to Table A1 (Lasslop et al., 2010)
	# save(ds, file="tmp/dsTestPartitioningLasslop10.RData")
	# extract the relevant columns in df with defined names (instead of passing many variables)
	dsR <- data.frame(
			sDateTime=ds[[1]]		# not used, but usually first column is a dateTime is kept for aiding debug
			,NEE=Var.V.n
			,sdNEE=ds[[NEESdVar.s]]
			, Temp=dsAns$NEW_FP_Temp
			, VPD=dsAns$NEW_FP_VPD
			, Rg=ds[[RadVar.s]]
			, isDay=isDay
			, isNight=isNight
	) 
	##seealso<< \code{\link{partGLFitLRCWindows}}
	resParms <- partGLFitLRCWindows( dsR
			, nRecInDay=nRecInDay
			, controlGLPart=controlGLPart
	)
	if( FALSE ){
		resLRCOld <- tmp <- .depr.partGLFitLRCWindows( dsR
				, nRecInDay=nRecInDay
				, controlGLPart.l=controlGLPart
		)
		resParms <- resLRCOld
	}
	# append parameter fits to the central record of day window
	#iGood <- which(resLRC$summary$parms_out_range == 0L)
	# resLRC provides parameters only for a subset of rows. For each row in the original data.frame
	# The parameter estimates are associated for interpolation of predicted fluxes 
	# either to the central record of the window, 
	# or the record with the time corresponding to the mean of all valid records in the window
	# default is isAssociateParmsToMeanOfValids=TRUE (double check partGLControl argument)
	colNameAssoc <- if( isTRUE(controlGLPart$isAssociateParmsToMeanOfValids) ) "iMeanRec" else "iCentralRec"
	# for the output, always report at central record
	dsAns[resParms$summary$iCentralRec,c("FP_R_refNight","FP_E0","FP_R_ref","FP_alpha","FP_beta","FP_k","FP_qc")] <- 
					resParms$summary[,c("R_ref_night","E_0","R_ref","a","b","k","parms_out_range")]
	matchFP_qc <- NA_integer_; matchFP_qc[resParms$summary[[colNameAssoc]] ] <- resParms$summary$parms_out_range	# here maybe indexed by meanRec
	#	
	##seealso<< \code{\link{partGLInterpolateFluxes}}
	dsAnsFluxes <- partGLInterpolateFluxes( ds[,RadVar.s]
					#, dsAns$NEW_FP_VPD, dsAns$NEW_FP_Temp		
					, ds[[VPDVar.s]], ds[[TempVar.s]]		# do prediction also using gap-Filled values
					, resParms
					, controlGLPart=controlGLPart
			)
	# set quality flag to 2 where next parameter estimate is more than 14 days away 
	dsAns$FP_dRecPar <- dsAnsFluxes$dRecNextEstimate
	dDaysPar <- round(dsAns$FP_dRecPar / nRecInDay)
	dsAns$FP_qc <- matchFP_qc[ 1:nrow(dsAns) + dsAns$FP_dRecPar ] # associate quality flag of parameter estimate to each record
	dsAns$FP_qc[ dDaysPar > 14] <- 2L	# set quality flag to 2 for records where next estimate is more than 14 days away
	#dsAns[is.finite(dsAns$FP_beta),]
	#
	dsAns[[RecoDTVar.s]] <- dsAnsFluxes$Reco
	attr(dsAns[[RecoDTVar.s]], 'varnames') <- RecoDTVar.s
	attr(dsAns[[RecoDTVar.s]], 'units') <- attr(Var.V.n, 'units')
	dsAns[[GPPDTVar.s]] <- dsAnsFluxes$GPP
	attr(dsAns[[GPPDTVar.s]], 'varnames') <- GPPDTVar.s
	attr(dsAns[[GPPDTVar.s]], 'units') <- attr(Var.V.n, 'units')
	if( controlGLPart$isSdPredComputed ){
		dsAns[[RecoDTSdVar.s]] <- dsAnsFluxes$sdReco
		attr(dsAns[[RecoDTSdVar.s]], 'varnames') <- RecoDTSdVar.s
		attr(dsAns[[RecoDTSdVar.s]], 'units') <- attr(Var.V.n, 'units')
		dsAns[[GPPDTSdVar.s]] <- dsAnsFluxes$sdGPP
		attr(dsAns[[GPPDTSdVar.s]], 'varnames') <- GPPDTSdVar.s
		attr(dsAns[[GPPDTSdVar.s]], 'units') <- attr(Var.V.n, 'units')
	}
	#sTEMP$GPP_DT_fqc <<- cbind(sDATA,sTEMP)[,QFFluxVar.s]
	#! New code: MDS gap filling information are not copied from NEE_fmet and NEE_fwin to GPP_fmet and GPP_fwin
	#           (since not known within this pure partitioning function)
	return(dsAns)
}

partGLControl <- function(
		### Default list of parameters for Lasslop 2010 daytime flux partitioning
		LRCFitConvergenceTolerance=1e-3	##<< convergence criterion for LRC fit. 
			## If relative improvement of reducing residual sum of squares between predictions and 
			## observations is less than this criterion, assume convergence.
			## Decrease to get more precise parameter estimates, Increase for speedup.
		,nBootUncertainty=30L			##<< number of bootstrap samples for estimating uncertainty. 
			## Set to zero to derive uncertainty from curvature of a single fit
		,minNRecInDayWindow = 10L 		##<< Minimum number of data points for regression 
		,isAssociateParmsToMeanOfValids=TRUE	##<< set to FALSE to associate parameters to 
			## the first record of the window for interpolation 
			## instead of mean across valid records inside a window
		,isLasslopPriorsApplied=FALSE	##<< set to TRUE to apply strong fixed priors on LRC fitting.	
			## Returned parameter estimates claimed valid for some case where not enough data was available 
		,isSdPredComputed=TRUE			##<< set to FALSE to avoid computing standard errors 
			## of Reco and GPP for small performance increase 	
		,isFilterMeteoQualityFlag=FALSE	##<< set to TRUE to use only records where quality flag 
			## of meteo drivers (Radation, Temperatrue, VPD) is zero, i.e. non-gapfilled for parameter estimation.
			## For prediction, the gap-filled value is used always, to produce predictions also for gaps.
		,isBoundLowerNEEUncertainty=TRUE	##<< set to FALSE to avoid adjustment of very low uncertainties before
			## day-Time fitting that avoids the high leverage those records with unreasonable low uncertainty.
		,smoothTempSensEstimateAcrossTime=TRUE	##<< set to FALSE to use independent estimates of temperature 
			## sensitivity on each windows instead of a vector of E0 that is smoothed over time
	){
	##author<< TW
	##seealso<< \code{\link{partitionNEEGL}}
	##description<<
	## For highest compatibility to Lasslop10 use 
	## \code{nBootUncertainty=0L, isAssociateParmsToMeanOfValids=FALSE
	##		, isLasslopPriorsApplied=TRUE, isBoundLowerNEEUncertainty=FALSE
	##		, smoothTempSensEstimateAcrossTime=FALSE
	##      }
	ctrl <- list(  
			LRCFitConvergenceTolerance=LRCFitConvergenceTolerance
			,nBootUncertainty=nBootUncertainty
			,minNRecInDayWindow=minNRecInDayWindow 
			,isAssociateParmsToMeanOfValids=isAssociateParmsToMeanOfValids
			,isLasslopPriorsApplied=isLasslopPriorsApplied
			,isSdPredComputed=isSdPredComputed
			,isFilterMeteoQualityFlag=isFilterMeteoQualityFlag
			,isBoundLowerNEEUncertainty=isBoundLowerNEEUncertainty
			,smoothTempSensEstimateAcrossTime=smoothTempSensEstimateAcrossTime
	)
	#display warning message for the following variables that we advise not to be changed
	#if (corrCheck != 0.5) warning("WARNING: parameter corrCheck set to non default value!")
	##value<< list with entries of given arguments.
	ctrl
}
attr(partGLControl,"ex") <- function(){
	partGLControl(nBootUncertainty=40L)
}


partGLFitLRCWindows=function(
		### Estimate temperature sensitivity and parameters of Rectangular Hyperbolic Light Response Curve function (a,b,R_ref, k) for successive periods
		ds					##<< data.frame with numeric columns NEE, sdNEE, Temp (degC), VPD, Rg, and logical columns isNight and isDay
		,WinSizeDays.i=4L	##<< Window size in days for daytime fits
		,WinSizeNight.i=3L*WinSizeDays.i	##<< Window size in days for nighttime fits  
		,DayStep.i=floor(WinSizeDays.i / 2L)##<< step in days for shifting the windows
		,isVerbose=TRUE		##<< set to FALSE to suppress messages
		,nRecInDay=48L		##<< number of records within one day (for half-hourly data its 48)
		,controlGLPart=partGLControl()		##<< list of further default parameters
){
	##seealso<< \code{\link{partGLFitNightTempSensOneWindow}}
	if( isVerbose ) message("  Estimating temperature sensitivity from night time NEE ", appendLF = FALSE)
	resNight <- applyWindows(ds, partGLFitNightTempSensOneWindow, prevRes=list(prevE0=NA)
			, winSizeRefInDays = WinSizeDays.i
			, winSizeInDays=WinSizeNight.i
			,isVerbose=isVerbose		
			,nRecInDay=nRecInDay
			#,isNAAllowed=TRUE
			#
			,controlGLPart=controlGLPart	
	)
	iNoFit <- which( is.na(resNight$summary$E0) )
	iExtend <- 1
	winExtendSizes <- WinSizeNight.i*c(2L,4L)
	while( length(iNoFit) && (iExtend <= length(winExtendSizes)) ){
		if( isVerbose ) message("    increase window size to ",winExtendSizes[iExtend], appendLF = FALSE)
		resNightExtend <- applyWindows(ds, partGLFitNightTempSensOneWindow, prevRes=list(prevE0=NA)
				, winSizeRefInDays = WinSizeDays.i
				,winSizeInDays=winExtendSizes[iExtend]
				,isVerbose=isVerbose		
				,nRecInDay=nRecInDay
				#,isNAAllowed= (iExtend < length(winExtendSizes))
				#
				,controlGLPart=controlGLPart	
		)
		resNight$resOptList[iNoFit] <- resNightExtend$resOptList[iNoFit]
		resNight$summary[iNoFit,] <- resNightExtend$summary[iNoFit,]
		#all(resNight$summary$iCentralRec == resNight24$summary$iCentralRec)
		iNoFit <- which( is.na(resNight$summary$E0) )
		iExtend <- iExtend + 1L
	}
	#
	##seealso<< \code{\link{partGLSmoothTempSens}}
	# remember E0 and sdE0 before overidden by smoothing						
	resNight$summary$E0Fit <- resNight$summary$E0
	resNight$summary$sdE0Fit <- resNight$summary$sdE0
	E0Smooth <- if( isTRUE(controlGLPart$smoothTempSensEstimateAcrossTime) ){
				if( isVerbose ) message("  Smoothing temperature sensitivity estimates")
				partGLSmoothTempSens( resNight$summary )
			}else {
				E0Smooth <- resNight$summary
				iNonFiniteE0 <- which(!is.finite(E0Smooth$E0))
				E0Smooth$sdE0[iNonFiniteE0] <- quantile(E0Smooth$sdE0, 0.9, na.rm=TRUE) # set uncertainty to the 90% quantile of the distribution of uncertainties
				E0Smooth$E0 <- fillNAForward(E0Smooth$E0)	# fill NA with value from previous window
				E0Smooth
			}
	# now all E0 and sdE0 are defined
	#
	##seealso<< \code{\link{partGLFitNightRespRefOneWindow}}
	if( isVerbose ) message("  Estimating respiration at reference temperature for smoothed temperature sensitivity from night time NEE ", appendLF = FALSE)
	resRef15 <- applyWindows(ds, partGLFitNightRespRefOneWindow
			, winSizeRefInDays = WinSizeDays.i
			, winSizeInDays=WinSizeDays.i
			,isVerbose=isVerbose		
			,nRecInDay=nRecInDay
			#
			,E0Win = E0Smooth
			,controlGLPart=controlGLPart	
	)
	E0Smooth$RRef <- fillNAForward( resRef15$summary$RRef )	# may contain NA if not enough night-time records
	# special case NA on first record fill backward
	iFirstFinite <- min(which(is.finite(E0Smooth$RRef)))
	if(iFirstFinite > 1) E0Smooth$RRef[1:(iFirstFinite-1)] <- E0Smooth$RRef[iFirstFinite]
	#
	##seealso<< \code{\link{partGLFitLRCOneWindow}}
	if( isVerbose ) message("  Estimating light response curve parameters from day time NEE ", appendLF = FALSE)
	resLRC <- applyWindows(ds, partGLFitLRCOneWindow, prevRes<-list(lastGoodParameters=rep(NA_real_, 5L))
			, winSizeInDays=4L
			,isVerbose=isVerbose		
			,nRecInDay=nRecInDay
			#
			,E0Win = E0Smooth
			,controlGLPart=controlGLPart	
	)
	resParms <- resLRC
	resParms$summary$E_0 <- E0Smooth$E0
	resParms$summary$E_0_sd <- E0Smooth$sdE0
	resParms$summary$R_ref_night <- E0Smooth$RRef
	# summary$iMeanRec yet based on window instead of entire time, need to add beginning of window
	resParms$summary$iMeanRec <- resParms$summary$iRecStart-1L + resParms$summary$iMeanRec
	# omit records where NULL was returned
	iWinNoFit <- which( is.na(resParms$summary$parms_out_range) )	
	if( length(iWinNoFit) ){
		resParms$summary <- resParms$summary[-iWinNoFit, ]
		resParms$resOptList <- resParms$resOptList[-iWinNoFit] 
	}
	resParms
}

.tmp.f <- function(){
	plot( E0 ~ dayStart, E0Smooth)
}

fillNAForward <- function(
		### replace NA by value of previous record
		x		##<< numeric vector to fill NAs
		, firstValue=median(x,na.rm=FALSE)	##<< value to be used for NA at the beginning of x
){
	iMissing <- which(!is.finite(x))
	if( length(iMissing) && (iMissing[1] == 1L)){
		# set first vluae
		x[1L] <- firstValue
		iMissing <- iMissing[-1]
	}
	if(length(iMissing)){
		for( i in iMissing ){
			# set to value from previous window
			x[i] <- x[i-1L]	 
		}
	}
	return(x)
}

applyWindows <- function(
		### apply a function to several windows of a data.frame
		ds						##<< dataframe to iterate
		,FUN					##<< function to apply to subsets of the data.frame
		## taking a subset of the data.frame as first argument
		## the second: a one-row data.frame with window information (iWindow, dayStart, dayEnd, iRecStart, iRecEnd, iCentralRec)
		## the third a list that can transport values from previous fits, i.e. the third item of its return value 
		## with first item a list (objects that cannot be rowbound) 
		## second item a single row data.frame (summary that can be row-bound)
		## third item a list that is will be provided to the call of this function on the next window as the second argument
		,prevRes=list()						##<< initial values for the list that is carried between windows
		,winSizeInDays=winSizeRefInDays		##<< Window size in days   
		,winSizeRefInDays=4L				##<< Window size in days for reference window (e.g. day-Window for night time)
		,strideInDays=floor(winSizeRefInDays/2L)	##<< step in days for shifting the window, for alligning usually a factor of winSizeRef
		,isVerbose=TRUE			##<< set to FALSE to suppress messages
		,nRecInDay=48L			##<< number of records within one day (for half-hourly data its 48)
		,...	##<< further arguments to FUN
){
	##details<<
	## Assumes equidistant rows with nRecInDay records forming one day and reporting full days
	## i.e. all of the nRecInDay records are in the first day.
	##details<<
	## In order to have a common reference winSizeRefInDays is given so that results by a different window
	## size correspond to each window of shifting a window of winSizeRefInDays
	## Each window is anchord so that the center equals the center of the reference window.
	## This becomes important when selecting records at the edges of the series.
	nRec <- nrow(ds) 
	nDay <- ceiling( nRec / nRecInDay) 
	nDayLastWindow <- nDay - (winSizeRefInDays/2)			# center of the reference window still in records  
	#iDayOfRec <- ((c(1:nRec)-1L) %/% nRecInDay)+1L 		# specifying the day for each record assuming equidistand records
	startDaysRef <- seq(1, nDayLastWindow, strideInDays)	# starting days for each reference window
	iCentralRec <- ((startDaysRef-1L)+winSizeRefInDays/2)*nRecInDay+1L	# assuming equidistant records
	nWindow <- length(startDaysRef)
	# precomputing the starting and end records for all periods in vectorized way
	dayStart0 <- as.integer(startDaysRef + winSizeRefInDays/2 - winSizeInDays/2)	# may become negative, negative needed for computation of dayEnd 
	dayStart <- pmax(1L, dayStart0)
	dayEnd <- pmin(nDay, dayStart0-1L+winSizeInDays)
	iRecStart0 <- iCentralRec -winSizeInDays/2*nRecInDay	# may become negative, negative needed for computation of iRecEnd
	iRecStart <- pmax(1L, iRecStart0 )
	iRecEnd <- pmin(nRec, iCentralRec-1L +winSizeInDays/2*nRecInDay ) 
	# central record in each window, only last record, refers to reference window of size winSizeRefInDays 
	dsRec <- data.frame(
			iWindow=1:nWindow
			,dayStart = dayStart
			,dayEnd=dayEnd
			,iRecStart=iRecStart
			,iRecEnd=iRecEnd
			,iCentralRec=iCentralRec
	)
	res1 <- vector("list",nWindow)
	res2List <- vector("list",nWindow)	# each will hold a data.frame to be row-bound afterwards (dont know the columns yet)
	for( iWindow in 1:nWindow){
		if( isVerbose ) message(",",startDaysRef[iWindow], appendLF = FALSE)
		startRec <- iRecStart[iWindow]
		endRec <- iRecEnd[iWindow]
		# range( which(iDayOfRec >= startDay & iDayOfRec <= endDay))	# slower but check for startRec and endRec
		dsWin <- ds[startRec:endRec,]
		resFun <- FUN(dsWin, dsRec[iWindow,], prevRes, ...)
		if( length(resFun) ){
			res1[[iWindow]] <- resFun[[1]]
			res2List[[iWindow]] <- resFun[[2]]
			prevRes <- resFun[[3]]
		}
	}
	if( isVerbose ) message("") # LineFeed
	#tmp <- res2List
	#res2List <- tmp
	# rbind res2List, but keep NULL results, therefore add dummy dataframe, and delete dummy column afterward
	res2List[ sapply(res2List, is.null) ] <- list(data.frame(..DUMMY=1))
	res2 <- rbind.fill(res2List)	# maybe later upgrade to dplyr
	res2$..DUMMY <- NULL
	dsSummary <- cbind(dsRec, res2)
	##value<< a list with components that correspond to the three result components of FUN
	return(list(
					resOptList=res1			##<< a list with restults of length nrow(ds)
					,summary=dsSummary	##<< a data.frame with nrow(ds) rows, also including window information (iWindow, dayStart, dayEnd, iRecStart, iRecEnd, iCentralRec)
					, prevRes=prevRes)	##<< last result of the values carried over between windows
	)
}




partGLFitNightTempSensOneWindow=function(
		### Estimate parameters of the Rectangular Hyperbolic Light Response Curve function (a,b,R_ref, k) for successive periods
		dss					##<< data.frame with numeric columns NEE, sdNEE, Temp (degC), VPD, Rg, and logical columns isNight and isDay
		,winInfo			##<< one-row data.frame with window information, including iWindow 
		,prevRes			##<< component prevRes from previous result, here with item prevE0
		,isVerbose=TRUE		##<< set to FALSE to suppress messages
		,nRecInDay=48L	##<< number of records within one day (for half-hourly data its 48)
		,controlGLPart=partGLControl()	##<< list of further default parameters
){
	##author<<
	## TW
	##seealso<< \code{\link{partitionNEEGL}}
	##description<<
	## Estimation of respiration at reference temperature (R_Ref) and temperature (E_0) for one window.
	isValid <- isValidNightRecord(dss) 
	# check that there are enough night and enough day-values for fitting, else continue with next window
	if( sum(isValid) < controlGLPart$minNRecInDayWindow ) return(NULL)
	dssNight <- dss[isValid,]
	# tmp <- dss[!is.na(dss$isNight) & dss$isNight & !is.na(dss$NEE), ]; plot(NEE ~ Temp, tmp)
	# points(NEE ~ Temp, dssNight, col="blue" )
	##seealso<< \code{\link{partGLEstimateTempSensInBoundsE0Only}}
	#TODO here only E0 is of interest, do not need to reestimate RRef
	#resNightFitOld <- partGLEstimateTempSensInBounds(dssNight$NEE, fConvertCtoK(dssNight$Temp)
	#			, prevE0=prevRes$prevE0)
	resNightFit <- partGLEstimateTempSensInBoundsE0Only(dssNight$NEE, fConvertCtoK(dssNight$Temp)
		, prevE0=prevRes$prevE0)
	return(list(
					resNightFit
					,data.frame(E0=resNightFit$E0, sdE0=resNightFit$sdE0, TRefFit=resNightFit$TRefFit, RRefFit=resNightFit$RRefFit)
					,list(prevE0=if(is.finite(resNightFit$E0)) resNightFit$E0 else prevRes$prevE0) 
		))
}

.tmp.f <- function(){
	plot( dss$NEE ~ dss$sDateTime)
	plot( dssNight$NEE ~ dssNight$sDateTime)
}

isValidNightRecord <- function(
		### compute logical vector of each rows in ds is its a valid night record
		ds		##<< data.frame with columns isNight, NEE, Temp (degC)
){
	isValid <- !is.na(ds$isNight) & ds$isNight & !is.na(ds$NEE)
	##details<<
	## For robustness, data is trimmed to conditions at temperature > 1 degC 
	## but only timmed if there are more at least 12 records left
	isFreezing <- ds$Temp[isValid] <= -1
	if( sum(!isFreezing) >= 12L ) isValid[isValid][isFreezing] <- FALSE
	##value<< a logical vector of length nrow(ds)
	return(isValid)
}

partGLEstimateTempSensInBoundsE0Only <- function(
		### Estimate temperature sensitivity E_0 and R_ref of ecosystem respiration, and apply bounds or previous estimate
		REco					##<< numeric vector: night time NEE, i.e. ecosytem respiration
		,temperatureKelvin		##<< numeric vector: temperature in Kelvin of same length as REco
		,prevE0	= NA			##<< numeric scalar: the previous guess of Temperature Sensitivity
){
	##author<< MM, TW
	##seealso<< \code{\link{partGLFitLRCWindows}}
	#twutz: using nls to avoid additional package dependency
	#resFitLM <- NLS.L <- nlsLM(formula=R_eco ~ fLloydTaylor(R_ref, E_0, Temp, T_ref.n=273.15+15), algorithm='default', trace=FALSE,
	#		data=as.data.frame(cbind(R_eco=REco.V.n,Temp=temperatureKelvin.V.n)), start=list(R_ref=mean(REco.V.n,na.rm=TRUE),E_0=100)
	#		,control=nls.lm.control(maxiter = 20))
	##details<< 
	## Basal respiration is reported for temperature of 15 degree Celsius. However during the fit
	## a reference temperature of the median of the dataset is used. This is done to avoid
	## strong correlations between estimated parameters E0 and R_ref, that occure if reference temperature 
	## is outside the center of the data.
	TRefFit <- median(temperatureKelvin, na.rm=TRUE)	# formerly 273.15+15
	resFit <- try(
			nls(formula=R_eco ~ fLloydTaylor(R_ref, E_0, Temp, T_ref.n=TRefFit), algorithm='default', trace=FALSE,
					data=as.data.frame(cbind(R_eco=REco,Temp=temperatureKelvin))
					, start=list(R_ref= mean(REco,na.rm=TRUE)
							,E_0=as.vector({if(is.finite(prevE0)) prevE0 else 100}))
					,control=nls.control(maxiter = 20L)
			)
			, silent=TRUE)
	#plot( REco.V.n ~ I(temperatureKelvin.V.n-273.15) )
	#plot( REcoCorrected ~ I(temperatureKelvin.V.n-273.15)[isNotFreezing] )
	if( inherits(resFit, "try-error")){
		#stop("debug partGLEstimateTempSensInBounds")
		#plot( REco.V.n	~ temperatureKelvin.V.n )
		E0 <- NA
		sdE0 <- NA
		RRefFit <- NA 
	} else {
		E0 <- coef(resFit)['E_0']
		sdE0 <- coef(summary(resFit))['E_0',2]
		RRefFit <- coef(resFit)['R_ref']
	}
	# resFit$convInfo$isConv
	##details<<
	## If E_0 is out of bounds [50,400] then report E0 as NA 
	if( is.na(E0) || (E0 < 50) || (E0 > 400)){
		E0 <- NA
		sdE0 <- NA
		RRefFit <- NA 
	} 
	##value<< list with entries
	return(list(
					E0=E0				##<< numeric scalar of estimated temperature sensitivty E0 bounded to [50,400]
					,sdE0=sdE0			##<< numeric scalar of standard deviation of E0
					,TRefFit=TRefFit	##<< numeric scalar reference temperature used in the E0 fit
					,RRefFit=RRefFit	##<< numeric scalar respiration at TRefFit
	))
	#
	# refit R_Ref with bounded E0 for 15 degC, instead of calling fLoydAndTaylor do a simple regression 
#   starting value from forward model
#	RRef15 <- fLloydTaylor( RRefFit, E_0Bounded.V.n, TRef15, T_ref.n=TRefFit)
#	resFit15 <-	nls(formula=R_eco ~ fLloydTaylor(R_ref, E_0, Temp, T_ref.n=TRef15), algorithm='default', trace=FALSE,
#					data=as.data.frame(cbind(R_eco=REcoFitting,Temp=temperatureKelvin.V.n[isNotFreezing], E_0=E_0Bounded.V.n))
#					, start=list(R_ref=RRef15)
#					,control=nls.control(maxiter = 20L)
#			)
#	(R_ref_ <- coef(resFit15)[1])
}

.tmp.f <- function(){
	# from recover at fitting E0
	temp <- temperatureKelvin - 273.15
	plot( REco ~ temp )
	lines( fLloydTaylor(RRefFit, E0, temperatureKelvin, T_ref.n=TRefFit) ~ temp)
}



partGLSmoothTempSens <- function(
		### Smoothes time development of E0
		E0Win				##<< data.frame with columns E0 and sdE0, RRefFit, and TRefFit with one row for each window
){
	#return(E0Win)
	#E0Win$E0[1] <- NA
	E0Win$E0[c(FALSE,(diff(E0Win$E0) == 0))] <- NA	# TODO return NA in the first place where the previous window was used
	isFiniteE0 <- is.finite(E0Win$E0)
	E0WinFinite <- E0Win[ isFiniteE0, ]
	output <- capture.output(
			gpFit <- mlegp(X=E0WinFinite$iCentralRec, Z=E0WinFinite$E0, nugget = E0WinFinite$sdE0^2)
			#gpFit <- mlegp(X=E0WinFinite$iCentralRec, Z=E0WinFinite$E0, nugget = (E0WinFinite$sdE0*2)^2, nugget.known=1L)
	)
	pred1 <- predict(gpFit, matrix(E0Win$iCentralRec, ncol=1), se.fit=TRUE)
	nuggetNewObs <- quantile(gpFit$nugget, 0.9)
	nugget <- rep(nuggetNewObs, nrow(E0Win))
	nugget[isFiniteE0] <- gpFit$nugget
	E0Win$E0 <- pred1$fit
	E0Win$sdE0 <- pred1$se.fit + sqrt(nugget) 
	##value<< dataframe E0Win with updated columns E0 and sdE0
#recover()	
	return(E0Win)
}
.tmp.f <- function(){
	plot( E0WinFinite$E0 ~ E0WinFinite$iCentralRec )
	#arrows( E0WinFinite$iCentralRec, E0WinFinite$E0-1.96*E0WinFinite$sdE0, y1=E0WinFinite$E0+1.96*E0WinFinite$sdE0, length=0, col="grey" )
	arrows( E0WinFinite$iCentralRec, E0WinFinite$E0-E0WinFinite$sdE0, y1=E0WinFinite$E0+E0WinFinite$sdE0, length=0, col="grey" )
	#
	E0Win$day <- (E0Win$iCentralRec-1) / 48 +1
	E0WinFinite$day <- (E0WinFinite$iCentralRec-1) / 48 +1
	plot( E0WinFinite$E0 ~ E0WinFinite$day )
	plot( E0Win$E0Fit ~ E0Win$day )
	points( E0Win$E0 ~ E0Win$day, col="blue", type="b", lty="dotted" )
	#points( E0Win$E0 ~ E0Win$day, col="blue" )
	#arrows( E0Win$day, E0Win$E0Fit, y1=E0Win$E0, col="grey", length=0.1)
	lines( I(E0Win$E0 + 1.06*E0Win$sdE0) ~ E0Win$day, col="lightblue" )
	lines( I(E0Win$E0 - 1.06*E0Win$sdE0) ~ E0Win$day, col="lightblue" )
	#
	E0Win$E0 <- E0Win$E0Fit
	E0Win$sdE0 <- E0Win$sdE0Fit
	E0Win$E0[c(FALSE,(diff(E0Win$E0) == 0))] <- NA	# TODO return NA in the first place
	#
	E0Win$isFiniteE0 <- isFiniteE0
	E0Win[ E0Win$E0 < 80,]
	E0Win[ 65:75,]
	subset(E0WinFinite, iWindow %in% 65:75)
	
 	E0Win$sdE0 / E0Win$E0 
}

partGLFitNightRespRefOneWindow=function(
		### Estimate Reference temperature from nicht-time and given temperature sensitivity E0  
		dss					##<< data.frame with numeric columns NEE, isNight, Temp, Rg
		,winInfo			##<< one-row data.frame with window information, including iWindow 
		,prevRes=list()		##<< component prevRes from previous result, here not used.
		,E0Win				##<< data.frame with columns E0 and sdE0, RRefFit, and TRefFit with one row for each window
		,isVerbose=TRUE		##<< set to FALSE to suppress messages
		,nRecInDay=48L	##<< number of records within one day (for half-hourly data its 48)
		,controlGLPart=partGLControl()	##<< list of further default parameters
){
	##author<<
	## TW
	##seealso<< \code{\link{partitionNEEGL}}
	##description<<
	## Estimation of respiration at reference temperature (R_Ref) and temperature (E_0) for one window.
	isValid <- isValidNightRecord(dss) 
	# check that there are enough night and enough day-values for fitting, else continue with next window
	if( sum(isValid) < controlGLPart$minNRecInDayWindow ) return(NULL)
	dssNight <- dss[isValid,]
	# tmp <- dss[!is.na(dss$isNight) & dss$isNight & !is.na(dss$NEE), ]; plot(NEE ~ Temp, tmp)
	# points(NEE ~ Temp, dssNight, col="blue" )
	##seealso<< \code{\link{partGLEstimateTempSensInBoundsE0Only}}
	REco <- dssNight$NEE
	E0 <- E0Win$E0[winInfo$iWindow]
	TRef15 <- 273.15+15	# 15degC in Kelvin
	R_ref <- if( length(REco) >= 3L ){
				temperatureKelvin <- 273.15+dssNight$Temp
				T_0.n=227.13         ##<< Regression temperature as fitted by LloydTaylor (1994) in Kelvin (degK)
				TFacLloydTaylor <-  exp(E0 * ( 1/(TRef15-T_0.n) - 1/(temperatureKelvin-T_0.n) ) )
				lm15 <- lm(REco ~ TFacLloydTaylor -1)
				coef(lm15)
			} else 
				fLloydTaylor( E0Win$RRefFit[winInfo$iWindow], E0, TRef15, T_ref.n=E0Win$TRefFit[winInfo$iWindow])
	R_refBounded <- max(0, R_ref)
	##value<< list with entries
	return(list(
					NULL
					,data.frame(RRef=R_refBounded)
					,prevRes
			))
}

partGLFitLRCOneWindow=function(
		### Estimate parameters of the Rectangular Hyperbolic Light Response Curve function (a,b,R_ref, k) for successive periods
		ds					##<< data.frame with numeric columns NEE, sdNEE, Temp (degC), VPD, Rg, and logical columns isNight and isDay
		,winInfo			##<< one-row data.frame with window information, including iWindow 
		,prevRes			##<< component prevRes from previous result, here with item prevE0
		,E0Win				##<< data.frame with columns E0, sdE0, RRef from nighttime, one row for each window
		,controlGLPart=partGLControl()	##<< list of further default parameters
){
	##author<< TW
	##seealso<< \code{\link{partitionNEEGL}}
	##description<<
	## Estimation as in Lasslop et al., 2010 for successive periods, i.e. windows.
	#requiredCols <- c("NEE", "sdNEE", "Temp", "VPD", "Rg", "isNight", "isDay")
	#iMissing <- which( is.na(match( requiredCols, names(ds) )))
	#if( length(iMissing) ) stop("missing columns: ",paste0(requiredCols[iMissing],collapse=","))
	isValidDayRec <- !is.na(ds$isDay) & ds$isDay & !is.na(ds$NEE) & !is.na(ds$Temp) & !is.na(ds$VPD) 
	# check that there are enough night and enough day-values for fitting, else continue with next window
	if( sum(isValidDayRec) < controlGLPart$minNRecInDayWindow ) return( NULL );
	dsDay <- ds[isValidDayRec,]
	##details<<
	## Each window estimate is associated with a time or equivalently with a record.
	## The first record, i.e. row number, of the day-window is reported.
	## Moreover, the mean of all valid records numbers in the daytime window is reported for interpolation.
	iMeanRecInDayWindow <- as.integer(round(mean(which(isValidDayRec)))) 
	#TODO firstRecInDayWindow.i <- which(SubsetDayPeriod.b)[1] # the rownumber of the first record inside the day window
	##seealso<< \code{\link{partGLEstimateTempSensInBoundsE0Only}}
	E0 <- E0Win$E0[winInfo$iWindow]
	# if no temperature - respiration relationship could be found, indicate no-fit
	if( is.na(E0) ) return(NULL)    
# if( DayStart.i > 72 ) recover()		
	sdE0 <- E0Win$sdE0[winInfo$iWindow]
	RRefNight <- E0Win$RRef[winInfo$iWindow]
	#
	##seealso<< \code{\link{partGLFitLRC}}
	resOpt <- resOpt0 <- partGLFitLRC(dsDay, E0=E0, sdE0=sdE0, RRefNight=RRefNight
			, controlGLPart=controlGLPart, lastGoodParameters=prevRes$lastGoodParameters)
	#
	if( !is.finite(resOpt$opt.parms.V[1]) ) return(NULL)
	# check the Beta bounds that depend on uncertainty, set to NA fit
	sdParms <- resOpt$opt.parms.V; sdParms[] <- NA
	sdParms[resOpt$iOpt] <- sqrt(diag(resOpt$covParms)[resOpt$iOpt])
	if(isTRUE(as.vector( (resOpt$opt.parms.V[2] > 100) && (sdParms[2] >= resOpt$opt.parms.V[2]) ))){
		return(NULL)
	}
	# check that R_ref estimated from daytime is not both:
	# larger than twice the estimate from nighttime and more than 0.7 in absolute terms  
	# else this indicates a bad fit
	# this is additional to Table A1 in Lasslop 2010
	if( (resOpt$opt.parms.V[4L] > 2*RRefNight) 		&& 
			((resOpt$opt.parms.V[4L]-RRefNight) > 0.7) 
			){
		return(NULL)
	}
	#
	#recover()			
	prevRes$lastGoodParameters <- resOpt$opt.parms.V
	# record valid fits results
	ans <- list(
		 resOpt=resOpt
		 ,summary = data.frame(
			nValidRec=nrow(dsDay)
			,iMeanRec=iMeanRecInDayWindow
			,E_0=E0, E_0_sd=sdE0
			,R_ref=resOpt$opt.parms.V[4], R_ref_SD=sdParms[4]
			,a=resOpt$opt.parms.V[3], a_SD=sdParms[3]
			,b=resOpt$opt.parms.V[2], b_SD=sdParms[2]
			,k=resOpt$opt.parms.V[1], k_SD=sdParms[1]
			,parms_out_range=as.integer(!identical(resOpt$iOpt,1:5))
		)
		,prevRes=prevRes
	)
	return(ans)
}





partGLFitLRC <- function(
		### Optimize rectangular hyperbolic light response curve against data in one window and estimate uncertainty
		dsDay				##<< data.frame with columns NEE, Rg, Temp_C, VPD, and no NAs in NEE
		, E0				##<< temperature sensitivity of respiration
		, sdE0				##<< standard deviation of E_0.n
		, RRefNight			##<< basal respiration estimated from night time data 
		, controlGLPart=partGLControl()	##<< further default parameters (see \code{\link{partGLControl}})
		, lastGoodParameters			##<< numeric vector of last good theta
){
	##author<< TW, MM
	##seealso<< \code{\link{partGLFitLRCWindows}}
	#	
	#Definition of initial guess theta, theta2 and theta3. Three initial guess vectors are defined according to Lasslop et al., 2010
	namesPars <- c("k","beta0", "alfa", "Rb","E0" )
	nPar <- length(namesPars)
	##details<<
	## Optimization is performed for three initial parameter sets that differ by beta0 (*1.3, *0.8).
	## From those three, the optimization result is selected that yielded the lowest misfit.
	## Starting values are: k=0, beta=interpercentileRange(0.03,0.97) of respiration, alpha=0.1, R_ref 
	## from nightTime estimate.
	## E0 is fixed to the night-time estimate, but varied for estimating parameter uncertainty.
	theta.V.n<-matrix(NA, 3,nPar, dimnames=list(NULL,namesPars))
	parameterPrior <- theta.V.n[1,] <- theta.V.n[2,] <- theta.V.n[3,] <- c(
			k=0
			#,beta=as.vector(abs(quantile(dsDay$NEE, 0.03)-quantile(dsDay$NEE, 0.97)))
			,beta=as.vector(abs(quantile(dsDay$NEE, 0.03, na.rm=TRUE)-quantile(dsDay$NEE, 0.97, na.rm=TRUE)))
			,alpha=0.1
			#,R_ref=mean(NEENight.V.n, na.rm=T)
			,R_ref=if( is.finite(RRefNight) ) as.vector(RRefNight) else stop("must provide finite RRefNight") #mean(NEENight.V.n, na.rm=T)
			,E_0=as.vector(E0)
	)   #theta [numeric] -> parameter vector (theta[1]=kVPD, theta[2]-beta0, theta[3]=alfa, theta[4]=Rref)
	#twutz: beta is quite well defined, so try not changing it too much
	theta.V.n[2,2] <- parameterPrior[2]*1.3
	theta.V.n[3,2] <- parameterPrior[2]*0.8
	#
	##seealso<< \code{\link{parmGLOptimLRCBounds}}
	resOpt3 <- apply( theta.V.n, 1, function(theta0){
				resOpt <- parmGLOptimLRCBounds(theta0, parameterPrior, dsDay=dsDay, ctrl=controlGLPart, lastGoodParameters.V.n=lastGoodParameters )
		})
	iValid <- which(sapply(resOpt3,function(resOpt){ is.finite(resOpt$theta[1]) }))
	resOpt3Valid <- resOpt3[iValid]
	optSSE <- sapply(resOpt3Valid, "[[", "value")
	if( sum(!is.na(optSSE)) == 0L ){
		# none of the intial fits yielded valid result, create NA-return values
		opt.parms.V <- structure( rep(NA_real_, nPar), names=colnames(theta.V.n) )
		covParms <- matrix(NA_real_, nPar, nPar, dimnames=list(colnames(theta.V.n),colnames(theta.V.n)))
		resOpt <- list(iOpt = integer(0))	
	} else {
		resOpt <- resOpt3Valid[[iBest <- which.min(optSSE)]] # select the one with the least cost
		opt.parms.V<-resOpt$theta
		if(controlGLPart$nBootUncertainty == 0L) {
			##details<< If \code{controlGLPart.l$nBootUncertainty == 0L} then the covariance matrix of the 
			## parameters is estimated by the Hessian of the LRC curve at optimum.
			## Then, the additional uncertainty and covariance with uncertaint E0 is neglected.
			#seParmsHess <- seParmsHess0 <- sqrt(abs(diag(solve(resOpt$hessian))))
			covParmsLRC <- if( (resOpt$hessian[1L,1L] < 1e-8) ){
				# case where k=0 and not varying: cov(k,:) = cov(:,) = 0
				covParmsLRC <- structure( diag(0,nrow=nrow(resOpt$hessian)), dimnames=dimnames(resOpt$hessian))
				covParmsLRC[-1L,-1L] <- solve(resOpt$hessian[-1L,-1L])
				covParmsLRC
			} else {
				solve(resOpt$hessian)
			}
			covParms <- structure( diag(0,nrow=length(resOpt$theta)), dimnames=list(namesPars,namesPars))
			covParms[5L,5L] <- sdE0^2
			covParms[resOpt$iOpt,resOpt$iOpt] <- covParmsLRC  
			if( any(diag(covParms) < 0)) opt.parms.V[] <- NA	# no real if covariance negative
		} else {
			##details<< 
			## If \code{controlGLPart.l$nBootUncertainty > 0L} then the covariance matrix of the 
			## parameters is estimated by a bootstrap of the data.
			## In each draw, E0 is drawn from N ~ (E_0, sdE_0).
			##seealso<< \code{\link{.bootStrapLRCFit}}
			resBoot  <- .bootStrapLRCFit(resOpt$theta, resOpt$iOpt, dsDay, sdE0, parameterPrior, controlGLPart)
			#resBoot  <- .bootStrapLRCFit(resOpt$theta, resOpt$iOpt, dsDay, sdE_0.n, parameterPrior, controlGLPart.l=within(controlGLPart.l,nBootUncertainty <- 30L))
			covParms <- cov(resBoot)
			#better not to average parameters
			#opt.parms.V <- apply(resBoot, 2, median, na.rm=TRUE)
			#se.parms.V <- apply(resBoot, 2, sd, na.rm=TRUE)
		}
	}
	##value<< a list, If none of the optimizations from different starting conditions converged,
	## the parameters are NA
	ans <- list(
			opt.parms.V=opt.parms.V		##<< numeric vector of optimized parameters including 
				## the fixed ones and including E0
			,iOpt=c(resOpt$iOpt,5L)		##<< index of parameters that have been optimized
				## , here including E0, which has been optimized prior to this function.
			,initialGuess.parms.V.n=theta.V.n[1,]	##<< the initial guess from data
			,covParms=covParms			##<< numeric matrix of the covariance matrix of parameters, including E0
	)
}

.tmp.f <- function(){
	plot(dsDay$Rg,-1*dsDay$NEE)
	thetaB <- resOpt$theta
	#thetaB <- opt.parms.V
	#thetaB <-  opt[[iBest <- which.min(optSSE)]]
	#
	#thetaB <- resOpt$par
	#thetaB <- c(resOpt3[[1]]$par, E_0=as.vector(E_0.n))
	#thetaB <- theta
	#thetaB <- thetaOrig
	points(
			#lines(
			dsDay$Rg, tmp <- partGL_RHLightResponse(thetaB
					,Rg = dsDay$Rg 
					,Temp=dsDay$Temp
					,VPD = dsDay$VPD
					#,E0 = E_0.n
					,fixVPD = FALSE
			)$NEP, col="red")
	points(dsDay$Rg, tmp <- partGL_RHLightResponse(thetaB
					,Rg = dsDay$Rg 
					,Temp=dsDay$Temp
					,VPD = dsDay$VPD
					,E0 = E_0.n
					,fixVPD = FALSE
			)$Reco, col="green")
	#)$GPP, col="blue")
}


parmGLOptimLRCBounds <- function(
		### Optimize parameters of light response curve and refit with some fixed parameters if fitted parameters are outside bounds
		theta0		##<< initial parameter estimate
		,parameterPrior	##<< prior estimate of model parameters
		, ...		##<< further parameters to \code{.optimLRC}, such as \code{dsDay}, and \code{ctrl} 
		,lastGoodParameters.V.n ##<< parameters vector of last successful fit
){
	##author<< TW, MM
	##seealso<< \code{\link{partGLFitLRC}}
	if( !is.finite(lastGoodParameters.V.n[3L]) ) lastGoodParameters.V.n[3L] <- 0.22	# twutz 161014: default alpha 	
	isAlphaFix <- FALSE
	isKFix <- FALSE
	resOpt <- resOpt0 <- .optimLRC(theta0, isUsingFixedVPD=isKFix, isUsingFixedAlpha=isAlphaFix, parameterPrior = parameterPrior, ... )
	# positions in theta0: "k"     "beta0" "alfa"  "Rb"    "E0"
	# IF kVPD parameter less or equal zero then estimate the parameters withouth VPD effect
	##details<<
	## If parameters alpha or k are outside bounds (Table A1 in Lasslop 2010), refit with some parameters fixed 
	## to values from fit of previous window.
	theta0Adj <- theta0	# intial parameter estimate with some parameters adjusted to bounds
	#dsDay <- list(...)$dsDay
	if (resOpt$theta[1L] < 0){
		isKFix <- TRUE
		theta0Adj[1L] <- 0
		resOpt <- .optimLRC(theta0Adj, isUsingFixedVPD=isKFix, isUsingFixedAlpha=isAlphaFix, parameterPrior = parameterPrior, ... )
		# check alpha, if less than zero estimate parameters with fixed alpha of last window 
		if ( (resOpt$theta[3L] > 0.22) && is.finite(lastGoodParameters.V.n[3L]) ){
			isAlphaFix <- TRUE
			theta0Adj[3L] <- lastGoodParameters.V.n[3L] 
			resOpt <- .optimLRC(theta0Adj, isUsingFixedVPD=isKFix, isUsingFixedAlpha=isAlphaFix, parameterPrior = parameterPrior, ... )
		}
	} else {
		# check alpha, if gt 0.22 estimate parameters with fixed alpha of last window
		# if not last window exists, let alpha > 0.22
		if ( (resOpt$theta[3L] > 0.22) && is.finite(lastGoodParameters.V.n[3L]) ){
			isAlphaFix <- TRUE
			theta0Adj[3L] <- lastGoodParameters.V.n[3L]
			resOpt <- .optimLRC(theta0Adj, isUsingFixedVPD=isKFix, isUsingFixedAlpha=isAlphaFix, parameterPrior = parameterPrior, ... )
			# check k, if less than zero estimate parameters without VPD effect and with fixed alpha of last window 
			if (resOpt$theta[1L] < 0){
				isKFix <- TRUE
				theta0Adj[1L] <- 0
				resOpt <- .optimLRC(theta0Adj, isUsingFixedVPD=isKFix, isUsingFixedAlpha=isAlphaFix, parameterPrior = parameterPrior, ... )
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


.bootStrapLRCFit <- function(
		### Compute parameters uncertainty by bootstrap
		theta0, iOpt, dsDay, sdE_0.n, parameterPrior, controlGLPart.l
){
	##value<<
	## matrix with each row a parameter estimate on a different bootstrap sample
	ans <-matrix(NA, nrow=controlGLPart.l$nBootUncertainty, ncol=length(theta0), dimnames=list(NULL,names(theta0)))
	isUsingFixedVPD <- !(1L %in% iOpt)
	isUsingFixedAlpha <- !(3L %in% iOpt)
	##details<<
	## In addition to resampling the original data, also the temperature sensitivity is resampled 
	## from its uncertainty distribution.
	E0r <- rnorm( controlGLPart.l$nBootUncertainty, theta0[5L], sdE_0.n	)
	E0 <- pmax(50,pmin(400,E0r))
	theta <- theta0
	#iBoot <- 1L
	for (iBoot in c(1:controlGLPart.l$nBootUncertainty)){
		idx <- sample(nrow(dsDay), replace=TRUE)
		dsDayB <- dsDay[idx,]
		theta[5L] <- E0[iBoot]
		resOptBoot <- .optimLRC(theta, isUsingFixedVPD=isUsingFixedVPD, isUsingFixedAlpha=isUsingFixedAlpha 
				, dsDayB, parameterPrior, controlGLPart.l )
		if( resOptBoot$convergence == 0L )	
			#TODO: also remove the bery bad cases? 
			ans[iBoot,]<-resOptBoot$theta
	}
	ans
}

.optimLRC <- function(
		###<< one fit of the light response curve with a subset of parameters depending on which are fixed
		theta					##<< numeric vector of starting values
		, isUsingFixedVPD=FALSE, isUsingFixedAlpha=FALSE	##<< selection of which parameters to optimize
		, dsDay					##<< dataframe of NEE, sdNEE and predictors Rg, VPD and Temp
		#, E_0.n					##<< temperature sensitivity
		, parameterPrior		##<< prior parameter estimates
		, ctrl					##<< list of further controls
){
	if( !all(is.finite(theta))) stop("need to provide finite starting values.")
	##details<<
	## Only those records are used for optimization where both NEE and sdNEE are finite.
	dsDayFinite <- dsDay[ is.finite(dsDay$NEE) & is.finite(dsDay$sdNEE), ]
	##details<<
	## Optimization of Light-response curve parameters takes into account the uncertainty of the flux values.
	## In order to avoid very strong leverage, values with a very low uncertainty (< median) are assigned
	## the median of the uncertainty.
	## This procedure downweighs records with a high uncertainty, but does not apply a large leverage for
	## records with a very low uncertainty. Avoid this correction by suppyling setting \code{ctrl$isBoundLowerNEEUncertainty = FALSE}
	Fc_unc <- if( isTRUE(ctrl$isBoundLowerNEEUncertainty) ){
		pmax( dsDayFinite$sdNEE, quantile(dsDayFinite$sdNEE, 0.3) ) #twutz: avoid excessive weights by small uncertainties (of 1/unc^2)
	} else {
		dsDayFinite$sdNEE
	}
	#plot( Fc_unc ~ dsDayFinite$sdNEE)
	##details<< 
	## The beta parameter is quite well defined. Hence use a prior with a standard deviation.
	## The specific results are sometimes a bit sensitive to the uncertainty of the beta prior. 
	## This uncertainty is set corresponding to 10 times the median relative flux uncertainty.
	## The prior is weighted n times the observations in the cost.
	## Hence, overall it is using a weight of 1/10 of the weight of all observations.
	sdParameterPrior <- if(ctrl$isLasslopPriorsApplied){
		c(k=50, beta=600, alpha=10, Rb=80, E0=NA)
	} else {
		medianRelFluxUncertainty <- abs(median(Fc_unc/dsDayFinite$NEE))
		sdBetaPrior <- 10*medianRelFluxUncertainty*parameterPrior[2]/sqrt(nrow(dsDayFinite))
		c(k=NA, beta=as.vector(sdBetaPrior), alpha=NA, Rb=NA, E0=NA)
	}
	isUsingHessian <- (ctrl$nBootUncertainty==0L)
	iOpt <- 
			if( !isUsingFixedVPD & !isUsingFixedAlpha ) 1:4 else
			if(  isUsingFixedVPD & !isUsingFixedAlpha ) 2:4 else
			if( !isUsingFixedVPD &  isUsingFixedAlpha ) c(1L,2L,4L) else
			if(  isUsingFixedVPD &  isUsingFixedAlpha ) c(2L,4L) 
	sdParameterPrior[-iOpt] <- NA
	# encountered the case where fit ran into a local optimum at very low (even negative) beta
	# hence first do a fit with log(beta) or a fit with very strong prior on beta and alpha
	# and relax priors afterwards.
#	thetaLog <- theta; thetaLog[2] <- log(theta[2])
#	parameterPriorLog <- parameterPrior; parameterPriorLog[2] <- log(parameterPrior[2])
#	# for the log-Fit assume a strong prior on beta to avoid local minimum at beta=0
#	sdParameterPriorLog <- sdParameterPrior  #; sdParameterPriorLog[2] <- log(parameterPrior[2]) - log(parameterPrior[2] - sdParameterPrior[2])
#	resOptimLog <- optim(thetaLog[iOpt], .partGLRHLightResponseCostLogBeta
#			#tmp <- .partGLRHLightResponseCost( theta[iOpt], 
#			,theta=thetaLog
#			,iOpt=iOpt
#			,flux = -dsDayFinite$NEE 
#			,sdFlux = Fc_unc	  
#			,parameterPrior = parameterPriorLog
#			,sdParameterPrior = sdParameterPriorLog
#			,Rg = dsDayFinite$Rg 
#			,VPD = dsDayFinite$VPD
#			,Temp=dsDayFinite$Temp
#			#,E0 = E_0.n
#			,control=list(reltol=ctrl$LRCFitConvergenceTolerance)
#			,method="BFGS", hessian=isUsingHessian)
#	thetaOrig <- theta; thetaOrig[iOpt] <- resOptimLog$par; thetaOrig[2] <- exp(thetaOrig[2])	
	#
	thetaOrig <- theta
	sdStrongPrior <- sdParameterPrior; sdStrongPrior[2] <- sdParameterPrior[2]/10; sdStrongPrior[3] <- 0.5
	resOptimStrongPrior <- optim(thetaOrig[iOpt], .partGLRHLightResponseCost
		#tmp <- .partGLRHLightResponseCost( theta[iOpt], 
		,theta=thetaOrig
		,iOpt=iOpt
		,flux = -dsDayFinite$NEE 
		,sdFlux = Fc_unc	  
		,parameterPrior = parameterPrior
		,sdParameterPrior = sdStrongPrior
		,Rg = dsDayFinite$Rg 
		,VPD = dsDayFinite$VPD
		,Temp=dsDayFinite$Temp
		#,E0 = E_0.n
		,control=list(reltol=ctrl$LRCFitConvergenceTolerance)
		,method="BFGS", hessian=isUsingHessian)
	thetaOrig[iOpt] <- resOptimStrongPrior$par	
	#
	resOptim <- optim(thetaOrig[iOpt], .partGLRHLightResponseCost
			#resOptim <- optim(theta, .partGLRHLightResponseCost
			#tmp <- .partGLRHLightResponseCost( theta[iOpt] 
				,theta=thetaOrig
				,iOpt=iOpt
				,flux = -dsDayFinite$NEE 
				,sdFlux = Fc_unc	  
				,parameterPrior = parameterPrior
				,sdParameterPrior = sdParameterPrior
				,Rg = dsDayFinite$Rg 
				,VPD = dsDayFinite$VPD
				,Temp=dsDayFinite$Temp
				#,E0 = E_0.n
				,control=list(reltol=ctrl$LRCFitConvergenceTolerance)
				,method="BFGS", hessian=isUsingHessian)
	##value<<
	## list of restult of \code{\link{optim}} amended with list
	thetaOpt <- theta; thetaOpt[iOpt] <- resOptim$par
	ans <- list(
			theta = thetaOpt	##<< numeric vector: optimized parameter vector including the fixed components
			,iOpt = iOpt		##<< integer vector: position of parameters that have been optimized
			)
	c(resOptim, ans)
}

.tmp.f <- function(){
	tmp <- .partGLRHLightResponseCost(theta 
	,flux = -dsDay$NEE 
	,sdFlux = Fc_unc	  
	,betaPrior = betaPrior
	,sdBetaPrior = sdBetaPrior
	,Rg = dsDay$Rg 
	,VPD = dsDay$VPD
	,Temp=dsDay$Temp
	,E_0.n = E_0.n
	)
}



partGL_RHLightResponse <- function(
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
	## with fixes E_0.n for the estimation of Rd
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


partGL_RHLightResponseGrad <- function(
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

.partGLRHLightResponseCost <- function(
		### Computing residual sum of sqares for predictions vs. data of NEE
		thetaOpt 	##<< parameter vecotr with components of theta0 that are optimized 
		,theta		##<< parameter vector with positions as in argument of \code{\link{partGL_RHLightResponse}} 
		,iOpt		##<< position in theta0 that are optimized 
		,flux=NA 	##<< numeric: NEP (-NEE) or GPP time series [umolCO2/m2/s], should not contain NA
		,sdFlux=NA 	##<< numeric: standard deviation of Flux [umolCO2/m2/s], should not contain NA
		,parameterPrior		##<< numeric vector along theta: prior estimate of parameter (range of values)
		,sdParameterPrior	##<< standard deviation of parameterPrior
		,...			##<< other arguments to \code{\link{partGL_RHLightResponse}}
		,VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010
		,fixVPD = FALSE   	##<< fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered
		,useCVersion=TRUE	##<< set to FALSE  to use R instead of fast C version, e.g. for debugging
		# also tried to pass dsDay data.frame instead of all the variables separately, but accessing data.frame
		# columns in the cost function was a severe penalty (here double execution time)
) {
	theta[iOpt] <- thetaOpt
	#if( FALSE ){
	if( useCVersion){
		# generated in RcppExports: RHLightResponseCostC <- function(theta, flux, sdFlux, parameterPrior, sdParameterPrior, Rg, VPD, Temp, VPD0, fixVPD) {
		# when generating add description 
		### Compute the cost of RLightResponse prediction versus observations by using fast C-code
		RHLightResponseCostC(theta, flux, sdFlux, parameterPrior, sdParameterPrior, ..., VPD0=VPD0, fixVPD=fixVPD)		
	} else {
		resPred <- partGL_RHLightResponse(theta, ..., VPD0=VPD0, fixVPD=fixVPD)
		NEP_mod <- resPred$NEP
		misFitPrior <- (((theta - parameterPrior))/(sdParameterPrior))^2
		misFitObs <- sum(((NEP_mod-flux)/sdFlux)^2)
		RSS <- misFitObs + sum(misFitPrior, na.rm=TRUE)
		#if( !is.finite(RSS) ) recover()	# debugging the fit
		RSS
	}
}

.partGLRHLightResponseCostLogBeta <- function(
		### Computing residual sum of sqares for predictions vs. data of NEE, with parameter beta given at log-Scale
		thetaOpt 	##<< parameter vecotr with components of theta0 that are optimized 
		,theta		##<< parameter vector with positions as in argument of \code{\link{partGL_RHLightResponse}} 
		,iOpt		##<< position in theta0 that are optimized 
		,flux=NA 	##<< numeric: NEP (-NEE) or GPP time series [umolCO2/m2/s], should not contain NA
		,sdFlux=NA 	##<< numeric: standard deviation of Flux [umolCO2/m2/s], should not contain NA
		,parameterPrior		##<< numeric vector along theta: prior estimate of parameter (range of values)
		,sdParameterPrior	##<< standard deviation of parameterPrior
		,...			##<< other arguments to \code{\link{partGL_RHLightResponse}}
		,VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010
		,fixVPD = FALSE   	##<< fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered
		,useCVersion=TRUE	##<< set to FALSE  to use R instead of fast C version, e.g. for debugging
# also tried to pass dsDay data.frame instead of all the variables separately, but accessing data.frame
# columns in the cost function was a severe penalty (here double execution time)
) {
	thetaOrig <- theta
	thetaOrig[iOpt] <- thetaOpt
	thetaOrig[2] <- exp(thetaOrig[2])
	#if( FALSE ){
	if( useCVersion){
		RHLightResponseCostC(thetaOrig, flux, sdFlux, parameterPrior, sdParameterPrior, ..., VPD0=VPD0, fixVPD=fixVPD)		
	} else {
		resPred <- partGL_RHLightResponse(thetaOrig, ..., VPD0=VPD0, fixVPD=fixVPD)
		NEP_mod <- resPred$NEP
		misFitPrior <- (((thetaOrig - parameterPrior))/(sdParameterPrior))^2
		misFitObs <- sum(((NEP_mod-flux)/sdFlux)^2)
		RSS <- misFitObs + sum(misFitPrior, na.rm=TRUE)
		#if( !is.finite(RSS) ) recover()	# debugging the fit
		RSS
	}
}


partGLBoundParameters <- function(
		### Check if parameters are in the range and correct to bounds
		resOpt			##<< list with first entry vector of parameters, second entry its uncertainty
		, last_good		##<< vector of windows most recent good fit, make sure to set it to reasonable values, e.g. initial guess before optimiztion
){
	##details<<
	## adjustments according to TAble A1 in Lasslop 2010
	## vector positions: 1: k, 2: beta, 3: alpha, 4: rb
	#
	# Table A1 Initial guess; Valid Range; If outside range 
	# E_0.n  100;	50-400;	Set to value of previous window, if no previous window exists estimates <50 were set to 50, estimates >400 were set to 400
	# rb	Mean of nighttime NEE	>0;	Whole parameter set is not used
	# alpha	0.01;	?0,<0.22;	Set to value of previous window, if no previous window exists and <0, set to zero
	# beta0	Abs (0.03quantile - 0.97quantile) of NEE	?0; <250; If >100 then ? (?)<?	If negative set to zero, else the whole parameter set is not used
	# kVPD	;0	?0; 	Set to zero
	#parms_out_range<-0 #IF set to 1 means that the parameters are outside range and no computation uncertainties
	opt.parms.V <- resOpt[[1]]
	se.parms.V <- resOpt[[2]]
	isGoodParameterSet <- TRUE	# FALSE means parameters are outside range and no uncertainties are reported
	if( any(is.na(opt.parms.V)) ){
		isGoodParameterSet <- FALSE
	} 
	# isTRUE(as.vector()) returns FALSE also for NA
	if ( isTRUE((opt.parms.V[1] < 0)) ){	# k		
		opt.parms.V[1]<-0
		isGoodParameterSet <- FALSE
	}
	if ( isTRUE(as.vector(opt.parms.V[2] < 0)) ){	# beta
		opt.parms.V[2]<-0
		isGoodParameterSet <- FALSE
	}
	if ( isTRUE(as.vector(opt.parms.V[3] <= 0 || opt.parms.V[3] > 0.22))){ #set to alpha values of the latest good parameters set
		opt.parms.V[3]<-last_good[3]	
		isGoodParameterSet <- FALSE
	}
	#whole par set not used, if beta > 250 or (beta > 100 and sdBeta >= beta) or rb < 0
	if ( isTRUE(as.vector(opt.parms.V[2] > 250 || (opt.parms.V[2] > 100 && se.parms.V[2] >= opt.parms.V[2] ) ))){ 
		opt.parms.V[]<-NA
		isGoodParameterSet <- FALSE
	}
	if ( isTRUE(as.vector(opt.parms.V[4] < 0 ))){ 
		opt.parms.V[]<-NA
		isGoodParameterSet <- FALSE
	}
	if( !isGoodParameterSet){
		se.parms.V[]<-NA                  
	}
	##value<< list with entries
	list(
			opt.parms.V = opt.parms.V	##<< numeric vector of bounded optimized parameters
			,se.parms.V = se.parms.V	##<< numeric vector of uncertainties of parameters
			,isGoodParameterSet = isGoodParameterSet	##<< boolean scalar that is FALSE if original parameters were outside bounds
	)
}

partGLInterpolateFluxes <- function(
		### Interpolate ecoystem respiration (Reco) and Gross primary production (GPP) and associated uncertainty from two neighboring parameter sets of Light response curves 
		Rg   	##<< photosynthetic flux density [umol/m2/s] or Global Radiation
		,VPD 	##<< Vapor Pressure Deficit [hPa]
		,Temp 	##<< Temperature [degC] 
		,resParms	##<< data frame with results of \code{\link{partGLFitLRCWindows}} of fitting the light-response-curve for several windows
		,controlGLPart=partGLControl()	##<< further default parameters, see \code{\link{partGLControl}}
){
	##author<< TW
	##seealso<< \code{link{partitionNEEGL}}
	##details<< 
	## \code{resLRC$iFirstRecInCentralDay} must denote the row for which the LRC parameters are representative, 
	## here, the first record of the center day
	# create a dataframe with index of rows of estimates before and after and correponding weights
	summaryLRC <- resParms$summary
	nLRC <- nrow(summaryLRC)
	nRec <- length(Rg) 
	Temp_Kelvin <- Temp+273.15
	if( isTRUE(controlGLPart$isAssociateParmsToMeanOfValids) ){
	  # there might be several rows with the same iMeanRec, omit those rows unless the first of each reoccuring iMeanRec
    tabMeanRec <- table(summaryLRC$iMeanRec) 
    iRecsDouble <- as.integer(names(tabMeanRec[ tabMeanRec > 1L ]))
    iRecsOmit <- do.call(c, lapply( iRecsDouble, function(iRecDouble){
      which(summaryLRC$iMeanRec==iRecDouble)[-1L]
    }))
    if( length(iRecsOmit)) summaryLRC <- summaryLRC[-iRecsOmit,]
	}
	##details<<
	## Parameter estimates are reported for the first record of the window, or
	## if \code{isTRUE(controlGLPart.l$isAssociateParmsToMeanOfValids)} for the mean time of all valid records within the window
	# for each original record merge parameters assicated with previous fit or next fit respectively
	colNameAssoc <- if( isTRUE(controlGLPart$isAssociateParmsToMeanOfValids) ) "iMeanRec" else "iCentralRec" 
	dsAssoc <- .partGPAssociateSpecialRows(summaryLRC[[colNameAssoc]],nRec)
	# now we have iBefore and iAfter
	dsBefore <- merge( 
			structure(data.frame(dsAssoc$iSpecialBefore, dsAssoc$iBefore),names=c("iParRec",colNameAssoc))
			, summaryLRC[,c(colNameAssoc,"R_ref","E_0","a","b","k")]
			)
	dsAfter  <- merge( structure(data.frame(dsAssoc$iSpecialAfter, dsAssoc$iAfter),names=c("iParRec",colNameAssoc)), summaryLRC[,c(colNameAssoc,"R_ref","E_0","a","b","k")])
	if( (nrow(dsBefore) != nRec) || (nrow(dsAfter) != nRec)) stop("error in merging parameters to original records.")
	Reco2 <- lapply( list(dsBefore,dsAfter), function(dsi){
		tmp <- fLloydTaylor(dsi$R_ref, dsi$E_0, Temp_Kelvin, T_ref.n=273.15+15)
	})
	#dsi <- dsBefore
	GPP2 <- lapply( list(dsBefore,dsAfter), function(dsi){
				theta <- as.matrix(dsi[,c("k","b","a","R_ref","E_0")])
				tmp <- partGL_RHLightResponse(theta, Rg, VPD, Temp=Temp)$GPP
		})
	# interpolate between previous and next fit, weights already sum to 1
	Reco <- (dsAssoc$wBefore*Reco2[[1]] + dsAssoc$wAfter*Reco2[[2]]) #/ (dsAssoc$wBefore+dsAssoc$wAfter)  
	GPP <- (dsAssoc$wBefore*GPP2[[1]] + dsAssoc$wAfter*GPP2[[2]]) #/ (dsAssoc$wBefore+dsAssoc$wAfter)
#recover()  # to inspect the deviations between successive estimates	
	ans <- ansPred <- data.frame(
			Reco = Reco
			,GPP = GPP
	)
	if( isTRUE(controlGLPart$isSdPredComputed)){
		varPred2 <- lapply( list(dsBefore,dsAfter), function(dsi){
					theta <- as.matrix(dsi[,c("k","b","a","R_ref","E_0")])
					grad <- partGL_RHLightResponseGrad(theta, Rg, VPD, Temp)
					varPred <- matrix(NA_real_, nrow=nrow(dsi), ncol=2L, dimnames=list(NULL,c("varGPP","varReco")))
					#iRec <- 1L
					for( iRec in 1:nrow(dsi)){
						iParRec <- dsi$iParRec[iRec]
						# get the fitting object, TODO better document
						resOpt <- resParms$resOptList[[ iParRec ]]
						gradGPP <- grad$GPP[iRec,]
						gradReco <- grad$Reco[iRec,]
						# make sure parameter names match postions in covParms
						varPred[iRec,1L] <- varGPP <-  gradGPP %*% resOpt$covParms[1:3,1:3] %*% gradGPP
						varPred[iRec,2L] <- varReco <-  gradReco %*% resOpt$covParms[4:5,4:5] %*% gradReco
					}
					varPred
				})
		sdGPP <- sqrt(dsAssoc$wBefore^2*varPred2[[1]][,"varGPP"] + dsAssoc$wAfter^2*varPred2[[1]][,"varGPP"]) 
		sdReco <- sqrt(dsAssoc$wBefore^2*varPred2[[1]][,"varReco"] + dsAssoc$wAfter^2*varPred2[[1]][,"varReco"])   
		ans <- cbind(ansPred, data.frame(sdGPP=sdGPP, sdReco=sdReco))
	}
	# compute differences in rows to next parameter estimate
	dRecBefore <- dsBefore[[colNameAssoc]] - 1:nrow(dsBefore)  
	dRecAfter <- dsAfter[[colNameAssoc]] - 1:nrow(dsBefore)
	isBeforeCloser <- abs(dRecBefore) <= abs(dRecAfter)
	ans$dRecNextEstimate <- ifelse(isBeforeCloser, dRecBefore, dRecAfter)
	##value<< data.frame with nrow() rows and columns  GPP, Reco, varGPP, varReco, and dRecNextEstimate
	ans
}


.tmp.f <- function(){
	# execute on recover jsut before ans <- in .partGPInterploateFluxes, from test_that("interpolate Fluxes"
	# there is only one period with changing parametrization
	x <- seq_along(Rg)
	plot( Reco ~ x )
	lines( Reco2[[1]] ~ x, col="blue" )
	lines( Reco2[[2]] ~ x, col="orange" )
	# note how the black dots first follow the blue line, then converge to the orange line
	plot( GPP ~ x )
	lines( GPP2[[1]] ~ x, col="blue" )
	lines( GPP2[[2]] ~ x, col="orange" )
	# not much difference between blue and orange (difference only seen if plot window is very large)
}

.partGPAssociateSpecialRows <- function(
		### associate each row with the previous and next row from a subset of special rows
		iRowsSpecial	##<< ordered unique integer vector specifying the rows for which some special data is available
		,nRec			##<< integer scalar of number of rows in the full data.frame 
){
	##details<< 
	## When only for a subset of rows some more data available, i.e parameter estimates for 
	## only a subset of rows, this function creates
	## columns that refer to the previous and next row that are in the subset.
	## E.g. if some more data is available for rows 3 and 7, then rows 4:6 will indicate 
	## \code{iBefore=3, iAfter=7}.
	##value<< a dataframe with index of previous and next rows inside the subset
	ans <- data.frame(
			iRec=1:nRec				##<< the original row number
			, iSpecialBefore=NA_integer_	##<< index within \code{iRowsSpecial} 
			, iSpecialAfter=NA_integer_		##<< index within \code{iRowsSpecial}
			, iBefore=NA_integer_	##<< index of the previous special row 
			, iAfter=NA_integer_	##<< index of the next special row
			, wBefore=NA_real_		##<< weight of the previous, inverse of the distance in records
			, wAfter=NA_real_)		##<< weight of the next, inverse of the distance in records
	##details<<
	## The subset rows inside the subset refer both (before and after) to the same subset rows, with weights 1/2
	nRecS <- length(iRowsSpecial)
	if( 0 == nRecS ) stop("cannot associate special rows, if length of argument iRowsSpecial is zero.")
	ans[iRowsSpecial,"iSpecialBefore"] <- ans[iRowsSpecial,"iSpecialAfter"] <- seq_along(iRowsSpecial)
	ans[iRowsSpecial,"iBefore"] <- ans[iRowsSpecial,"iAfter"] <- iRowsSpecial
	ans[iRowsSpecial,c("wBefore","wAfter")] <- 0.5
	#iS <- 2L
	for( iS in 1:nRecS ){
		currRec <- iRowsSpecial[iS]
		# before and after last special row will be treated afterwards
		prevRec <- if( iS==1L) currRec else iRowsSpecial[iS-1L]
		nextRec <- if( iS==nRecS) currRec else iRowsSpecial[iS+1L]
		#c(prevRec,currRec,nextRec)
		##details<<
		## The weight is inversely proportional to the distance in rows
		## The two weights wBefore and wAfter always sum to 1 
		distPrev <- currRec-prevRec 
		if( distPrev > 1L){
			ans[(prevRec+1L):(currRec-1L),"iSpecialAfter"] <- iS
			ans[(prevRec+1L):(currRec-1L),"iAfter"] <- currRec
			ans[(prevRec+1L):(currRec-1L),"wAfter"] <- (1:(distPrev-1))/distPrev  	 
		}
		distNext <- nextRec-currRec
		if( distNext > 1L){
			ans[(currRec+1L):(nextRec-1L),"iSpecialBefore"] <- iS
			ans[(currRec+1L):(nextRec-1L),"iBefore"] <- currRec
			ans[(currRec+1L):(nextRec-1L),"wBefore"] <- ((distNext-1):1)/distNext  	
		} 	
	}
	##details<<
	## the rows before the first subset row refer bot (after and before) to the first subset row with weights 1/2
	## similar the rows after the last subset row refer to the last subset row
	ans[1:iRowsSpecial[1],c("iSpecialBefore","iSpecialAfter")] <- 1L
	ans[1:iRowsSpecial[1],c("iBefore","iAfter")] <- iRowsSpecial[1L] 
	ans[1:iRowsSpecial[1],c("wBefore","wAfter")] <- 0.5 
	ans[iRowsSpecial[nRecS]:nrow(ans),c("iSpecialBefore","iSpecialAfter")] <- nRecS
	ans[iRowsSpecial[nRecS]:nrow(ans),c("iBefore","iAfter")] <- iRowsSpecial[nRecS]
	ans[iRowsSpecial[nRecS]:nrow(ans),c("wBefore","wAfter")] <- 0.5
	ans
}


