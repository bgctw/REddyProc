partitionNEEGL=function(
		### Partitioning NEE fluxes into GP and Reco after daytime method. 
		ds							##<< dataset with all the specified input columns and full days in equidistant times		
		,NEEVar.s=paste0('NEE',SuffixDash.s,'_f')		##<< Variable of Net Ecosystem Exchange flux
		,QFNEEVar.s=paste0('NEE',SuffixDash.s,'_fqc')	##<< Quality flag of variable
		,QFNEEValue.n=0         						##<< Value of quality flag for _good_ (original) data
		,NEESdVar.s=paste0('NEE',SuffixDash.s,'_fsd')	##<< Variable of standard deviation of net ecosystem fluxes
		,TempVar.s=paste0('Tair_f')     ##<< Filled air or soil temperature variable (degC)
		,QFTempVar.s=paste0('Tair_fqc') ##<< Quality flag of filled temperature variable
		,QFTempValue.n=0       		##<< Value of temperature quality flag for _good_ (original) data
		,VPDVar.s=paste0('VPD_f')     ##<< Filled Vapor Pressure Deficit - VPD - (hPa)
		,QFVPDVar.s=paste0('VPD_fqc') ##<< Quality flag of filled VPD variable    
		,QFVPDValue.n=0        		##<< Value of VPD quality flag for _good_ (original) data
		,RadVar.s='Rg'         		##<< Unfilled (original) radiation variable
		,PotRadVar.s="PotRad_NEW"	##<< Variable name of potential radiation (W/m2)			   
		,Suffix.s = ""		   		##<< string inserted into column names before identifier for NEE column defaults (see \code{\link{sMDSGapFillAfterUstar}}).
		,controlGLPart=partGLControl()	##<< further default parameters, see \code{\link{partGLControl}}
		,isVerbose=TRUE			 	##<< set to FALSE to suppress output messages
		,nRecInDay=48L		 		##<< number of records within one day (for half-hourly data its 48)
		,lrcFitter=RectangularLRCFitter()	##<< R5 class instance responsible for fitting the light response curve.
			##<< Current possibilities are \code{RectangularLRCFitter()}, \code{NonrectangularLRCFitter()}, and \code{LogisticSigmoidLRCFitter()}. (See details)
)
##details<<
## Daytime-based partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco)
##
## The fit to the light-response-curve is done by default using the Rectangular hyperbolic function, as in Lasslop et al. (2010)
## Alternative fittings can be used by providing the correspodning subclass of \code{LightResponseCurveFitter-class} to \code{lrcFitter} argument.
## (see \code{\link{LightResponseCurveFitter_predictGPP}})
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
	## \itemize{
	## \item{Reco_DT_<suffix>}{predicted ecosystem respiraiton: mumol CO2/m2/second}
	## \item{GPP_DT_<suffix>}{predicted gross primary production mumol CO2/m2/second}
	## }
	RecoDTVar.s <- paste0('Reco_DT',SuffixDash.s) 
	GPPDTVar.s <- paste0('GPP_DT',SuffixDash.s) 
	RecoDTSdVar.s <- paste0(RecoDTVar.s,"_SD") 
	GPPDTSdVar.s <- paste0(GPPDTVar.s,"_SD") 
	dsAns0 <- data.frame(
			FP_VARnight=rep(NA_real_,nrow(ds))	##<< NEE filtered for nighttime records (others NA)
			,FP_VARday=NA_real_		##<< NEE filtered for daytime recores (others NA)
			,NEW_FP_Temp=NA_real_	##<< temperature after filtering for quality flag degree Celsius
			,NEW_FP_VPD=NA_real_	##<< vapour pressure deficit after filtering for quality flag, hPa
			,FP_RRef_Night=NA_real_	##<< basal respiration estimated from nighttime (W/m2)
			,FP_qc=NA_integer_		##<< quality flag: 0: good parameter fit, 1: some parameters out of range, required refit, 2: next parameter estimate is more than two weeks away
			,FP_dRecPar=NA_integer_	##<< records until or after closest record that has a parameter estimate associated
	)
	## \item{<LRC>}{Furhter light response curve (LRC) parameters and their standard deviation depend on the used LRC
	## (e.g. for the non-rectangular LRCC see \code{\link{NonrectangularLRCFitter_getParameterNames}}). 
	## They are estimated for windows and are reported with the first record of the window}
	##end<<
	# append LRC parameter result columns
	lrcParNames <- lrcFitter$getParameterNames()
	lrcParNames <- c(lrcParNames, paste0(lrcParNames,"_sd"))
	FP_lrcParNames <- paste0("FP_",lrcParNames)
	tmp <- matrix( NA_real_, nrow=nrow(ds), ncol=length(FP_lrcParNames), dimnames=list(NULL,FP_lrcParNames) )
	dsAns <- cbind(dsAns0, tmp)
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
	##seealso<< \code{\link{partGLFitNightTimeTRespSens}}
	dsTempSens <- partGLFitNightTimeTRespSens( dsR
			, nRecInDay=nRecInDay
			, controlGLPart=controlGLPart
	)
	##seealso<< \code{\link{partGLFitLRCWindows}}
	resParms <- partGLFitLRCWindows( dsR
			, nRecInDay=nRecInDay
			, dsTempSens=dsTempSens
			, controlGLPart=controlGLPart
			, lrcFitter=lrcFitter
	)
	# append parameter fits to the central record of day window
	#iGood <- which(resLRC$summary$parms_out_range == 0L)
	# resLRC provides parameters only for a subset of rows. For each row in the original data.frame
	# The parameter estimates are associated for interpolation of predicted fluxes 
	# either to the central record of the window, 
	# or the record with the time corresponding to the mean of all valid records in the window
	# default is isAssociateParmsToMeanOfValids=TRUE (double check partGLControl argument)
	colNameAssoc <- if( isTRUE(controlGLPart$isAssociateParmsToMeanOfValids) ) "iMeanRec" else "iCentralRec"
	# for the output, always report at central record
	dsAns[resParms$summary$iCentralRec,c("FP_RRef_Night","FP_qc",FP_lrcParNames)] <- 
					resParms$summary[,c("RRef_night","parms_out_range",lrcParNames)]
	#	
	##seealso<< \code{\link{partGLInterpolateFluxes}}
	dsAnsFluxes <- partGLInterpolateFluxes( ds[,RadVar.s]
					#, dsAns$NEW_FP_VPD, dsAns$NEW_FP_Temp		
					, ds[[VPDVar.s]], ds[[TempVar.s]]		# do prediction also using gap-Filled values
					, resParms
					, controlGLPart=controlGLPart
					, lrcFitter = lrcFitter
			)
	dsAns$FP_dRecPar <- dsAnsFluxes$dRecNextEstimate
	# copy quality flag from parameter row 
	# 	first copy from iCentralRec to colNameAssoc
	#	next transfer by dRecNextEstimate to each row
	iFiniteMeanRec <- which(is.finite(resParms$summary[[colNameAssoc]]))
	dsAns$FP_qc[resParms$summary[[colNameAssoc]][iFiniteMeanRec] ] <- dsAns$FP_qc[resParms$summary$iCentralRec[iFiniteMeanRec] ]
	dsAns$FP_qc <- dsAns$FP_qc[ 1:nrow(dsAns)+dsAns$FP_dRecPar ]
	#set quality flag to 2 where next parameter estimate is more than 14 days away 
	dsAns$FP_qc[ abs(dsAns$FP_dRecPar) > (14*nRecInDay)] <- 2L	# set quality flag to 2 for records where next estimate is more than 14 days away
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
		LRCFitConvergenceTolerance=1e-3	##<< convergence criterion for rectangular light response curve fit. 
		## If relative improvement of reducing residual sum of squares between predictions and 
		## observations is less than this criterion, assume convergence.
		## Decrease to get more precise parameter estimates, Increase for speedup.
		,nLRCFitConvergenceTolerance=1e-3	##<< convergence criterion for nonrectangular light response curve fit.
		## Here its a factor of machine tolerance.
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
		,NRHRfunction=FALSE #<< deprecated: Flag if TRUE use the NRHRF for partitioning; Now use \code{lrcFitter=NonrectangularLRCFitter()}
## Definition of the LRC Model
){
	##author<< TW
	##seealso<< \code{\link{partitionNEEGL}}
	##description<<
	## For highest compatibility to Lasslop10 use 
	## \code{nBootUncertainty=0L, isAssociateParmsToMeanOfValids=FALSE
	##		, isLasslopPriorsApplied=TRUE, isBoundLowerNEEUncertainty=FALSE
	##		, smoothTempSensEstimateAcrossTime=FALSE
	##      }
	if( NRHRfunction ) stop("option 'NRHRfunction' is deprecated. Use instead in partitionNEEGL argument: lrcFitter=NonrectangularLRCFitter()")
	ctrl <- list(  
			LRCFitConvergenceTolerance=LRCFitConvergenceTolerance
			,nLRCFitConvergenceTolerance=nLRCFitConvergenceTolerance
			,nBootUncertainty=nBootUncertainty
			,minNRecInDayWindow=minNRecInDayWindow 
			,isAssociateParmsToMeanOfValids=isAssociateParmsToMeanOfValids
			,isLasslopPriorsApplied=isLasslopPriorsApplied
			,isSdPredComputed=isSdPredComputed
			,isFilterMeteoQualityFlag=isFilterMeteoQualityFlag
			,isBoundLowerNEEUncertainty=isBoundLowerNEEUncertainty
			,smoothTempSensEstimateAcrossTime=smoothTempSensEstimateAcrossTime
			#,NRHRfunction=NRHRfunction
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
		,winSizeDays=4L		##<< Window size in days for daytime fits
		,strideInDays=2L	##<< step in days for shifting the windows
		,nRecInDay=48L		##<< number of records within one day (for half-hourly data its 48)
		,dsTempSens			##<< data.frame that reports for each window temperature sensitivity parameters E0 and RRef
		,isVerbose=TRUE		##<< set to FALSE to suppress messages
		,controlGLPart=partGLControl()		##<< list of further default parameters
		,lrcFitter			##<< R5 class instance responsible for fitting the light response curve  	
){
	##seealso<< \code{\link{partGLFitLRCOneWindow}}
	if( isVerbose ) message("  Estimating light response curve parameters from day time NEE ", appendLF = FALSE)
	resLRC <- applyWindows(ds, partGLFitLRCOneWindow, prevRes=list(resOpt=list(thetaOpt=rep(NA_real_, 6L)))
			, winSizeInDays=4L
			,isVerbose=isVerbose		
			,nRecInDay=nRecInDay
			#
			,E0Win = dsTempSens
			,controlGLPart=controlGLPart
			,lrcFitter=lrcFitter
	)
	##value<< a list with item \code{resOptList} with all optimization results 
	## and a item \code{summary} listing winInfo, LRC parameters, and their standard deviation  
	## and estimated from night-time data after smoothing and forward-filling: 
	## the uncertainty of temperature sensitivity \code{E0_night_sd} 
	## and the respiration at reference temperature \code{RRef_night}.
	lrcSummary <- lapply(resLRC$resFUN, "[[", "summary")
	iNoSummary <- which( sapply(lrcSummary, length)==0 )
	if( length(iNoSummary) ){
		stop("expected summary returned by all fits, but found missing summaries.")
		# put dummy NA data.frame where no fit was obtained
		dummySummary <- lrcSummary[-iNoSummary][[1]]
		dummySummary[] <- NA
		lrcSummary[iNoSummary] <- list(dummySummary)
	}
	resParms <- list(
			resOptList = lapply(resLRC$resFUN, "[[", "resOpt")
			,summary = cbind( resLRC$winInfo, rbind.fill(lrcSummary)) 
	)
	#table(resParms$summary$convergence)
	#E0_night equals E0, but uncertaint might differ 
	#resParms$summary$E0_night <- dsTempSens$E0
	resParms$summary$E0_night_sd <- dsTempSens$sdE0
	resParms$summary$RRef_night <- dsTempSens$RRef
	# summary$iMeanRec yet based on window instead of entire time, need to add beginning of window
	resParms$summary$iMeanRec <- resParms$summary$iRecStart-1L + resParms$summary$iMeanRec
#	# omit records where NULL was returned
#	iWinNoFit <- which( is.na(resParms$summary$parms_out_range) )	
#	if( length(iWinNoFit) ){
#		resParms$summary <- resParms$summary[-iWinNoFit, ]
#		resParms$resOptList <- resParms$resOptList[-iWinNoFit] 
#	}
	resParms
}

partGLFitLRCOneWindow=function(
		### Estimate parameters of the Rectangular Hyperbolic Light Response Curve function (a,b,R_ref, k) for successive periods
		ds					##<< data.frame with numeric columns NEE, sdNEE, Temp (degC), VPD, Rg, and logical columns isNight and isDay
		,winInfo			##<< one-row data.frame with window information, including iWindow 
		,prevRes			##<< component prevRes from previous result, here with item prevE0
		,E0Win				##<< data.frame with columns E0, sdE0, RRef from nighttime, one row for each window
		,controlGLPart=partGLControl()	##<< list of further default parameters
		,lrcFitter=RectangularLRCFitter()	##<< R5 class instance responsible for fitting the light response curve  	
){
	##author<< TW
	##seealso<< \code{\link{partitionNEEGL}}
	##description<<
	## Estimation as in Lasslop et al., 2010 for successive periods, i.e. windows.
	#requiredCols <- c("NEE", "sdNEE", "Temp", "VPD", "Rg", "isNight", "isDay")
	#iMissing <- which( is.na(match( requiredCols, names(ds) )))
	#if( length(iMissing) ) stop("missing columns: ",paste0(requiredCols[iMissing],collapse=","))
	isValidDayRec <- !is.na(ds$isDay) & ds$isDay & !is.na(ds$NEE) & !is.na(ds$sdNEE) & !is.na(ds$Temp) & !is.na(ds$VPD) & !is.na(ds$Rg) 
	dsDay <- ds[isValidDayRec,]
	##details<<
	## Each window estimate is associated with a time or equivalently with a record.
	## The first record, i.e. row number, of the day-window is reported.
	## Moreover, the mean of all valid records numbers in the daytime window is reported for interpolation.
	iMeanRecInDayWindow <- as.integer(round(mean(which(isValidDayRec))))
	#better report NA and care for it properly: if( is.na(iMeanRecInDayWindow)) iMeanRecInDayWindow <- as.integer(nrow(ds)%/%2)
	#TODO firstRecInDayWindow.i <- which(SubsetDayPeriod.b)[1] # the rownumber of the first record inside the day window
	##seealso<< \code{\link{partGLEstimateTempSensInBoundsE0Only}}
	E0 <- E0Win$E0[winInfo$iWindow]
	# if too few records or 
	getNAResult <- function(convergenceCode){ list(
			resOpt = NULL
			,summary = data.frame(
					nValidRec=nrow(dsDay)
					,iMeanRec=iMeanRecInDayWindow
					,convergence=convergenceCode
			)
			,isValid=FALSE
	)}
	# if no temperature-respiration relationship could be found, indicate no-fit, but report Window properties
	if( is.na(E0)  ) return(getNAResult(1010L))
	if( (sum(isValidDayRec) < controlGLPart$minNRecInDayWindow) ) return(getNAResult(1011L))
	# if( DayStart.i > 72 ) recover()		
	sdE0 <- E0Win$sdE0[winInfo$iWindow]
	RRefNight <- E0Win$RRef[winInfo$iWindow]
	#
	##seealso<< \code{\link{LightResponseCurveFitter_fitLRC}}
	resOpt <- resOpt0 <- lrcFitter$fitLRC(dsDay, E0=E0, sdE0=sdE0, RRefNight=RRefNight
			, controlGLPart=controlGLPart, lastGoodParameters=prevRes$resOpt$thetaOpt)
	if( !is.finite(resOpt$thetaOpt[1]) ) {
		return(getNAResult(resOpt$convergence))
	}
	sdTheta <- resOpt$thetaOpt; sdTheta[] <- NA
	sdTheta[resOpt$iOpt] <- sqrt(diag(resOpt$covParms)[resOpt$iOpt])
	#
	# record valid fits results
	as.data.frame(t(resOpt$thetaOpt))
	ans <- list(
		 resOpt=resOpt
		 ,summary = cbind(data.frame(
			nValidRec=nrow(dsDay)
			,iMeanRec=iMeanRecInDayWindow
			,convergence=resOpt$convergence
			,parms_out_range=as.integer(!identical(resOpt$iOpt,1:5))
			)
			,as.data.frame(t(resOpt$thetaOpt))
			,as.data.frame(t(structure(sdTheta,names=paste0(names(sdTheta),"_sd"))))
		,isValid=TRUE
		)
	)
	return(ans)
}

.bootStrapLRCFit <- function(
		### Compute parameters uncertainty by bootstrap
		theta0, iOpt, dsDay, sdE_0.n, parameterPrior, controlGLPart
		, lrcFitter		##<< Light Response Curve R5 instance
		,iPosE0=5L	##<< position (integer scalar) of temperature sensitivity in parameter vector
){
	##value<<
	## matrix with each row a parameter estimate on a different bootstrap sample
	ans <-matrix(NA, nrow=controlGLPart$nBootUncertainty, ncol=length(theta0), dimnames=list(NULL,names(theta0)))
	##details<<
	## In addition to resampling the original data, also the temperature sensitivity is resampled 
	## from its uncertainty distribution.
	E0r <- rnorm( controlGLPart$nBootUncertainty, theta0[5L], sdE_0.n	)
	E0 <- pmax(50,pmin(400,E0r))
	theta <- theta0
	#iBoot <- 1L
	for (iBoot in c(1:controlGLPart$nBootUncertainty)){
		idx <- sample(nrow(dsDay), replace=TRUE)
		dsDayB <- dsDay[idx,]
		theta[iPosE0] <- E0[iBoot]
		resOptBoot <- lrcFitter$optimLRCOnAdjustedPrior(theta, iOpt=iOpt, dsDay=dsDayB, parameterPrior=parameterPrior, ctrl=controlGLPart)
		if( resOptBoot$convergence == 0L ){	
			#TODO: also remove the very bad cases? 
			ans[iBoot,]<-resOptBoot$theta
		}else{
			#recover()
		}
	}
	ans
}


partGLInterpolateFluxes <- function(
		### Interpolate ecoystem respiration (Reco) and Gross primary production (GPP) and associated uncertainty from two neighboring parameter sets of Light response curves 
		Rg   	##<< photosynthetic flux density [umol/m2/s] or Global Radiation
		,VPD 	##<< Vapor Pressure Deficit [hPa]
		,Temp 	##<< Temperature [degC] 
		,resParms	##<< data frame with results of \code{\link{partGLFitLRCWindows}} of fitting the light-response-curve for several windows
		,controlGLPart=partGLControl()	##<< further default parameters, see \code{\link{partGLControl}}
		,lrcFitter	##<< R5 class instance responsible for fitting the light response curve  	
){
	##author<< TW
	##seealso<< \code{link{partitionNEEGL}}
	##details<< 
	## \code{resLRC$iFirstRecInCentralDay} must denote the row for which the LRC parameters are representative, 
	## here, the first record of the center day
	# create a dataframe with index of rows of estimates before and after and correponding weights
	isValidWin <- is.finite(resParms$summary$parms_out_range)
	summaryLRC <- resParms$summary[ isValidWin, ,drop=FALSE]
    resOptList <- resParms$resOptList[isValidWin]
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
	parNames <- lrcFitter$getParameterNames()
	dsBefore <- merge( 
			structure(data.frame(dsAssoc$iSpecialBefore, dsAssoc$iBefore),names=c("iParRec",colNameAssoc))
			, summaryLRC[,c(colNameAssoc, parNames)]
	)
	dsAfter  <- merge( structure(data.frame(dsAssoc$iSpecialAfter, dsAssoc$iAfter),names=c("iParRec",colNameAssoc)), summaryLRC[,c(colNameAssoc,lrcFitter$getParameterNames())])
	if( (nrow(dsBefore) != nRec) || (nrow(dsAfter) != nRec)) stop("error in merging parameters to original records.")
	Reco2 <- lapply( list(dsBefore,dsAfter), function(dsi){
		#twutz170316: fLloydTaylor gives unreasonable values with very low temperatures, hence constrain lower temperature
		tmp <- fLloydTaylor(dsi$RRef, dsi$E0, pmax(-40,Temp_Kelvin), T_ref.n=273.15+15)
	})
	#dsi <- dsBefore
	GPP2 <- lapply( list(dsBefore,dsAfter), function(dsi){
							theta <- as.matrix(dsi[,parNames])
							tmp <- lrcFitter$predictLRC(theta, Rg, VPD, Temp=pmax(-40,Temp))$GPP
						})
	# interpolate between previous and next fit, weights already sum to 1
	Reco <- (dsAssoc$wBefore*Reco2[[1]] + dsAssoc$wAfter*Reco2[[2]]) #/ (dsAssoc$wBefore+dsAssoc$wAfter)  
	GPP <- (dsAssoc$wBefore*GPP2[[1]] + dsAssoc$wAfter*GPP2[[2]]) #/ (dsAssoc$wBefore+dsAssoc$wAfter)
	ans <- ansPred <- data.frame(
			Reco = Reco
			,GPP = GPP
	)
	if( isTRUE(controlGLPart$isSdPredComputed)){
		#dsi <- dsBefore
		varPred2 <- lapply( list(dsBefore,dsAfter), function(dsi){
							theta <- as.matrix(dsi[,parNames])
							grad <- lrcFitter$computeLRCGradient(theta, Rg, VPD, pmax(-40,Temp))
							varPred <- matrix(NA_real_, nrow=nrow(dsi), ncol=2L, dimnames=list(NULL,c("varGPP","varReco")))
							#iRec <- 1L
							for( iRec in 1:nrow(dsi)){
								iParRec <- dsi$iParRec[iRec]	# row within sequence of valid parameters (at centralREc or meanRec)
								# get the fitting object, TODO better document
								resOpt <- resOptList[[ iParRec ]]
								gradGPP <- grad$GPP[iRec,]
								gradReco <- grad$Reco[iRec,]
								# make sure parameter names match postions in covParms
								varPred[iRec,2L] <- varReco <-  gradReco %*% resOpt$covParms[names(gradReco),names(gradReco)] %*% gradReco
								varPred[iRec,1L] <- varGPP <-  gradGPP %*% resOpt$covParms[names(gradGPP),names(gradGPP)] %*% gradGPP
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


