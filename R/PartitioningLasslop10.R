partitionNEEGL=function(
		### Partitioning NEE fluxes into GP and Reco after daytime method. 
		ds							##<< dataset with all the specified input columns and full days in equidistant times
		,NEEVar.s=paste0('NEE',SuffixDash.s,'_f')		##<< Variable of Net Ecosystem Exchange flux
		,TempVar.s=paste0('Tair_f') ##<< Filled air or soil temperature variable (degC)
		,VPDVar.s=paste0('VPD_f')   ##<< Filled Vapor Pressure Deficit - VPD - (hPa)
		,Suffix.s = ""		   		##<< string inserted into column names before identifier for NEE column defaults (see \code{\link{sMDSGapFillAfterUstar}}).
		,...						##<< further arguments to \code{\link{partGLExtractStandardData}}, such as \code{PotRadVar.s}
		,controlGLPart=partGLControl()	##<< further default parameters, see \code{\link{partGLControl}}
		,isVerbose=TRUE			 	##<< set to FALSE to suppress output messages
		,nRecInDay=48L		 		##<< number of records within one day (for half-hourly data its 48)
		,lrcFitter=RectangularLRCFitter()	##<< R5 class instance responsible for fitting the light response curve.
			## Current possibilities are \code{RectangularLRCFitter()}, \code{NonrectangularLRCFitter()}, and \code{LogisticSigmoidLRCFitter()}. (See details)
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
	SuffixDash.s <- paste( (if(fCheckValString(Suffix.s)) "_" else ""), Suffix.s, sep="") # used to compute default NEEVar.s 
	if( isVerbose ) message('Start daytime flux partitioning for variable ', NEEVar.s, ' with temperature ', TempVar.s, '.')
	dsR <- partGLExtractStandardData(ds, NEEVar.s=NEEVar.s, TempVar.s=TempVar.s, VPDVar.s=VPDVar.s, Suffix.s=Suffix.s
		, ..., controlGLPart = controlGLPart)
	##value<< 
	## \itemize{
	## \item{Reco_DT_<suffix>}{predicted ecosystem respiraiton: mumol CO2/m2/second}
	## \item{GPP_DT_<suffix>}{predicted gross primary production mumol CO2/m2/second}
	## }
	dsAns0 <- data.frame(
			FP_VARnight=rep(NA_real_,nrow(ds))	##<< NEE filtered for nighttime records (others NA)
			,FP_VARday=NA_real_		##<< NEE filtered for daytime recores (others NA)
			,NEW_FP_Temp=dsR$Temp	##<< temperature after filtering for quality flag degree Celsius
			,NEW_FP_VPD=dsR$VPD		##<< vapour pressure deficit after filtering for quality flag, hPa
			,FP_RRef_Night=NA_real_	##<< basal respiration estimated from nighttime (W/m2)
			,FP_qc=NA_integer_		##<< quality flag: 0: good parameter fit, 1: some parameters out of range, required refit, 2: next parameter estimate is more than two weeks away
			,FP_dRecPar=NA_integer_	##<< records until or after closest record that has a parameter estimate associated
			,FP_errorcode=NA_integer_ ##<< information why LRC-fit was not successful or was rejected, see result of \code{\link{LightResponseCurveFitter_fitLRC}}
	)
	## \item{<LRC>}{Further light response curve (LRC) parameters and their standard deviation depend on the used LRC
	## (e.g. for the non-rectangular LRCC see \code{\link{NonrectangularLRCFitter_getParameterNames}}). 
	## They are estimated for windows and are reported with the first record of the window}
	##end<<
	# append LRC parameter result columns
	lrcParNames <- lrcFitter$getParameterNames()
	lrcParNames <- c(lrcParNames, paste0(lrcParNames,"_sd"))
	FP_lrcParNames <- paste0("FP_",lrcParNames)
	tmp <- matrix( NA_real_, nrow=nrow(ds), ncol=length(FP_lrcParNames), dimnames=list(NULL,FP_lrcParNames) )
	dsAns <- cbind(dsAns0, tmp)
	dsAns$FP_VARnight <- ifelse(dsR$isNight, dsR$NEE, NA)
	attr(dsAns$FP_VARnight, 'varnames') <- paste(attr(dsR$NEE, 'varnames'), '_night', sep='')
	attr(dsAns$FP_VARnight, 'units') <- attr(dsR$NEE, 'units')
	dsAns$FP_VARday <- ifelse(dsR$isDay, dsR$NEE, NA)
	attr(dsAns$FP_VARday, 'varnames') <- paste(attr(dsR$NEE, 'varnames'), '_day', sep='')
	attr(dsAns$FP_VARday, 'units') <- attr(dsR$NEE, 'units')
	#
	#Estimate Parameters of light response curve: R_ref, alpha, beta and k according to Table A1 (Lasslop et al., 2010)
	# save(ds, file="tmp/dsTestPartitioningLasslop10.RData")
	#
	##seealso<< \code{\link{partGLFitNightTimeTRespSens}}
	isUsingFixedTempSens <- length(controlGLPart$fixedTempSens) && 
			all(is.finite(controlGLPart$fixedTempSens$E0)) &&
			all(is.finite(controlGLPart$fixedTempSens$sdE0)) 
	dsTempSens <- if( !isUsingFixedTempSens	){ 
				dsTempSens <- partGLFitNightTimeTRespSens( dsR
						, nRecInDay=nRecInDay
						, controlGLPart=controlGLPart
				)
			} else	{
				dsTempSens <- controlGLPart$fixedTempSens
				nWindow <- length(getStartRecsOfWindows( nrow(dsR), nRecInDay = nRecInDay))
				# if one row is given, use it for all windows
				if( nrow(dsTempSens) == 1L ) dsTempSens <- do.call(rbind, lapply(1:nWindow, function(i) dsTempSens ))
				if( nrow(dsTempSens) != nWindow ) stop("when providing controlGLPart$fixedTempSens then it must be 1 row",
							"or rows must macht the number of windows (",nWindow,"), but was ",nrow(dsTempSens))
				# if no RRef is given, estimate from nighttime data
				if( !length(dsTempSens$RRef) || all(is.na(dsTempSens$RRef)) ){
					resRef15 <- simplifyApplyWindows( tmp <- applyWindows(dsR, partGLFitNightRespRefOneWindow
									,nRecInDay=nRecInDay
									,E0Win = dsTempSens
									,controlGLPart=controlGLPart	
							))
					dsTempSens$RRef <- resRef15$RRef
				}
				# fill missing RRef by previous window
				dsTempSens$RRef <- fillNAForward( dsTempSens$RRef, firstValue= 
								# on NA at the start of the series, take the first occuring finite value
								dsTempSens$RRef[ which(is.finite(dsTempSens$RRef))[1] ] )
				dsTempSens
			}
	##seealso<< \code{\link{partGLFitLRCWindows}}
	resParms <- partGLFitLRCWindows( dsR
			, nRecInDay=nRecInDay
			, dsTempSens=dsTempSens
			, controlGLPart=controlGLPart
			, lrcFitter=lrcFitter
	)
	# if no windows was fitted, return error. Else error on missing columns (no parameter columns returned)
	if( sum(resParms$summary$convergence==0) == 0 ) stop("could not fit a single window in dayTime partitioning. Check the data.")
	# append parameter fits to the central record of day window
	#iGood <- which(resLRC$summary$parms_out_range == 0L)
	# resLRC provides parameters only for a subset of rows. For each row in the original data.frame
	# The parameter estimates are associated for interpolation of predicted fluxes 
	# either to the central record of the window, 
	# or the record with the time corresponding to the mean of all valid records in the window
	# default is isAssociateParmsToMeanOfValids=TRUE (double check partGLControl argument)
	colNameAssoc <- if( isTRUE(controlGLPart$isAssociateParmsToMeanOfValids) ) "iMeanRec" else "iCentralRec"
	# for the output, always report at central record
	dsAns[resParms$summary$iCentralRec,c("FP_RRef_Night","FP_qc","FP_errorcode",FP_lrcParNames)] <- 
					resParms$summary[,c("RRef_night","parms_out_range","convergence",lrcParNames)]
	#	
	##seealso<< \code{\link{partGLInterpolateFluxes}}
	dsAnsFluxes <- partGLInterpolateFluxes( dsR$Rg
					#, dsAns$NEW_FP_VPD, dsAns$NEW_FP_Temp		
					, ds[[VPDVar.s]], ds[[TempVar.s]]		# do prediction using non-filtered, i.e. gap-filled, values
					, resParms
					, controlGLPart=controlGLPart
					, lrcFitter = lrcFitter
			)
	if( !controlGLPart$isNeglectVPDEffect && controlGLPart$isRefitMissingVPDWithNeglectVPDEffect){
		##details<<
		## There common case where VPD is missing for predicting GPP, the default 
		## (with \code{controlGLPart$isRefitMissingVPDWithNeglectVPDEffect=TRUE})
		## is to redo the estimation of LRC parameters with neglecting the VPD-effect.
		## The cases (rows) with missing VPD are then replaced with predictions based on LRC-fits that neglected the VPD effect.  
		iNAVPD <- which(is.na(ds[[VPDVar.s]] & is.na(dsAnsFluxes$GPP)))
		if( length(iNAVPD)){
			message("  could not predict GPP in ",length(iNAVPD)," cases due to missing VPD.")
			message("    Therefore refitting LightResponseCurve with option isNeglectVPDEffect=TRUE")
			ctrl2 <- within(controlGLPart, isNeglectVPDEffect<-TRUE)
			resParms2 <- partGLFitLRCWindows( dsR
					, nRecInDay=nRecInDay
					, dsTempSens=dsTempSens
					, controlGLPart=ctrl2
					, lrcFitter=lrcFitter
			)
			dsAnsFluxes2 <- partGLInterpolateFluxes( dsR$Rg
					#, dsAns$NEW_FP_VPD, dsAns$NEW_FP_Temp		
					, ds[[VPDVar.s]], ds[[TempVar.s]]		# do prediction also using gap-Filled values
					, resParms2
					, controlGLPart=ctrl2
					, lrcFitter = lrcFitter
			)
			dsAnsFluxes[iNAVPD,] <- dsAnsFluxes2[iNAVPD,]
		}
	}
	nNAGPP <- sum(is.na(dsAnsFluxes$GPP))
	if( nNAGPP ) warning("could not predict GPP in ",nNAGPP," cases.")
	#
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
	RecoDTVar.s <- paste0('Reco_DT',SuffixDash.s) 
	GPPDTVar.s <- paste0('GPP_DT',SuffixDash.s) 
	RecoDTSdVar.s <- paste0(RecoDTVar.s,"_SD") 
	GPPDTSdVar.s <- paste0(GPPDTVar.s,"_SD")
	dsAns[[RecoDTVar.s]] <- dsAnsFluxes$Reco
	attr(dsAns[[RecoDTVar.s]], 'varnames') <- RecoDTVar.s
	attr(dsAns[[RecoDTVar.s]], 'units') <- attr(dsR$NEE, 'units')
	dsAns[[GPPDTVar.s]] <- dsAnsFluxes$GPP
	attr(dsAns[[GPPDTVar.s]], 'varnames') <- GPPDTVar.s
	attr(dsAns[[GPPDTVar.s]], 'units') <- attr(dsR$NEE, 'units')
	if( controlGLPart$isSdPredComputed ){
		dsAns[[RecoDTSdVar.s]] <- dsAnsFluxes$sdReco
		attr(dsAns[[RecoDTSdVar.s]], 'varnames') <- RecoDTSdVar.s
		attr(dsAns[[RecoDTSdVar.s]], 'units') <- attr(dsR$NEE, 'units')
		dsAns[[GPPDTSdVar.s]] <- dsAnsFluxes$sdGPP
		attr(dsAns[[GPPDTSdVar.s]], 'varnames') <- GPPDTSdVar.s
		attr(dsAns[[GPPDTSdVar.s]], 'units') <- attr(dsR$NEE, 'units')
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
		,isLasslopPriorsApplied=TRUE	##<< set to TRUE to apply strong fixed priors on LRC fitting.	
		## Returned parameter estimates claimed valid for some case where not enough data was available
		,isUsingLasslopQualityConstraints=FALSE	##<< set to TRUE to avoid quality constraints additional to Lasslop 2010		
		,isSdPredComputed=TRUE			##<< set to FALSE to avoid computing standard errors 
			## of Reco and GPP for small performance increase 	
		,isFilterMeteoQualityFlag=FALSE	##<< set to TRUE to use only records where quality flag 
		## of meteo drivers (Radation, Temperatrue, VPD) is zero, i.e. non-gapfilled for parameter estimation.
		## For prediction, the gap-filled value is used always, to produce predictions also for gaps.
		,isBoundLowerNEEUncertainty=TRUE	##<< set to FALSE to avoid adjustment of very low uncertainties before
			## day-Time fitting that avoids the high leverage those records with unreasonable low uncertainty.
		,fixedTRefAtNightTime=NA		##<< if a finite value (degree Centigrade) is given, 
			## it is used instead of median data temperature as reference temperature in estimation of temperatue sensitivity from night data
		,smoothTempSensEstimateAcrossTime=TRUE	##<< set to FALSE to use independent estimates of temperature 
			## sensitivity on each windows instead of a vector of E0 that is smoothed over time
		,NRHRfunction=FALSE				##<< deprecated: Flag if TRUE use the NRHRF for partitioning; Now use \code{lrcFitter=NonrectangularLRCFitter()}
		,isNeglectVPDEffect=FALSE 		##<< set to TRUE to avoid using VPD in the computations. This may help when VPD is rarely measured.
		,isRefitMissingVPDWithNeglectVPDEffect=TRUE	##<< set to FALSE to avoid repeating estimation with \code{isNeglectVPDEffect=TRUE} trying to predict when VPD is missing
		,fixedTempSens=data.frame(E0=NA_real_, sdE0=NA_real_, RRef=NA_real_)	##<< data.frame of one row or nRow=nWindow
			## corresponding to return value of \code{\link{partGLFitNightTimeTRespSens}}
			## While column \code{RRef} is used only as a  prior and initial value for the daytime-fitting and can be NA,
			## \code{E0} is used as given temperature sensitivity and varied according to \code{sdE0} in the bootstrap.
		,replaceMissingSdNEEParms=c(perc=0.2, minSd=0.7)	##<< parameters for replacing missing standard deviation of NEE.
			## see \code{\link{replaceMissingSdByPercentage}}.
			## Default sets missing uncertainty to 20% of NEE but at least 0.7 flux-units (usually mumo CO2/m2/s).
			## Specify c(NA,NA) to avoid replacing missings in standard deviation of NEE and to omit those records from LRC fit.
		,neglectNEEUncertaintyOnMissing=FALSE	##<< If set to TRUE: if there are records with missing uncertainty of NEE inside one window, set all uncertainties to 1. 
			## This overules option replaceMissingSdNEEParms.
		,minPropSaturation=NA	##<< quality criterion for sufficient data in window. 
			## If GPP prediction of highest PAR of window is less than minPropSaturation*(GPP at light-saturation, i.e. beta)
			## this indicates that PAR is not sufficiently high to constrain the shape of the LRC
){
	##author<< TW
	##seealso<< \code{\link{partitionNEEGL}}
	##description<<
	## For highest compatibility to the pvWave code of G.Lasslop (used by first BGC-online tool)
	## see function \code{\link{partGLControlLasslopCompatible}}. 
	if( NRHRfunction ) stop("option 'NRHRfunction' is deprecated. Use instead in partitionNEEGL argument: lrcFitter=NonrectangularLRCFitter()")
	if( isTRUE(neglectNEEUncertaintyOnMissing) ) replaceMissingSdNEEParms <- c(NA,NA)
	ctrl <- list(  
			LRCFitConvergenceTolerance=LRCFitConvergenceTolerance
			,nLRCFitConvergenceTolerance=nLRCFitConvergenceTolerance
			,nBootUncertainty=nBootUncertainty
			,minNRecInDayWindow=minNRecInDayWindow 
			,isAssociateParmsToMeanOfValids=isAssociateParmsToMeanOfValids
			,isLasslopPriorsApplied=isLasslopPriorsApplied
			,isUsingLasslopQualityConstraints=isUsingLasslopQualityConstraints
			,isSdPredComputed=isSdPredComputed
			,isFilterMeteoQualityFlag=isFilterMeteoQualityFlag
			,isBoundLowerNEEUncertainty=isBoundLowerNEEUncertainty
			,fixedTRefAtNightTime=fixedTRefAtNightTime
			,smoothTempSensEstimateAcrossTime=smoothTempSensEstimateAcrossTime
			,isNeglectVPDEffect=isNeglectVPDEffect
			,isRefitMissingVPDWithNeglectVPDEffect=isRefitMissingVPDWithNeglectVPDEffect
			,fixedTempSens=fixedTempSens
			,replaceMissingSdNEEParms=replaceMissingSdNEEParms
			,neglectNEEUncertaintyOnMissing = neglectNEEUncertaintyOnMissing
			,minPropSaturation=minPropSaturation
	)
	#display warning message for the following variables that we advise not to be changed
	#if (corrCheck != 0.5) warning("WARNING: parameter corrCheck set to non default value!")
	##value<< list with entries of given arguments.
	ctrl
}
attr(partGLControl,"ex") <- function(){
	partGLControl(nBootUncertainty=40L)
}

partGLControlLasslopCompatible <- function(
		### List of parameters for daytime flux partitioning that ensure highest compatibility with the pvWave implementation of Lasslop 
		nBootUncertainty=0L						##<< 0: Derive uncertainty from curvature of a single fit, neglecting the uncertainty of previously estimated temperature sensitivity, E0
		,minNRecInDayWindow = 10L 				##<< Minimum number of 10 valid records for regression in a single window 
		,isAssociateParmsToMeanOfValids=FALSE	##<< associate parameters to 
			## the first record of the window for interpolation instead of mean across valid records inside a window
		,isLasslopPriorsApplied=TRUE			##<< Apply fixed Lasslop priors in LRC fitting.	
		,isUsingLasslopQualityConstraints=TRUE	##<< avoid quality constraints additional to the ones in Lasslop 2010		
		,isBoundLowerNEEUncertainty=FALSE		##<< FALSE: avoid adjustment of very low uncertainties before
			## day-Time fitting that avoids the high leverage those records with unreasonable low uncertainty.
		,fixedTRefAtNightTime=15				##<< use fixed (degree Centigrade) temperature sensitivity 
			## instead of median data temperature as reference temperature in estimation of temperatue sensitivity from night data
		,smoothTempSensEstimateAcrossTime=FALSE	##<< FALSE: use independent estimates of temperature 
			## sensitivity on each windows instead of a vector of E0 that is smoothed over time
		,isRefitMissingVPDWithNeglectVPDEffect=FALSE	##<< FALSE: avoid repeating estimation with \code{isNeglectVPDEffect=TRUE}
		,minPropSaturation=NA					##<< NA: avoid quality constraint of sufficient saturation in data 
			## This option is overruled, i.e. not considered, if option isUsingLasslopQualityConstraints=TRUE.
		,isNeglectVPDEffect=FALSE 				##<< FALSE: do not neglect VPD effect
		,replaceMissingSdNEEParms=c(NA,NA)		##<< do not replace missing NEE, but see option 
		,neglectNEEUncertaintyOnMissing=TRUE	##<< if there are records with missing uncertainty of NEE inside one window, set all sdNEE to 1. 
			## This overules option replaceMissingSdNEEParms.
		, ... 									##<< further arguemtns to \code{\link{partGLControl}}
){
	##author<< TW
	##seealso<< \code{\link{partGLControl}}
	partGLControl(
			nBootUncertainty=nBootUncertainty
			,minNRecInDayWindow=minNRecInDayWindow 
			,isAssociateParmsToMeanOfValids=isAssociateParmsToMeanOfValids
			,isLasslopPriorsApplied=isLasslopPriorsApplied
			,isUsingLasslopQualityConstraints=isUsingLasslopQualityConstraints
			,isBoundLowerNEEUncertainty=isBoundLowerNEEUncertainty
			,fixedTRefAtNightTime=fixedTRefAtNightTime
			,smoothTempSensEstimateAcrossTime=smoothTempSensEstimateAcrossTime
			,isNeglectVPDEffect=isNeglectVPDEffect
			,isRefitMissingVPDWithNeglectVPDEffect=isRefitMissingVPDWithNeglectVPDEffect
			,replaceMissingSdNEEParms = replaceMissingSdNEEParms
			,neglectNEEUncertaintyOnMissing = neglectNEEUncertaintyOnMissing
			,minPropSaturation=minPropSaturation
			,...
	)
}
attr(partGLControlLasslopCompatible,"ex") <- function(){
	partGLControlLasslopCompatible()
}

partGLExtractStandardData <- function(
		### extract the relevant columns from original input to generate dataset with defined names 
		ds								##<< dataset with all the specified input columns and full days in equidistant times
		,NEEVar.s=paste0('NEE',SuffixDash.s,'_f')		##<< Variable of Net Ecosystem Exchange flux
		,QFNEEVar.s=paste0('NEE',SuffixDash.s,'_fqc')	##<< Quality flag of variable
		,QFNEEValue.n=0         						##<< Value of quality flag for _good_ (original) data
		,NEESdVar.s=paste0('NEE',SuffixDash.s,'_fsd')	##<< Variable of standard deviation of net ecosystem fluxes
		,TempVar.s=paste0('Tair_f')     ##<< Filled air or soil temperature variable (degC)
		,QFTempVar.s=paste0('Tair_fqc') ##<< Quality flag of filled temperature variable
		,QFTempValue.n=0       			##<< Value of temperature quality flag for _good_ (original) data
		,VPDVar.s=paste0('VPD_f')     	##<< Filled Vapor Pressure Deficit - VPD - (hPa)
		,QFVPDVar.s=paste0('VPD_fqc') 	##<< Quality flag of filled VPD variable    
		,QFVPDValue.n=0        			##<< Value of VPD quality flag for _good_ (original) data
		,RadVar.s='Rg'         			##<< Unfilled (original) radiation variable
		,PotRadVar.s="PotRad_NEW"		##<< Variable name of potential radiation (W/m2)			   
		,Suffix.s = ""		   			##<< string inserted into column names before identifier for NEE column defaults (see \code{\link{sMDSGapFillAfterUstar}}).
		,controlGLPart=partGLControl()	##<< further default parameters, see \code{\link{partGLControl}}
){	
	# Check if specified columns exist in sDATA or sTEMP and if numeric and plausible. Then apply quality flag
	SuffixDash.s <- paste( (if(fCheckValString(Suffix.s)) "_" else ""), Suffix.s, sep="")
	'Partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco) using Lasslop et al., 2010'
	# Check if specified columns exist in sDATA or sTEMP and if numeric and plausible. Then apply quality flag
	fCheckColNames(ds, c(NEEVar.s, QFNEEVar.s, TempVar.s, QFTempVar.s, RadVar.s, PotRadVar.s, NEESdVar.s), 'sGLFluxPartition')
	fCheckColNum(ds, c(NEEVar.s, QFNEEVar.s, TempVar.s, QFTempVar.s, RadVar.s, PotRadVar.s, NEESdVar.s), 'sGLFluxPartition')
	fCheckColPlausibility(ds, c(NEEVar.s, QFNEEVar.s, TempVar.s, QFTempVar.s, RadVar.s, PotRadVar.s), 'sGLFluxPartition')
	NEEFiltered <- fSetQF(ds, NEEVar.s, QFNEEVar.s, QFNEEValue.n, 'sGLFluxPartition')
	#
	# Apply quality flag for temperature and VPD
	# TODO: docu meteo filter, standard FALSE
	NEW_FP_Temp <- if( isTRUE(controlGLPart$isFilterMeteoQualityFlag) ) fSetQF(ds, TempVar.s, QFTempVar.s, QFTempValue.n, 'partGLExtractStandardData') else ds[[TempVar.s]]
	NEW_FP_VPD <- if( isTRUE(controlGLPart$isFilterMeteoQualityFlag) ) fSetQF(ds, VPDVar.s, QFVPDVar.s, QFVPDValue.n, 'partGLExtractStandardData') else ds[[VPDVar.s]]
	#
	# Filter night time values only
	#! Note: Rg <= 4 congruent with Lasslop et al., 2010 to define Night for the calculation of E_0.n
	# Should be unfilled (original) radiation variable, therefore dataframe set to sDATA only
	#! New code: PotRad in sGLFluxPartition: Slightly different subset than PV-Wave due to time zone correction (avoids timezone offset between Rg and PotRad)
	isNight <- (ds[,RadVar.s] <= 4 & ds[[PotRadVar.s]] == 0)
	# Filter day time values only
	#! Note: Rg > 4 congruent with Lasslop et al., 2010 to define Day for the calculation of paremeters of Light Response Curve 
	# Should be unfilled (original) radiation variable, therefore dataframe set to sDATA only, twutz does not understand this comment
	isDay=(ds[,RadVar.s] > 4 & ds[[PotRadVar.s]] != 0)
	#	
	##value<< a data.frame with columns
	dsR <- data.frame(
			sDateTime=ds[[1]]			##<< first column of ds, usually the time stamp
			## not used, but usually first column is a dateTime is kept for aiding debug
			,NEE=NEEFiltered			##<< NEE filtered for quality flay
			,sdNEE=ds[[NEESdVar.s]]		##<< standard deviation of NEE with missing values replaced 
			, Temp=NEW_FP_Temp			##<< Temperature, quality filtered if isTRUE(controlGLPart$isFilterMeteoQualityFlag)
			, VPD=NEW_FP_VPD			##<< Water pressure deficit, quality filtered if isTRUE(controlGLPart$isFilterMeteoQualityFlag)
			, Rg=ds[[RadVar.s]]			##<< Incoming radiation
			, isDay=isDay				##<< Flag that is true for daytime records
			, isNight=isNight			##<< Flag that is true for nighttime records
	)
	##end<<
	##details<<
	## The LRC fit ususally weights NEE records by its uncertainty. In order to also use
	## records with missing \code{NEESdVar.s}, uncertainty of the missing values is by default set 
	## to a conservatively high value, parameterized by \code{controlGLPart$replaceMissingSdNEEParms).
	## Controlled by argument \code{replaceMissingSdNEEParms} in \code{\link{partGLControl}}, but overruled
	## by argument \code{neglectNEEUncertaintyOnMissing}.
	if( !controlGLPart$neglectNEEUncertaintyOnMissing )
		dsR$sdNEE <- replaceMissingSdByPercentage(dsR$sdNEE, dsR$NEE
				, controlGLPart$replaceMissingSdNEEParms[1], controlGLPart$replaceMissingSdNEEParms[2] )
	dsR
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
	#E0_night equals E0, but uncertainty might differ
	resParms$summary$E0_bootstrap_sd <- resParms$summary$E0_sd		# due to bootstrap, this may differ, save before overriding by night-time estimate
	resParms$summary$E0 <- dsTempSens$E0				# report E0 and E0_sd from nighttime even when LRC fit did not converge
	resParms$summary$E0_sd <- dsTempSens$sdE0				
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
		### Estimate parameters of the Rectangular Hyperbolic Light Response Curve function (a,b,R_ref, k) for a single window
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
	##details<< Fitting is done on a subset of the data where isDay, NEE, sdNEE, Temp, Rg, and VPD are all non-NA 
	## and isDay is TRUE 
	isValidDayRecNoSdNEEConstraint <- !is.na(ds$isDay) & ds$isDay & !is.na(ds$NEE) & !is.na(ds$Temp) & !is.na(ds$Rg)
	##details<<
	## With option \code{controlGLPart$neglectNEEUncertaintyOnMissing=TRUE} all sdNEE are set to 1 if there is any missing sdNEE at otherwise valid records
	if( isTRUE(controlGLPart$neglectNEEUncertaintyOnMissing) && any(!is.finite(ds$sdNEE[isValidDayRecNoSdNEEConstraint])) ){
		ds$sdNEE <- 1L
	}
	isValidDayRecNoVPDConstraint <- isValidDayRecNoSdNEEConstraint & !is.na(ds$sdNEE) 
	##details<<
	## If  \code{controlGLPart$isNeglectVPDEffect=TRUE}, also records with VPD=NA maybe valid
	isValidDayRec <- if( isTRUE(controlGLPart$isNeglectVPDEffect) ) 
				isValidDayRecNoVPDConstraint else isValidDayRecNoVPDConstraint & !is.na(ds$VPD)
	iMeanRecInDayWindow <- as.integer(round(mean(which(isValidDayRec))))
	getNAResult <- function(convergenceCode){
			list(
				resOpt = NULL
				,summary = data.frame(
						nValidRec=sum(isValidDayRec)
						,iMeanRec=iMeanRecInDayWindow
						,convergence=convergenceCode
				)
				,isValid=FALSE
		)}
	##details<< If there are too few records (< \code{controlGLPart$minNRecInDayWindow}), then
	## the constraint on non-NA VPD is neglected and \code{controlGLPart$isNeglectVPDEffect} is set to TRUE
	## If there are still too few records, an NA-result with convergence code 1011L is returned.
	if( (sum(isValidDayRec) < controlGLPart$minNRecInDayWindow) ){
		controlGLPart$isNeglectVPDEffect <- TRUE
		isValidDayRec <- isValidDayRecNoVPDConstraint
		iMeanRecInDayWindow <- as.integer(round(mean(which(isValidDayRec))))
		if( (sum(isValidDayRec) < controlGLPart$minNRecInDayWindow) ) return(getNAResult(1011L)) 
	} 
	dsDay <- ds[isValidDayRec,]
	##details<<
	## Each window estimate is associated with a time or equivalently with a record.
	## The first record, i.e. row number, of the day-window is reported.
	## Moreover, the mean of all valid records numbers in the daytime window is reported for interpolation.
	#better report NA and care for it properly: if( is.na(iMeanRecInDayWindow)) iMeanRecInDayWindow <- as.integer(nrow(ds)%/%2)
	#TODO firstRecInDayWindow.i <- which(SubsetDayPeriod.b)[1] # the rownumber of the first record inside the day window
	##seealso<< \code{\link{partGLEstimateTempSensInBoundsE0Only}}
	E0 <- E0Win$E0[winInfo$iWindow]
	# if too few records or 
	# if no temperature-respiration relationship could be found, indicate no-fit, but report Window properties
	if( is.na(E0)  ) return(getNAResult(1010L))
	# if( DayStart.i > 72 ) recover()		
	sdE0 <- E0Win$sdE0[winInfo$iWindow]
	RRefNight <- E0Win$RRef[winInfo$iWindow]
	#
	##seealso<< \code{\link{LightResponseCurveFitter_fitLRC}}
	#lrcFitter <- RectangularLRCFitter()
	resOpt <- resOpt0 <- lrcFitter$fitLRC(dsDay, E0=E0, sdE0=sdE0, RRefNight=RRefNight
			, controlGLPart=controlGLPart, lastGoodParameters=prevRes$resOpt$thetaOpt
	)
	if( !is.finite(resOpt$thetaOpt[1]) ) {
		return(getNAResult(resOpt$convergence))
	}
	sdTheta <- resOpt$thetaOpt; sdTheta[] <- NA
	sdTheta[resOpt$iOpt] <- sqrt(diag(resOpt$covParms)[resOpt$iOpt])
	#
	# record valid fits results
	#as.data.frame(t(resOpt$thetaOpt))
#if( as.POSIXlt(dsDay$sDateTime[1])$mday+2L >= 11 ) recover()
#if( as.POSIXlt(dsDay$sDateTime[1])$mday+2L >= 27 ) recover()
# save(dsDay, file="tmp/dsDayDebug.RData")
ans <- list(
		 resOpt=resOpt
		 ,summary = cbind(data.frame(
			nValidRec=nrow(dsDay)
			,iMeanRec=iMeanRecInDayWindow
			,convergence=resOpt$convergence
			,parms_out_range=as.integer( !all(1:5 %in% resOpt$iOpt) )
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
		#should be dealt with iOpt ,isNeglectVPD=FALSE	##<< set to TRUE to neglect VPD effect
){
	##value<<
	## Matrix with each row a parameter estimate on a different bootstrap sample.
	## Some of the rows might be NA, if the subsampled dataset could not be fitted.
	ans <-matrix(NA, nrow=controlGLPart$nBootUncertainty, ncol=length(theta0), dimnames=list(NULL,names(theta0)))
	##details<<
	## In addition to resampling the original data, also the temperature sensitivity is resampled 
	## from its uncertainty distribution but bounded in between 50 to 400.
	E0r <- rnorm( controlGLPart$nBootUncertainty, theta0[5L], sdE_0.n	)
	E0 <- pmax(50,pmin(400,E0r))
	theta <- theta0
	#iBoot <- 1L
	for (iBoot in c(1:controlGLPart$nBootUncertainty)){
		idx <- sample(nrow(dsDay), replace=TRUE)
		dsDayB <- dsDay[idx,]
		theta[iPosE0] <- E0[iBoot]
		resOptBoot <- lrcFitter$optimLRCOnAdjustedPrior(theta, iOpt=iOpt, dsDay=dsDayB
			, parameterPrior=parameterPrior, ctrl=controlGLPart)
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
		Rg   	##<< numeric vector of photosynthetic flux density [umol/m2/s] or Global Radiation
		,VPD 	##<< numeric vector of Vapor Pressure Deficit [hPa]
		,Temp 	##<< numeric vector of Temperature [degC] 
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
		# they all will be based on the same valid data, if iMeanRec is equal
	    tabMeanRec <- table(summaryLRC$iMeanRec) 
	    iRecsDouble <- as.integer(names(tabMeanRec[ tabMeanRec > 1L ]))
	    iRecsOmit <- do.call(c, lapply( iRecsDouble, function(iRecDouble){
	      which(summaryLRC$iMeanRec==iRecDouble)[-1L]
    }))
    if( length(iRecsOmit)) summaryLRC <- summaryLRC[-iRecsOmit,]
	}
	##details<<
	## Parameter estimates are reported for the central record of the window, or
	## if \code{isTRUE(controlGLPart.l$isAssociateParmsToMeanOfValids)} for the mean time of all valid records within the window
	# for each original record merge parameters assicated with previous fit or next fit respectively
	colNameAssoc <- if( isTRUE(controlGLPart$isAssociateParmsToMeanOfValids) ) "iMeanRec" else "iCentralRec" 
	dsAssoc <- .partGPAssociateSpecialRows(summaryLRC[[colNameAssoc]],nRec)
	# now we have columns iBefore and iAfter, which can be used to merge the parameter estimate columns to each row
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

computeAggregatedCovariance <- function(
	### compute the sum of covariances between Reco and GPP predictions that are due to being based on the same uncertaint model coefficients
	dsPred	##<< data.frame with predictors (Rg, VPD, Temp)
	#dsBefore	##<< data.frame with predictors (Rg, VPD, Temp) and   
	#,dsAfter	##<< data.frame with predictors () and parameters of subsequent estimate
	,resParms	##<< data frame with results of \code{\link{partGLFitLRCWindows}} of fitting the light-response-curve for several windows
	,controlGLPart=partGLControl()	##<< further default parameters, see \code{\link{partGLControl}} with entry "isAssociateParmsToMeanOfValids"
	,lrcFitter	##<< R5 class instance responsible for fitting the light response curve, with method getParameterNames()
	,iAggregate=1:nrow(dsPred)	##<< row indices about which to sum over, must be contiguous
){
	sumCovGPP <- 0
	sumCovReco <- 0
	# merge parameters to predictors
	isValidWin <- is.finite(resParms$summary$parms_out_range)
	summaryLRC <- resParms$summary[ isValidWin, ,drop=FALSE]
	resOptList <- resParms$resOptList[isValidWin]
	colNameAssoc <- if( isTRUE(controlGLPart$isAssociateParmsToMeanOfValids) ) "iMeanRec" else "iCentralRec" 
	dsAssoc <- .partGPAssociateSpecialRows(summaryLRC[[colNameAssoc]],nRec)
	# now we have columns iBefore and iAfter, which can be used to merge the parameter estimate columns to each row
	parNames <- lrcFitter$getParameterNames()
	dsBefore <- merge( 
			structure(data.frame(dsAssoc$iSpecialBefore, dsAssoc$iBefore),names=c("iParRec",colNameAssoc))
			, summaryLRC[,c(colNameAssoc, parNames)]
	)
	dsAfter  <- merge( structure(data.frame(dsAssoc$iSpecialAfter, dsAssoc$iAfter),names=c("iParRec",colNameAssoc)), summaryLRC[,c(colNameAssoc,lrcFitter$getParameterNames())])
	#
	# compute gradients (each entry is a list with components vectors Reco and GPP
	grad2 <- lapply( list(dsBefore,dsAfter), function(dsi){
				theta <- as.matrix(dsi[,parNames])
				grad <- lrcFitter$computeLRCGradient(theta, dsPred$Rg, dsPred$VPD, pmax(-40,dsPred$Temp))
			})
	for( iRec in iAggregate[-1]){    # in first line only entry with i==j
		iParRecBefore <- dsAssoc$iSpecialBefore[iRec]
		resOptBefore <- resOptList[[ iParRecBefore ]]
		iParRecAfter <- dsAssoc$iSpecialAfter[iRec]
		resOptAfter <- resOptList[[ iParRecAfter ]]
		gradGPPIBefore <- grad[[1]]$GPP[iRec,]
		gradRecoIBefore <- grad[[1]]$Reco[iRec,]			  
		gradGPPIAfter <- grad[[2]]$GPP[iRec,]
		gradRecoIAfter <- grad[[2]]$Reco[iRec,]			  
		for( jRec in iAggregate[1]:(iRec-1) ){ # iterate from start to one before iRec
			# assume covariance larger than zero only in same interval between parameter estimates
			if( (dsAssoc$iSpecialBefore[jRec] == iParRecBefore) &&
				(dsAssoc$iSpecialAfter[jRec] == iParRecAfter)
			){ 
				gradGPPJBefore <- grad[[1]]$GPP[jRec,]
				gradRecoJBefore <- grad[[1]]$Reco[jRec,]			  
				gradGPPJAfter <- grad[[2]]$GPP[jRec,]
				gradRecoJAfter <- grad[[2]]$Reco[jRec,]			  
				covRecoBefore <- gradRecoIBefore %*% resOptBefore$covParms[names(gradRecoIBefore),names(gradRecoIBefore)] %*% gradRecoJBefore
				covGPPBefore <-  gradGPPIBefore %*% resOptBefore$covParms[names(gradGPPIBefore),names(gradGPPIBefore)] %*% gradGPPJBefore
				covRecoAfter <- gradRecoIAfter %*% resOptAfter$covParms[names(gradRecoIAfter),names(gradRecoIAfter)] %*% gradRecoJAfter
				covGPPBAfter <-  gradGPPIAfter %*% resOptAfter$covParms[names(gradGPPIAfter),names(gradGPPIAfter)] %*% gradGPPJAfter
				covRecoMix <- dsAssoc$wBefore[iRec]*dsAssoc$wBefore[jRec]*covRecoBefore + dsAssoc$wAfter[iRec]*dsAssoc$wAfter[jRec]*covRecoAfter 
				covGPPMix <- dsAssoc$wBefore[iRec]*dsAssoc$wBefore[jRec]*covGPPBefore + dsAssoc$wAfter[iRec]*dsAssoc$wAfter[jRec]*covGPPAfter
				sumCovReco <- sumCovReco + 2*covRecoMix  # matrix symmetrical, add also from transposed
				sumCovGPP <- sumCovGPP + 2*covGPPMix
			} # match of iParRecBefore and iParRecAfter
		} # for jRec
	} # for iRec
	##value<< named numeri vector with two components: 
	ans <- c( sumCovGPP = sumCovGPP		##<< sum_{i<>j} cov(GPP_i,GPP_j)
			, sumCovReco = sumCovReco	##<< sum_{i<>j} cov(Reco_i,Reco_j)
			)
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


replaceMissingSdByPercentage <- function(
	### replace missing standard deviation of a measure x by a percentage of x
	sdX		##<< numeric vector: with missing to be repalce
	,x		##<< numeric vector of length(sdX): value form which percentage is computed
	,perc=0.2	##<< numeric scalar: sdX = perc*x
	,minSdX=0.7	##<< numeric scalar: minimum of sdX to be applied for low x
){
	##details<<
	## If either perc or inSdX is NA then only the other criterion is applied.
	## If both are NA then all missings are set to NA.
	## \code{sdX[iToFill] <- pmax(minSdX, abs(x[iToFill]*perc), na.rm=TRUE)}
	iToFill <- !is.finite(sdX) 
	sdX[iToFill] <- pmax(minSdX, abs(x[iToFill]*perc), na.rm=TRUE)
	##value<< sdX with non-finite values replaced.
	sdX
}

