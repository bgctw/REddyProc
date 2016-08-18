partGLControl <- function(
		### Default list of parameters for Lasslop 2010 daytime flux partitioning
		LRCFitConvergenceTolerance=1e-3	##<< convergence criterion for LRC fit. 
			##<< If relative improvement of reducing residual sum of squares between predictions and observations is less than this criterion, assume convergence.
			##<< Decrease to get more precise parameter estimates, Increase for speedup.
		#TODO: increase default of nBoot after testing, option to use Hessian
		,nBootUncertainty=10L			##<< number of bootstrap samples for estimating uncertainty. Set to zero to derive uncertainty from curvature of a single fit
		,minNRecInDayWindow = 2L 		##<< Minimum number of data points for regression #TODO CHECK IN GITTA'S CODE MINIMUM NUMBERS OF DAYS
		,isAssociateParmsToMeanOfValids=FALSE	##<< set to TRUE to associate parameters to record of  mean across non-NA records inside a window instead of the central records of the window
){
	##author<< TW
	ctrl <- list(  
			LRCFitConvergenceTolerance=LRCFitConvergenceTolerance
			,nBootUncertainty=nBootUncertainty
			,minNRecInDayWindow=minNRecInDayWindow 
			,isAssociateParmsToMeanOfValids=isAssociateParmsToMeanOfValids
	)
	#display warning message for the following variables that we advise not to be changed
	#if (corrCheck != 0.5) warning("WARNING: parameter corrCheck set to non default value!")
	ctrl
}
attr(partGLControl,"ex") <- function(){
	partGLControl()
}

parGLPartitionFluxes=function(
		### sGLFluxPartition - Flux partitioning after Lasslop et al. (2010)
		##description<<
		## Nighttime-based partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco)
		ds						##<< dataset with all the specified input columns and full days in equidistant times		
		,FluxVar.s=paste0('NEE',SuffixDash.s,'_f')       ##<< Variable of net ecosystem fluxes
		,QFFluxVar.s=paste0('NEE',SuffixDash.s,'_fqc')  ##<< Quality flag of variable
		,QFFluxValue.n=0         						##<< Value of quality flag for _good_ (original) data
		,FluxSdVar.s=paste0('NEE',SuffixDash.s,'_fsd')       ##<< Variable of standard deviation of net ecosystem fluxes
		,TempVar.s=paste0('Tair',SuffixDash.s,'_f')     ##<< Filled air or soil temperature variable (degC)
		,QFTempVar.s=paste0('Tair',SuffixDash.s,'_fqc') ##<< Quality flag of filled temperature variable
		,QFTempValue.n=0       ##<< Value of temperature quality flag for _good_ (original) data
		,RadVar.s='Rg'         ##<< Unfilled (original) radiation variable
		,VPDVar.s=paste0('VPD',SuffixDash.s,'_f')     ##<< Filled Vapor Pressure Deficit - VPD - (hPa)
		,QFVPDVar.s=paste0('VPD',SuffixDash.s,'_fqc') ##<< Quality flag of filled VPD variable    
		,QFVPDValue.n=0        ##<< Value of VPD quality flag for _good_ (original) data
		,PotRadVar.s="PotRad_NEW"	##<< Variable name of potential radiation (W/m2)			   
		,Suffix.s = ""		   ##<< string inserted into column names before identifier (see \code{\link{sMDSGapFillUStar}}).
		,debug.l=list(		   ##<< list with debugging control, see \code{\link{sRegrE0fromShortTerm}}.
				##describe<< 
				useLocaltime.b=FALSE	##<< by default corrects hour (given in local winter time) for latitude to solar time
		##<< where noon is exactly at 12:00. Set this to TRUE to compare to code that uses local winter time
		##end<< 
		)      
		,controlGLPart.l=partGLControl()	##<< further default parameters, see \code{\link{partGLControl}}
		,isVerbose=TRUE			 ##<< set to FALSE to suppress output messages
		,nRecInDay.i=48L		 ##<< number of records within one day (for half-hourly data its 48)
)
##author<<
## MM, TW
##references<<
## Lasslop G, Reichstein M, Papale D, et al. (2010) Separation of net ecosystem exchange into assimilation and respiration using 
## a light response curve approach: critical issues and global evaluation. Global Change Biology, Volume 16, Issue 1, Pages 187–208
{
	'Partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco) using Lasslop et al., 2010'
	# Check if specified columns exist in sDATA or sTEMP and if numeric and plausible. Then apply quality flag
	SuffixDash.s <- paste( (if(fCheckValString(Suffix.s)) "_" else ""), Suffix.s, sep="")
	'Partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco) using Lasslop et al., 2010'
	# Check if specified columns exist in sDATA or sTEMP and if numeric and plausible. Then apply quality flag
	fCheckColNames(ds, c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s, PotRadVar.s, FluxSdVar.s), 'sGLFluxPartition')
	fCheckColNum(ds, c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s, PotRadVar.s, FluxSdVar.s), 'sGLFluxPartition')
	fCheckColPlausibility(ds, c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s, PotRadVar.s), 'sGLFluxPartition')
	Var.V.n <- fSetQF(ds, FluxVar.s, QFFluxVar.s, QFFluxValue.n, 'sGLFluxPartition')
	if( isVerbose ) message('Start daytime flux partitioning for variable ', FluxVar.s, ' with temperature ', TempVar.s, '.')
	##value<< data.frame with columns
	## Reco_DT_<suffix>: predicted ecosystem respiraiton: mumol CO2/m2/second
	## GPP_DT_<suffix>: predicted gross primary production mumol CO2/m2/second
	## Light response curve parameters are estimated for windows, and are reported for the first record of the central day of the window
	REcoDTVar.s <- paste0('Reco_DT',SuffixDash.s) 
	GPPDTVar.s <- paste0('GPP_DT',SuffixDash.s) 
	dsAns <- data.frame(
			FP_VARnight=rep(NA_real_,nrow(ds))	##<< NEE filtered for nighttime records (others NA)
			,FP_VARday=NA_real_		##<< NEE filtered for daytime recores (others NA)
			,NEW_FP_Temp=NA_real_	##<< temperature after filtering for quality flag degree Celsius
			,NEW_FP_VPD=NA_real_	##<< vapour pressure deficit after filtering for quality flag, hPa
			,FP_E0=NA_real_			##<< temperature sensitivity estimated from nighttime NEE window  in Kelvin (degK) 
			,FP_R_ref=NA_real_		##<< basal respiration estimated from LRC of daytime window  (W/m2)
		#TODO Mirco: add descriptions to meaning of LRC parameters
		#TODO: report uncertainties of parameters?
		#TODO: old tool file also reports SE_GPP_HBLR
			,FP_alpha=NA_real_			##<< 
			,FP_beta=NA_real_			##<< 
			,FP_k=NA_real_				##<< 
	)
	##end<<
	# Filter night time values only
	#! Note: Rg <= 4 congruent with Lasslop et al., 2010 to define Night for the calculation of E0
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
	dsAns$NEW_FP_Temp <- fSetQF(ds, TempVar.s, QFTempVar.s, QFTempValue.n, 'sGLFluxPartition')
	dsAns$NEW_FP_VPD <- fSetQF(ds, VPDVar.s, QFVPDVar.s, QFVPDValue.n, 'sGLFluxPartition')
	#Estimate Parameters of light response curve: R_ref, alpha, beta and k according to Table A1 (Lasslop et al., 2010)
	# save(ds, file="tmp/dsTestPartitioningLasslop10.RData")
	# extract the relevant columns in df with defined names (instead of passing many variables)
	dsR <- data.frame(
			NEE=Var.V.n
			,sdNEE=ds[[FluxSdVar.s]]
			, Temp=dsAns$NEW_FP_Temp
			, VPD=dsAns$NEW_FP_VPD
			, Rg=ds[[RadVar.s]]
			, isDay=isDay
			, isNight=isNight
	) 
	resLRC <- tmp <- partGLFitLRCWindows( dsR
			, nRecInDay=nRecInDay.i
			, controlGLPart.l=controlGLPart.l
	)
	#dput(resLRC)
	# append good parameter fits to the first record of day window
	iGood <- which(resLRC$parms_out_range == 0L)	 	
	dsAns[resLRC$iFirstRec[iGood],c("FP_E0","FP_R_ref","FP_alpha","FP_beta","FP_k")] <- resLRC[iGood,c("E_0","R_ref","a","b","k")]
	#	
	dsAnsFluxes <- partGLInterpolateFluxes( ds[,RadVar.s], dsAns$NEW_FP_VPD, dsAns$NEW_FP_Temp, resLRC, controlGLPart.l=controlGLPart.l	)
	dsAns[[REcoDTVar.s]] <- dsAnsFluxes$Reco
	attr(dsAns[[REcoDTVar.s]], 'varnames') <- REcoDTVar.s
	attr(dsAns[[REcoDTVar.s]], 'units') <- attr(Var.V.n, 'units')
	dsAns[[GPPDTVar.s]] <- dsAnsFluxes$GPP
	attr(dsAns[[GPPDTVar.s]], 'varnames') <- GPPDTVar.s
	attr(dsAns[[GPPDTVar.s]], 'units') <- attr(Var.V.n, 'units')
	#sTEMP$GPP_DT_fqc <<- cbind(sDATA,sTEMP)[,QFFluxVar.s]
	#! New code: MDS gap filling information are not copied from NEE_fmet and NEE_fwin to GPP_fmet and GPP_fwin
	#           (since not known within this pure partitioning function)
	return(dsAns)
}



partGLFitLRCWindows=function(
		### estimateLRCParms - Estimation of the parameters of the Rectangular Hyperbolic Light Response Curve function (a,b,R_ref, k)
		ds					##<< data.frame with numeric columns NEE, sdNEE, Temp (degC), VPD, Rg, adn boolean columns isNight, isDay
		,WinSizeDays.i=4L	##<< Window size (days) for daytime fits
		,WinSizeNight.i=3L*WinSizeDays.i	##<< Window size (days) for nighttime fits  
		,DayStep.i=WinSizeDays.i %/% 2L     ##<< step in days for shifting the windows
		,isVerbose=TRUE		##<< set to FALSE to suppress messages
		,nRecInDay.i=48L	##<< number of records within one day (for half-hourly data its 48)
		,controlGLPart.l=partGLControl()	##<< list of further default parameters
){
	##author<<
	## MM, TW
	##description<<
	## Estimation of the parameters of the Rectangular Hyperbolic Light Response Curve function (a,b,R_ref, k) as in Lasslop et al., 2010 \code{\link{fLloydTaylor()}} for successive periods
	'Estimation of the reference respiration Rref of fLloydTaylor() for successive periods'
	requiredCols <- c("NEE", "sdNEE", "Temp", "VPD", "Rg", "isNight", "isDay")
	iMissing <- which( is.na(match( requiredCols, names(ds) )))
	if( length(iMissing) ) stop("missing columns: ",paste0(requiredCols[iMissing],collapse=","))
	# Regression settings
	LMRes.F <- data.frame(NULL) #Results of linear regression
	# Loop regression periods
	#DayCounter.V.i <- c(1:sINFO$DIMS) %/% sINFO$DTS #twutz: do not rely on sINFO here, also a -1 is missing 
	##details<<
	## All the vectors must have the same length and consist of entire days, with each day having the same number of records
	nRec <- length(ds$NEE) 
	DayCounter.V.i <- ((c(1:nRec)-1L) %/% nRecInDay.i)+1L 	# specifying the day for each record assuming equidistand records
	startDays.V.i <- seq(1, max(DayCounter.V.i), DayStep.i)
	# setup a data.frame for the results
	##value<< data.frame with a row for each window with columns
	resDf <- data.frame(
			Start=startDays.V.i	##<< the starting day of the day-window
			, End=NA_integer_		##<< the endding day of the day-window  
			, Num=NA_integer_		##<< the number of records flux records in the window
			, iMeanRec=NA_integer_	##<< the mean across all valid record numbers in the day-window
			, iCentralRec=NA_integer_	##<< the record in the centre of the day-window (including NA-records)
			, iFirstRec=NA_integer_	##<< the first record number in the day-window
			, E_0=NA_real_			##<< temperature sensitivty
			, E_0_SD=NA_real_		##<< standard deviation of temperature sensitivity
			, R_ref12=NA_real_		##<< respiration at reference temperature estimated from night time data
			, R_ref=NA_real_		##<< respiration at reference temperature
			, R_ref_SD=NA_real_		##< standard deviation of reference temperature
			,a=NA_real_, a_SD=NA_real_	##<< coefficients and associated standard deviation of the light respons curve
			,b=NA_real_, b_SD=NA_real_
			,k=NA_real_, k_SD=NA_real_
			,parms_out_range=NA_integer_	##<< zero if it was a good parameter estimate, 1 if could not fit or fell outside ranges
			#,isGoodParameterSet=FALSE	##<< TRUE if the fit was successful
			)
	lastGoodParameters.V.n <- rep(NA_real_, 5)		# indicate no good parameter set found yet
	E_0.n <- R_refNight.n <- NA		# there are no previous estimates yet
	CountRegr.i <- 0L
	#iDay<-1L
	for (iDay in seq_along(startDays.V.i)) {   #not sure is correct to me should be
		DayStart.i <- startDays.V.i[iDay]
		#DayMiddle.i <- DayStart.i-1L + WinSizeDays.i/2
		if( isVerbose ) message(",",DayStart.i, appendLF = FALSE)
		DayEnd.i <- DayStart.i-1L+WinSizeDays.i		#-1: imagine a windows size of 1, then end day would be the same day
		DayStart.Night.i <- DayStart.i + WinSizeDays.i/2 - WinSizeNight.i/2 
		DayEnd.Night.i <- DayStart.Night.i-1L+WinSizeNight.i
		#c(DayStart.i, DayEnd.i, DayStart.Night.i, DayEnd.Night.i)
		SubsetDayPeriod.b <- DayCounter.V.i >= DayStart.i & DayCounter.V.i <= DayEnd.i # could be done faster with direct row-computation but so its more clear 
		SubsetValidDay.b <-  SubsetDayPeriod.b & 
				ds$isDay & !is.na(ds$NEE) & !is.na(ds$Temp) & !is.na(ds$VPD)
		SubsetValidNight.b <- DayCounter.V.i >= DayStart.Night.i & DayCounter.V.i <= DayEnd.Night.i & 
				ds$isNight & !is.na(ds$NEE)
		dsNight <- ds[SubsetValidNight.b,]
		dsDay <- ds[SubsetValidDay.b,]
		##details<<
		## Each window estimate is associated with a time or equivalently with a record.
		## The first record, i.e. row number, of the day-window is reported.
		## Moreover, the mean of all valid records numbers in the daytime window is reported for interpolation.
		meanRecInDayWindow.i <- as.integer(round(mean(which(SubsetValidDay.b)))) 
		firstRecInDayWindow.i <- which(SubsetDayPeriod.b)[1] # the rownumber of the first record inside the day window
		#NEENightInPeriod.V.n <- subset(ds$NEE, SubsetValidNight.b)
		#NEESdNightInPeriod.V.n <- subset(NEESd.V.n, Subset.Night.b)
		#NEEDayInPeriod.V.n <- subset(ds$NEEDay.V.n, SubsetValidDay.b)
		#NEESdDayInPeriod.V.n <- subset(NEESd.V.n, SubsetValidDay.b)
		# 
		#TempInPeriod.V.n <- subset(Temp.V.n, SubsetValidDay.b)
		#TempInNightPeriod.V.n <- subset(Temp.V.n, SubsetValidNight.b)
		#TempInPeriod_degK.V.n <- fConvertCtoK(TempInPeriod.V.n)
		#TempInNightPeriod_degK.V.n <- fConvertCtoK(TempInNightPeriod.V.n)
		#VPDInPeriod.V.n <- subset(ds$VPD, SubsetValidDay.b)
		#RgInPeriod.V.n  <- subset(Rg.V.n, SubsetValidDay.b)
		#		
		if( nrow(dsDay) > controlGLPart.l$minNRecInDayWindow ) {
			CountRegr.i <- CountRegr.i+1L
			resNightFit <- partGLEstimateTempSensInBounds(dsNight$NEE, fConvertCtoK(dsNight$Temp), prevE0=E_0.n, prevR_ref=R_refNight.n)
			E_0.n <- resNightFit$E_0
			R_refNight.n <- resNightFit$R_ref
			#
			#tryCatch({
	#if( DayMiddle.i == 113) recover()
			resOpt <- resOpt0 <- partGLFitLRC(dsDay, dsNight$NEE, E_0.n=E_0.n, R_refNight.n=R_refNight.n, controlGLPart.l=controlGLPart.l)
			.tmp.plot <- function(){
				plot( -NEE ~ Rg, dsDay )
				tmp <- partRHLightResponse(resOpt$opt.parms.V, RgInPeriod.V.n, VPDInPeriod.V.n, NEEDayInPeriod.V.n, 1, TempInPeriod.V.n,  E_0.n)
				lines(  tmp ~ RgInPeriod.V.n )
			}
			#
			#isFirstDay <- (DayMiddle.i == (WinDays.i+1))
			# on first day, set last good parameter set to the initial guess from before the optimization
			if( is.na(lastGoodParameters.V.n[1]) ) lastGoodParameters.V.n <- resOpt$initialGuess.parms.V.n	
			resOptBounded <- partGLBoundParameters( resOpt, lastGoodParameters.V.n)
			if( resOptBounded$isGoodParameterSet )
				lastGoodParameters.V.n <- resOptBounded$opt.parms.V
			if( isTRUE(as.vector(resOptBounded$opt.parms.V["Rb"] < 0))) stop("encountered negative basal respiration")			
			#
			# twutz: avoid slow rbind, but write into existing data.frame
			resDf[iDay, ] <- data.frame(
					Start=DayStart.i, End=DayEnd.i, Num=nrow(dsDay)
					,iMeanRec=meanRecInDayWindow.i
					,iCentralRec=NA_integer_	# faster to compute outside the loop
					,iFirstRec=firstRecInDayWindow.i
					,E_0=E_0.n, E_0_SD=resNightFit$E_0_SD   
					,R_refNight=R_refNight.n
					,R_ref=resOptBounded$opt.parms.V[4], R_ref_SD=resOptBounded$se.parms.V[4],
					a=resOptBounded$opt.parms.V[3], a_SD=resOptBounded$se.parms.V[3],
					b=resOptBounded$opt.parms.V[2], b_SD=resOptBounded$se.parms.V[2],
					k=resOptBounded$opt.parms.V[1], k_SD=resOptBounded$se.parms.V[1],
					parms_out_range=as.integer(!resOptBounded$isGoodParameterSet)
					#isGoodParameterSet=resOptBounded$isGoodParameterSet
				)
		} # if enough records
		#! Note on PV-Wave code: trimmed linear regression not used in the end, i.e. in online webtool
	} # for i in days
	if( isVerbose ) message("") # LineFeed
	# central record in each window
	resDf$iCentralRec <- ((startDays.V.i-1L)+WinSizeDays.i/2)*nRecInDay.i	# assuming equidistant records
	# last window might be shorter, take the average between start record and end of all records
	resDf$iCentralRec[nrow(resDf)] <- ((resDf$Start[nrow(resDf)]-1L)*nRecInDay.i + nrow(dss))/2	    
	# remove those rows where there was not enough data
	OPTRes.LRC <- resDf[!is.na(resDf$End),]
#	#! New code: Omit regressions with R_ref <0, in PV-Wave smaller values are set to 0.000001, not mentioned in paper
#	#TODO later: Flag for long distances between R_refs, especially if long distance in the beginning - twutz: may make use of resDf$End == NA 
#	#TODO later: Provide some kind of uncertainty estimate from R_ref_SD
}

partGLEstimateTempSensInBounds <- function(
		### Estimate temperature sensitivity E_0 of Reco, and apply bounds or previous estimate
		REco.V.n	##<< numeric vector: night time NEE, i.e. ecosytem respiration
		,temperatureKelvin.V.n		##<< temperature in K
		,prevE0	= NA				##<< numeric scalar: the previous guess of Temperature Sensitivity
		,prevR_ref= NA				##<< nuermic scalar: previous guess of 
){
	#twutz: using nls to avoid additional package dependency
	#resFitLM <- NLS.L <- nlsLM(formula=R_eco ~ fLloydTaylor(R_ref, E_0, Temp, T_ref.n=273.15+15), algorithm='default', trace=FALSE,
	#		data=as.data.frame(cbind(R_eco=REco.V.n,Temp=temperatureKelvin.V.n)), start=list(R_ref=mean(REco.V.n,na.rm=TRUE),E_0=100)
	#		,control=nls.lm.control(maxiter = 20))
	resFit <- nls(formula=R_eco ~ fLloydTaylor(R_ref, E_0, Temp, T_ref.n=273.15+15), algorithm='default', trace=FALSE,
			data=as.data.frame(cbind(R_eco=REco.V.n,Temp=temperatureKelvin.V.n))
			, start=list(R_ref=as.vector({if(is.finite(prevR_ref)) prevR_ref else mean(REco.V.n,na.rm=TRUE)})
						,E_0=as.vector({if(is.finite(prevE0)) prevE0 else 100}))
			,control=nls.control(maxiter = 20))
	E_0Bounded.V.n <- E_0.V.n <- coef(resFit)['E_0']
	R_ref <- R_ref0 <- coef(resFit)['R_ref'] 
	E_0_SD.V.n <- coef(summary(resFit))['E_0',2]
	# resFit$convInfo$isConv
	if( (E_0.V.n < 50) || (E_0.V.n > 400)){
		E_0Bounded.V.n <- if( is.na(prevE0) ){
					min(400,max(50,E_0.V.n))
				} else {
					prevE0
				}
		R_ref <- mean(REco.V.n, na.rm=T)
	}
	##value<< list with entries
	list(
			E_0=E_0Bounded.V.n		##<< numeric scalar of estimated temperature sensitivty E0 bounded to [50,400]
			,E_0_SD=E_0_SD.V.n		##<< numeric scalar of standard deviation of E0
			,R_ref=R_ref			##<< numeric scalar of estimated respiration at reference temperature
			,resFit=resFit				##<< the fit-object
	)
}

partGLFitLRC <- function(
		### optimization of light respons curve against data in one window for three different initial parameter sets
		dsDay				##<< data.frame with columns NEEDay, Rg, Temp_C, VPD
		, NEENight.V.n		##<< non-na numeric vector of night time fluxes to estimate initial value of Rb
		, E_0.n				##<< temperature sensitivity of respiration
		, R_refNight.n		##<< basal respiration estimated from night time data 
		,controlGLPart.l=partGLControl()	##<< further default parameters (see \code{\link{partGLControl}})
){
	#Definition of initial guess theta, theta2 and theta3. Three initial guess vectors are defined according to Lasslop et al., 2010
	namesPars <- c("k","beta0", "alfa", "Rb" )
	nPar <- length(namesPars)
	theta.V.n<-matrix(NA, 3,nPar, dimnames=list(NULL,namesPars))
	theta.V.n[1,]<-c(0,
			as.numeric(abs(quantile(dsDay$NEE, 0.03)-quantile(dsDay$NEE, 0.97))),
			0.1,
			#mean(NEENight.V.n, na.rm=T)
			R_refNight.n
	)   #theta [numeric] -> parameter vector (theta[1]=kVPD, theta[2]-beta0, theta[3]=alfa, theta[4]=Rref)
	betaPrior <- theta.V.n[1,2]
	theta.V.n[2,]<-theta.V.n[1,]/2
	theta.V.n[3,]<-theta.V.n[1,]*2
	#twutz: alpha is quite well defined, so try not changing it too much
	theta.V.n[2,2] <- betaPrior*1.3
	theta.V.n[3,2] <- betaPrior*0.8
	#
	resOpt3 <- apply( theta.V.n, 1, function(theta0){
				resOpt <- resOpt0 <- .optimLRC(theta0, isUsingFixedVPD=FALSE, dsDay
						, E_0.n, betaPrior, controlGLPart.l )
				# IF kVPD parameter less or equal zero then estimate the parameters withouth VPD effect
				if (resOpt$par[1] <= 0) resOpt <- .optimLRC(resOpt0$par, isUsingFixedVPD=TRUE, dsDay
							, E_0.n, betaPrior, controlGLPart.l )
				resOpt
			} )
	.tmp.plot <- function(){
		plot(dsDay$Rg,-1*dsDay$NEE)
		thetaB <- resOpt$par
		#thetaB <-  opt[[iBest <- which.min(optSSE)]]	
		points(Rg.V.n, tmp <- partRHLightResponse(thetaB,
						Rg = Rg.V.n, 
						Temp=Temp_C.V.n,
						VPD = ds$VPD,
						E0 = E_0.n,
						fixVPD = FALSE,
						)$NEP, col="green")
	}
	optSSE <- sapply(resOpt3, "[[", "value")
	if( sum(!is.na(optSSE)) == 0L ){
		# none of the intial fits yielded result
		opt.parms.V <- se.parms.V <- rep(NA_real_, npars)
		names(opt.parms.V) <- names(se.parms.V) <- colnames(theta.V.n)
	} else {
		resOpt <- resOpt3[[iBest <- which.min(optSSE)]] # select the one with the least cost
		opt.parms.V<-resOpt$par
		if(controlGLPart.l$nBootUncertainty == 0L) {
			seParmsHess <- seParmsHess0 <- sqrt(abs(diag(solve(resOpt$hessian))))
			if( length(seParmsHess) == 3L) seParmsHess <- c(k=0, seParmsHess0)
			se.parms.V <- seParmsHess
		} else {
			resBoot  <- .bootStrapLRCFit(resOpt$par, dsDay, E_0.n, betaPrior, controlGLPart.l)
			opt.parms.V <- apply(resBoot, 2, median, na.rm=TRUE)
			se.parms.V <- apply(resBoot, 2, sd, na.rm=TRUE)
		}
	}
	##value<< a list, If none of the optimizations from different starting conditions converged,
	## the parameters are NA
	ans <- list(
			opt.parms.V=opt.parms.V		##<< numeric vector of optimized parameters
			,se.parms.V=se.parms.V		##<< numeric vector of their standard deviation
			,initialGuess.parms.V.n=theta.V.n[1,]	##<< the initial guess
	)
}

.bootStrapLRCFit <- function(
		### Compute parameters uncertainty by bootstrap
		theta0, dsDay, E_0.n, betaPrior, controlGLPart.l
){
	##value<<
	## matrix with each row a parameter estimate on a different bootstrap sample
	ans <-matrix(NA, nrow=controlGLPart.l$nBootUncertainty, ncol=length(theta0), dimnames=list(NULL,names(theta0)))
	isUsingFixedVPD <- (theta0[1] <= 0) 
	#iBoot <- 1L
	for (iBoot in c(1:controlGLPart.l$nBootUncertainty)){
		idx <- sample(nrow(dsDay), replace=TRUE)
		dsDayB <- dsDay[idx,]
		resOptBoot <- .optimLRC(theta0, isUsingFixedVPD=isUsingFixedVPD, dsDayB
				, E_0.n, betaPrior, controlGLPart.l )
		if( resOptBoot$convergence == 0L )	
			#TODO: also remove the bery bad cases? 
			ans [iBoot,]<-resOptBoot$par
	}
	ans
	
}



.optimLRC <- function(
		###<< one fit of the light response curve
		theta, isUsingFixedVPD=FALSE, dsDay, E_0.n, betaPrior, ctrl
){
	# TODO: think about uncertainty,
	##details<<
	## Optimization of Light-response curve parameters takes into account the uncertainty of the flux values.
	## In order to avoid very strong leverage, values with a very low uncertainty (< 75% percentile) are assigned
	## the 75% percentile of the uncertainty.
	## This procedure downweighs records with a high uncertainty, but does not apply a large leverage for
	## records wiht a very low uncertainty.
	Fc_unc <- pmax( dsDay$sdNEE, quantile(dsDay$sdNEE, 0.75) ) #twutz: avoid excessive weights by small uncertainties (of 1/unc^2)
	##details<< 
	## The beta parameter is quite well defined. Hence use a prior with a standard deviation.
	## The specific results are sometimes a bit sensitive to the uncertainty of the beta prior. 
	## This uncertainty is set corresponding to 10 times the median relative flux uncertainty.
	## The prior is weighted n times the observations in the cost.
	## Hence, overall it is using a weight of 1/10 of the weight of all observations.
	#
	# different optimization funcitons are used depending on whether fitting 3 or 4 parameters
	# This is important for computing uncertainty from Hessian
	medianRelFluxUncertainty <- abs(median(Fc_unc/dsDay$NEE))
	sdBetaPrior <- 10*medianRelFluxUncertainty*betaPrior
	isUsingHessian <- (ctrl$nBootUncertainty==0L)
	if( isUsingFixedVPD){
		resOptim <- optim(theta[-1], .partGLRHLightResponseCost_NoVPD
				,flux = -dsDay$NEE 
				,sdFlux = Fc_unc	  
				,betaPrior = betaPrior
				,sdBetaPrior = sdBetaPrior
				,Rg = dsDay$Rg 
				,VPD = dsDay$VPD
				,Temp=dsDay$Temp
				,E0 = E_0.n
				,control=list(reltol=ctrl$LRCFitConvergenceTolerance)
				,method="BFGS", hessian=isUsingHessian)
		resOptim$par=c(k=0,resOptim$par)	# add VPD parameter again
		resOptim
	} else {
		tmp <- optim(theta, .partGLRHLightResponseCost 
				,flux = -dsDay$NEE 
				,sdFlux = Fc_unc	  
				,betaPrior = betaPrior
				,sdBetaPrior = sdBetaPrior
				,Rg = dsDay$Rg 
				,VPD = dsDay$VPD
				,Temp=dsDay$Temp
				,E0 = E_0.n
				,control=list(reltol=ctrl$LRCFitConvergenceTolerance)
				,method="BFGS", hessian=isUsingHessian)
	}
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
	,E0 = E_0.n
	)
}



partRHLightResponse <- function(
		###Rectungular Hyperbolic Light Response function: (Xu & Baldocchi, 2004; Falge et al., 2001; Lasslop et al., 2010)
		theta 	##<< theta [numeric] -> parameter vector (theta[1]=kVPD (k), theta[2]=beta0 (beta), theta[3]=alfa, theta[4]=Rref (rb))
		,Rg   	##<< ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation
		,VPD 	##<< VPD [numeric] -> Vapor Pressure Deficit [hPa]
		,Temp 	##<< Temp [degC] -> Temperature [degC] 
		,E0 	##<< Temperature sensitivity ("activation energy") in Kelvin (degK) #get("testparams", envir=environment(foo)
		,VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010
		,fixVPD = FALSE   	##<< fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered
) {
	##details<<
	## with fixes E0 for the estimation of Rd
	## VPD effect included according to Lasslop et al., 2010
	##details<<
	## If theta is a matrix, a different row of parameters is used for different entries of other inputs
	if( is.matrix(theta) ){
		kVPD<-theta[,1]
		beta0<-theta[,2]
		alfa<-theta[,3]
		Rref<-theta[,4]
	} else {
		kVPD<-theta[1]
		beta0<-theta[2]
		alfa<-theta[3]
		Rref<-theta[4]
	}
	Amax <- if( isTRUE(fixVPD) ) beta0 else {
				ifelse(VPD > VPD0, beta0*exp(-kVPD*(VPD-VPD0)), beta0)
			} 
	Reco<-Rref*exp(E0*(1/((273.15+10)-227.13)-1/(Temp+273.15-227.13)))
	GPP <- (Amax*alfa*Rg)/(alfa*Rg+Amax)
	NEP <- GPP - Reco
	## a data.frame of length of Rg of computed  
	ans <- list(
			NEP=NEP		##<< Net ecosystem production (-NEE), vector of length(Rg)
			,Reco=Reco	##<< Ecosystem respiration 
			,GPP=GPP	##<< Gross primary production
	)
}

.partGLRHLightResponseCost <- function(
		### Computing residual sum of sqares for predictions vs. data of NEE
		theta 		##<< theta [numeric] -> parameter vector with positions as in argument of \code{\link{partRHLightResponse}}
		,flux=NA 	##<< numeric: NEP (-NEE) or GPP time series [umolCO2/m2/s]
		,sdFlux=NA 	##<< numeric: standard deviation of Flux [umolCO2/m2/s]
		,betaPrior		##<< prior estimate of beta parameter (range of values)
		,sdBetaPrior	##<< standard deviation of betaPrior
		,...			##<< other arguments to \code{\link{partRHLightResponse}}
		,VPD0 = 10 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010
		,fixVPD = FALSE   	##<< fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered
		,useCVersion=TRUE	##<< set to FALSE  to use R instead of fast C version, e.g. for debugging
		# also tried to pass dsDay data.frame instead of all the variables separately, but accessing data.frame
		# columns in the cost function was a severe penalty (here double execution time)
) {
	if( useCVersion){
		RHLightResponseCostC(theta, flux, sdFlux, betaPrior, sdBetaPrior, ..., VPD0=VPD0, fixVPD=fixVPD)		
	} else {
		resPred <- partRHLightResponse(theta, ..., VPD0=VPD0, fixVPD=fixVPD)
		NEP_mod <- resPred$NEP
		misFitPrior <- (((theta[2] - betaPrior))/(sdBetaPrior))^2
		misFitObs <- sum(((NEP_mod-flux)/sdFlux)^2)
		RSS <- misFitObs + misFitPrior*length(flux)
		#if( !is.finite(RSS) ) recover()	# debugging the fit
		RSS
	}
}

.partGLRHLightResponseCost_NoVPD <- function(
		### calling \code{\link{fRHRF_VPDRdFit}} with a parameter vector that omits first component, which is assumed zero 
		theta	##<< parameter vector with first component omitted
		,...	##<< further parameters to \code{\link{fRHRF_VPDRdFit}}
){
	##details<<
	## necessary for fitting only a subset of parameters, with fixing the first parameter to zero
	.partGLRHLightResponseCost( c(0,theta), ..., fixVPD=TRUE)
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
	# E0  100;	50–400;	Set to value of previous window, if no previous window exists estimates <50 were set to 50, estimates >400 were set to 400
	# rb	Mean of nighttime NEE	>0;	Whole parameter set is not used
	# alpha	0.01;	?0,<0.22;	Set to value of previous window, if no previous window exists and <0, set to zero
	# beta0	Abs (0.03quantile – 0.97quantile) of NEE	?0; <250; If >100 then ? (?)<?	If negative set to zero, else the whole parameter set is not used
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
		### Predict REco and GPP by Light respons curve for two neighboring parameter sets and interpolate
		Rg   	##<< ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation
		,VPD 	##<< VPD [numeric] -> Vapor Pressure Deficit [hPa]
		,Temp 	##<< Temp [degC] -> Temperature [degC] 
		,resLRC	##<< data frame with results of \code{\link{partGLFitLRCWindows}} of fitting the light-response-curve for several windows
		,controlGLPart.l=partGLControl()	##<< further default parameters, see \code{\link{partGLControl}}
){
	##details<< 
	## \code{resLRC$iFirstRecInCentralDay} must denote the row for which the LRC parameters are representative, 
	## here, the first record of the center day
	# create a dataframe with index of rows of estimates before and after and correponding weights
	nLRC <- nrow(resLRC)
	nRec <- length(Rg) 
	Temp_Kelvin <- Temp+273.15
	# for each original record merge parameters assicated with previous fit or next fit respectively
	colNameAssoc <- if( isTRUE(controlGLPart.l$isAssociateParmsToMeanOfValids) ) "iMeanRec" else "iCentralRec" 
	dsAssoc <- .partGPAssociateSpecialRows(resLRC[[colNameAssoc]],nRec)
	dsBefore <- merge( structure(data.frame(dsAssoc$iBefore),names=colNameAssoc), resLRC[,c(colNameAssoc,"R_ref","E_0","a","b","k")])
	dsAfter <- merge( structure(data.frame(dsAssoc$iAfter),names=colNameAssoc), resLRC[,c(colNameAssoc,"R_ref","E_0","a","b","k")])
	if( (nrow(dsBefore) != nRec) || (nrow(dsAfter) != nRec)) stop("error in merging parameters to original records.")
	Reco2 <- lapply( list(dsBefore,dsAfter), function(dsi){
		tmp <- fLloydTaylor(dsi$R_ref, dsi$E_0, Temp_Kelvin, T_ref.n=273.15+15)
	})
	#dsi <- dsBefore
	GPP2 <- lapply( list(dsBefore,dsAfter), function(dsi){
				theta <- as.matrix(dsi[,c("k","b","a","R_ref")])
				tmp <- partRHLightResponse(theta, Rg, VPD, Temp=Temp, E0=dsi$E_0)$GPP
		})
	# interpolate between previous and next fit, weights already sum to 1
	Reco <- (dsAssoc$wBefore*Reco2[[1]] + dsAssoc$wAfter*Reco2[[2]]) #/ (dsAssoc$wBefore+dsAssoc$wAfter)  
	GPP <- (dsAssoc$wBefore*GPP2[[1]] + dsAssoc$wAfter*GPP2[[2]]) #/ (dsAssoc$wBefore+dsAssoc$wAfter)
#recover()  # to inspect the deviations between successive estimates	
	ans <- data.frame(
			Reco_DT = Reco
			,GPP_DT = GPP
	)
}

.tmp.plot <- function(){
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
	## When only for a subset of rows some more data available, this function creates
	## columns that refer to the previous and next row that are in the subset.
	## E.g. if some more data is available for rows 3 and 7, then rows 4:6 will indicate 
	## \code{iBefore=3, iAfter=7}.
	##value<< a dataframe with index of previous and next rows inside the subset
	ans <- data.frame(
			iRec=1:nRec				##<< the original row number
			, iBefore=NA_integer_	##<< index of the previous special row 
			, iAfter=NA_integer_	##<< index of the next special row
			, wBefore=NA_real_		##<< weight of the previous, inverse of the distance in records
			, wAfter=NA_real_)		##<< weight of the next, inverse of the distance in records
	##details<<
	## The subset rows inside the subset refer both (before and after) to the same subset rows, with weights 1/2
	nRecS <- length(iRowsSpecial)
	if( 0 == nRecS ) stop("cannot associate special rows, if length of argument iRowsSpecial is zero.")
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
			ans[(prevRec+1L):(currRec-1L),"iAfter"] <- currRec
			ans[(prevRec+1L):(currRec-1L),"wAfter"] <- (1:(distPrev-1))/distPrev  	 
		}
		distNext <- nextRec-currRec
		if( distNext > 1L){
			ans[(currRec+1L):(nextRec-1L),"iBefore"] <- currRec
			ans[(currRec+1L):(nextRec-1L),"wBefore"] <- ((distNext-1):1)/distNext  	
		} 	
	}
	##details<<
	## the rows before the first subset row refer bot (after and before) to the first subset row with weights 1/2
	## similar the rows after the last subset row refer to the last subset row
	ans[1:iRowsSpecial[1],c("iBefore","iAfter")] <- iRowsSpecial[1L] 
	ans[1:iRowsSpecial[1],c("wBefore","wAfter")] <- 0.5 
	ans[iRowsSpecial[nRecS]:nrow(ans),c("iBefore","iAfter")] <- iRowsSpecial[nRecS]
	ans[iRowsSpecial[nRecS]:nrow(ans),c("wBefore","wAfter")] <- 0.5
	ans
}




