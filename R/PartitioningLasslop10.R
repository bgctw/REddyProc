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
		,T_ref.n=273.15+15       ##<< Reference temperature in Kelvin (degK) used in \code{fLloydTaylor} for regressing Flux and Temperature  
		,parsE0Regression=list() ##<< list with further parameters passed down to \code{\link{sRegrE0fromShortTerm}} and \code{\link{fRegrE0fromShortTerm}}, such as \code{TempRange.n}
		,isVerbose=TRUE			 ##<< set to FALSE to suppress output messages
		,nRecInDay=48L			##<< number of records within one day (for half-hourly data its 48)
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
	dsAns$FP_VARnight <- ifelse(ds[,RadVar.s] > 4 | ds[[PotRadVar.s]] != 0, NA,  Var.V.n)
	attr(dsAns$FP_VARnight, 'varnames') <- paste(attr(Var.V.n, 'varnames'), '_night', sep='')
	attr(dsAns$FP_VARnight, 'units') <- attr(Var.V.n, 'units')
	# Filter day time values only
	#! Note: Rg > 4 congruent with Lasslop et al., 2010 to define Day for the calculation of paremeters of Light Response Curve 
	# Should be unfilled (original) radiation variable, therefore dataframe set to sDATA only
	dsAns$FP_VARday <- ifelse(ds[,RadVar.s] < 4 | ds[[PotRadVar.s]] == 0, NA,  Var.V.n)
	attr(dsAns$FP_VARday, 'varnames') <- paste(attr(Var.V.n, 'varnames'), '_day', sep='')
	attr(dsAns$FP_VARday, 'units') <- attr(Var.V.n, 'units')
	#! New code: Slightly different subset than PV-Wave due to time zone correction (avoids timezone offset between Rg and PotRad)
	# Apply quality flag for temperature and VPD
	dsAns$NEW_FP_Temp <- fSetQF(ds, TempVar.s, QFTempVar.s, QFTempValue.n, 'sGLFluxPartition')
	dsAns$NEW_FP_VPD <- fSetQF(ds, VPDVar.s, QFVPDVar.s, QFVPDValue.n, 'sGLFluxPartition')
	#Estimate Parameters of light response curve: R_ref, alpha, beta and k according to Table A1 (Lasslop et al., 2010)
	# save(ds, file="tmp/dsTestPartitioningLasslop10.RData")
	resLRC <- tmp <- partGLFitLRCWindows(dsAns$FP_VARnight, dsAns$FP_VARday
			, NEESd.V.n=ds[[FluxSdVar.s]]
			, Temp.V.n=dsAns$NEW_FP_Temp
			, VPD.V.n=dsAns$NEW_FP_VPD
			, Rg.V.n=ds[[RadVar.s]]
			, CallFunction.s='sGLFluxPartition'
			, nRecInDay=nRecInDay
	)
	dsAns[resLRC$iFirstRecInCentralDay,c("FP_E0","FP_R_ref","FP_alpha","FP_beta","FP_k")] <- resLRC[,c("E_0","R_ref","a","b","k")] 	
	dsAnsFluxes <- partGLInterpolateFluxes( ds[,RadVar.s], dsAns$NEW_FP_VPD, dsAns$NEW_FP_Temp, resLRC	)
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
		NEENight.V.n        ##<< numeric vector of ecosystem carbon flux, i.e. respiration with all non-night records set to NA
		,NEEDay.V.n         ##<< numeric vector of ecosystem carbon flux, i.e. Net Ecosystem Exchange with all non-day records set to NA
		,NEESd.V.n			##<< numeric vector of estimated standard deviation of NEE
		,Temp.V.n        	##<< numeric vector of (original) air or soil temperature (degC)
		,VPD.V.n         	##<< numeric vector of (original) Vapor Pressure Deficit VPD (hPa)
		,Rg.V.n          	##<< numeric vector of (original) Global Radiation (Wm-2)
		,WinDays.i=2		##<< Window size for \code{\link{fHLRC()}} fitting in days (Half of the interval, if Windows is 4 WinDays.i = 2)
		,WinNight.i=6		##<< Window size for E0 fitting with \code{\link{fHLRC()}} in days  (Half of the interval, if Windows is 13 WinDays.i = 2)
		,DayStep.i=2        ##<< Window step for \code{\link{fHLRC()}} regression in days#Verify IF CORRECT
		,CallFunction.s=''  ##<< Name of function called from
		,isVerbose=TRUE		##<< set to FALSE to suppress messages
		,nRecInDay=48L		##<< number of records within one day (for half-hourly data its 48)
){
	##author<<
	## MM, TW
	##description<<
	## Estimation of the parameters of the Rectangular Hyperbolic Light Response Curve function (a,b,R_ref, k) as in Lasslop et al., 2010 \code{\link{fLloydTaylor()}} for successive periods
	'Estimation of the reference respiration Rref of fLloydTaylor() for successive periods'
	# Regression settings
	LMRes.F <- data.frame(NULL) #Results of linear regression
	MinData.n <- 2 # Minimum number of data points for regression #CHECK IN GITTA'S CODE MINIMUM NUMBERS OF DAYS
	# Loop regression periods
	#DayCounter.V.i <- c(1:sINFO$DIMS) %/% sINFO$DTS #twutz: do not rely on sINFO here, also a -1 is missing 
	##details<<
	## All the vectors must have the same length and consist of entire days, with each day having the same number of records
	nRec <- length(NEENight.V.n) 
	DayCounter.V.i <- (c(1:nRec)-1L) %/% nRecInDay 
	#DayMiddle.i <- WinDays.i+1
	# setup a data.frame for the results
	middleDays.V.i <- seq(WinDays.i+1, max(DayCounter.V.i), DayStep.i)
	##value<< data.frame with columns
	resDf <- data.frame(
			Start=middleDays.V.i	##<< the starting day of the window
			, End=NA_integer_		##<< the endding day of the window  
			, Num=NA_integer_		##<< the number of records flux records in the window
			, MeanH=NA_real_		##<< the record for which the parameters have been estimated
			, iFirstRecInCentralDay=NA_real_	##<< the record number of the first record of the center day of the window
			, E_0=NA_real_			##<< temperature sensitivty
			, E_0_SD=NA_real_		##<< standard deviation of temperature sensitivity
			, R_ref=NA_real_		##<< respiration at reference temperature
			, R_ref_SD=NA_real_		##< standard deviation of reference temperature
			,a=NA_real_, a_SD=NA_real_
			,b=NA_real_, b_SD=NA_real_
			,k=NA_real_, k_SD=NA_real_
			,parms_out_range=NA_integer_)
	lastGoodParameters.V.n <- rep(NA_real_, 5)		# indicate no good parameter set found yet
	E_0.n <- NA
	CountRegr.i <- 0L
	#iDay<-1L
	for (iDay in seq_along(middleDays.V.i)) {   #not sure is correct to me should be 
		DayMiddle.i <- middleDays.V.i[iDay]
		if( isVerbose ) message(",",DayMiddle.i, appendLF = FALSE)
		DayStart.i <- DayMiddle.i-WinDays.i
		DayEnd.i <- DayMiddle.i+WinDays.i
		DayStart.Night.i <- DayMiddle.i-WinNight.i
		DayEnd.Night.i <- DayMiddle.i+WinNight.i
		#! Window size of 2 days corresponds to a full window length of 5 days, non-congruent with PV-Wave code of 4 days, in paper not mentioned
		#! New code: Last window has minimum of window size
		SubsetDay.b <- DayCounter.V.i >= DayStart.i & DayCounter.V.i <= DayEnd.i 
		Subset.b <-  SubsetDay.b & 
				!is.na(NEEDay.V.n) & !is.na(Temp.V.n) & !is.na(VPD.V.n)
		Subset.Night.b <- DayCounter.V.i >= DayStart.Night.i & DayCounter.V.i <= DayEnd.Night.i & 
				!is.na(NEENight.V.n)
		MeanHour.i <- round(mean(which(Subset.b))) # the rownumber in the entire dataset representing the center of the period
		firstRecInCentralDay.i <- which(SubsetDay.b)[1] # the rownumber of the first record of the day of time center
		NEENightInPeriod.V.n <- subset(NEENight.V.n, Subset.Night.b)
		#NEESdNightInPeriod.V.n <- subset(NEESd.V.n, Subset.Night.b)
		NEEDayInPeriod.V.n <- subset(NEEDay.V.n, Subset.b)
		NEESdDayInPeriod.V.n <- subset(NEESd.V.n, Subset.b)
		# 
		TempInPeriod.V.n <- subset(Temp.V.n, Subset.b)
		TempInNightPeriod.V.n <- subset(Temp.V.n, Subset.Night.b)
		TempInPeriod_degK.V.n <- fConvertCtoK(TempInPeriod.V.n)
		TempInNightPeriod_degK.V.n <- fConvertCtoK(TempInNightPeriod.V.n)
		VPDInPeriod.V.n <- subset(VPD.V.n, Subset.b)
		RgInPeriod.V.n  <- subset(Rg.V.n, Subset.b)
		#		
		if( length(NEEDayInPeriod.V.n) > MinData.n ) {
			CountRegr.i <- CountRegr.i+1L
			#Estimation of model parameters
			#tryCatch({
			#  LM.L <- lm(R_eco ~ 0 + fLloydTaylor(R_ref, E_0, Temp_degK, T_ref.n=273.15+15), data=as.data.frame(cbind(R_eco=NEEnight.V.n, R_ref=1, E_0=E_0.V.n, Temp_degK=Temp_degK.V.n)))
			#  LMRes.F <- rbind(LMRes.F, cbind(Start=DayStart.i, End=DayEnd.i, Num=length(NEEnight.V.n), MeanH=MeanHour.i, 
			#                                  R_ref=coef(summary(LM.L))[1], R_ref_SD=coef(summary(LM.L))[2]))
			
			#! Note on PV-Wave code: trimmed linear regression not used in the end, i.e. in online webtool
			
			#  if( F ) { # Plot for testing
			#    plot(NEEnight.V.n ~ fLloydTaylor(1, E_0.V.n, Temp_degK.V.n, T_ref.n=273.15+15))
			#    curve(coef(LM.L)[1] * x, add=T, col='green')
			#  }  
			#}, error = function(e) {
			#  LMRes.F <- rbind(LMRes.F, cbind(Start=DayStart.i, End=DayEnd.i, Num=length(NEEnight.V.n), MeanH=MeanHour.i, 
			#                                  R_ref=NA, R_ref_SD=NA))
			#}
			#			
			#resOptim <- sOptimSingleE0_Lev( NEEnight.V.n, Tempnight_degK.V.n)
			# TODO: also use uncertainties in fit?
			resE0 <- partGLEstimateTempSensInBounds(NEENightInPeriod.V.n, TempInNightPeriod_degK.V.n, prevE0=E_0.n)
			E_0.n <- resE0$E_0
			#
			#tryCatch({
	#if( DayMiddle.i == 113) recover()
			resOpt <- resOpt0 <- partGLFitLRC(NEEDayInPeriod.V.n, NEESdDayInPeriod.V.n, NEENightInPeriod.V.n, RgInPeriod.V.n, TempInPeriod.V.n, VPDInPeriod.V.n, E_0.n=E_0.n)
			.tmp.plot <- function(){
				plot( -NEEDayInPeriod.V.n ~ RgInPeriod.V.n )
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
			#
			# twutz: avoid slow rbind, but write into existing data.frame
			resDf[iDay, ] <- data.frame(
					Start=DayStart.i, End=DayEnd.i, Num=length(NEEDayInPeriod.V.n)
					,MeanH=MeanHour.i
					,iFirstRecInCentralDay = firstRecInCentralDay.i
					,E_0=E_0.n, E_0_SD=resE0$E_0_SD   
					,R_ref=resOptBounded$opt.parms.V[4], R_ref_SD=resOptBounded$se.parms.V[4],
					a=resOptBounded$opt.parms.V[3], a_SD=resOptBounded$se.parms.V[3],
					b=resOptBounded$opt.parms.V[2], b_SD=resOptBounded$se.parms.V[2],
					k=resOptBounded$opt.parms.V[1], k_SD=resOptBounded$se.parms.V[1],
					parms_out_range=as.integer(!resOptBounded$isGoodParameterSet))
		} # if enough records
		# remove those rows where there was not enough data
		#! Note on PV-Wave code: trimmed linear regression not used in the end, i.e. in online webtool
		#if( F ) { # Plot for testing
		#  plot(NEEday.V.n ~ fRHRF_RdFit(1, E_0.V.n, Temp_degK.V.n, T_ref.n=273.15+15))
		#  curve(coef(LM.L)[1] * x, add=T, col='green')
		#}  
#							}, error = function(e) {
#								OPTRes.LRC <- rbind(OPTRes.LRC, cbind(Start=DayStart.i, End=DayEnd.i, Num=length(NEEday.V.n), MeanH=MeanHour.i, 
#												R_ref=NA, R_ref_SD=NA,
#												a=NA, a_SD=NA,
#												b=NA, b_SD=NA,
#												k=NA, k_SD=NA,
#												parms_out_range=NA))
#							})  #Spaces between brackets required to avoid replacement on documentation generation
		
		
	} # for i in days
	if( isVerbose ) message("") # LineFeed
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
){
	#twutz: using nls to avoid additional package dependency
	#resFitLM <- NLS.L <- nlsLM(formula=R_eco ~ fLloydTaylor(R_ref, E_0, Temp, T_ref.n=273.15+15), algorithm='default', trace=FALSE,
	#		data=as.data.frame(cbind(R_eco=REco.V.n,Temp=temperatureKelvin.V.n)), start=list(R_ref=mean(REco.V.n,na.rm=TRUE),E_0=100)
	#		,control=nls.lm.control(maxiter = 20))
	resFit <- nls(formula=R_eco ~ fLloydTaylor(R_ref, E_0, Temp, T_ref.n=273.15+15), algorithm='default', trace=FALSE,
			data=as.data.frame(cbind(R_eco=REco.V.n,Temp=temperatureKelvin.V.n)), start=list(R_ref=mean(REco.V.n,na.rm=TRUE),E_0=100)
			,control=nls.control(maxiter = 20))
	E_0Bounded.V.n <- E_0.V.n <- coef(resFit)['E_0']
	E_0_SD.V.n <- coef(summary(resFit))['E_0',2]
	if( (E_0.V.n < 50) || (E_0.V.n > 400)){
		E_0Bounded.V.n <- if( is.na(prevE0) ){
					min(400,max(50,E_0.V.n))
				} else {
					prevE0
				}
	}
	##value<< list with entries
	list(
			E_0=E_0Bounded.V.n		##<< numeric scalar of estimated temperature sensitivty E0 bounded to [50,400]
			,E_0_SD=E_0_SD.V.n		##<< numeric scalar of standard deviation of E0
			,resFit=resFit				##<< the fit-object
	)
}

partGLFitLRC <- function(
		### optimization for three different initial parameter sets
		NEEDay.V.n			##<< non-na numeric vector of day time fluxes
		,NEESdDay.V.n		##<< standard deviation of NEEDay
		, NEENight.V.n		##<< non-na numeric vector of night time fluxes to estimate initial value of Rb
		, Rg.V.n			##<< solar radion (numeric vector of length of NEEDay)
		, Temp_C.V.n		##<< temperature in deg Celsius
		, VPD.V.n			##<< VPD	
		, E_0.n				##<< temperature sensitivity of respiration
		, nBoot.i=10L		##<< scalar integer of number of bootstrap samples for estimating uncertainty of parameters
		#TODO: increase default of nBoot after testing, option to use Hessian
){
	#Definition of initial guess theta, theta2 and theta3. Three initial guess vectors are defined according to Lasslop et al., 2010
	theta.V.n<-matrix(NA, 3,4, dimnames=list(NULL,c("k","beta0", "alfa", "Rb" )))
	theta.V.n[1,]<-c(0,
			as.numeric(abs(quantile(NEEDay.V.n, 0.03)-quantile(NEEDay.V.n, 0.97))),
			0.1,
			mean(NEENight.V.n, na.rm=T))   #theta [numeric] -> parameter vector (theta[1]=kVPD, theta[2]-beta0, theta[3]=alfa, theta[4]=Rref)
	betaPrior <- theta.V.n[1,2]
	theta.V.n[2,]<-theta.V.n[1,]/2
	theta.V.n[3,]<-theta.V.n[1,]*2
	#twutz: alpha is quite well defined, so try not changing it too much
	theta.V.n[2,2] <- betaPrior*1.3
	theta.V.n[3,2] <- betaPrior*0.8
	#
	npars<-dim(theta.V.n)[2]
	nobs<-length(NEEDay.V.n)
	opt<-list(opt1=rep(NA,npars), opt2=rep(NA,npars), opt3=rep(NA,npars))
	optSSE<-list(opt1=NA, opt2=NA, opt3=NA)
	optimLRC <- function(theta, isUsingFixedVPD=FALSE, idx=TRUE){
		# one fit of the light response curve, as function to avoid duplication
		# TODO: think about uncertainty, 
		# twutz: need to have a minimum uncertainty, else division by zero for NEE=0
		Fc_unc0 <- NEESdDay.V.n[idx]
		#Fc_unc0 <- abs(0.05*NEEDay.V.n[idx])
		Fc_unc <- pmax( Fc_unc0, quantile(Fc_unc0, 0.75) ) #twutz: avoid excessive weights by small uncertainties (of 1/unc^2)
		if( isUsingFixedVPD){
			resOptim <- optim(theta[-1], .partRHLightResponseCost_NoVPD, 
					Rg = Rg.V.n[idx], 
					Fc = NEEDay.V.n[idx], 
					Fc_unc = Fc_unc,	  
					Temp=Temp_C.V.n[idx],
					VPD = VPD.V.n[idx],
					E0 = E_0.n
					,betaPrior = betaPrior
					,method="BFGS", hessian=FALSE)
			resOptim$par=c(k=0,resOptim$par)	# add VPD parameter again
			resOptim
		} else {
			tmp <- optim(theta, .partRHLightResponseCost, 
					Rg = Rg.V.n[idx], 
					Fc = NEEDay.V.n[idx], 
					Fc_unc = Fc_unc,	  
					Temp=Temp_C.V.n[idx],
					VPD = VPD.V.n[idx],
					E0 = E_0.n
					,betaPrior = betaPrior
					,method="BFGS", hessian=FALSE)
		}
	}
	.tmp.f <- function(){
		tmp <- .partRHLightResponseCost(theta,Rg = Rg.V.n[idx], 
				Fc = NEEDay.V.n[idx], 
				Fc_unc = Fc_unc,	  
				Temp=Temp_C.V.n[idx],
				VPD = VPD.V.n[idx],
				E0 = E_0.n)
	}
	#iparms=1L
	for(iparms in 1:nrow(theta.V.n)){ 
		#CHANGE Fc_unc
		resOpt <- resOpt0 <- optimLRC(theta.V.n[iparms,], isUsingFixedVPD=FALSE)
		# IF kVPD parameter less than 0 estimate the parameters withouth VPD effect
		if (resOpt$par[1] <= 0) resOpt <- optimLRC(resOpt0$par, isUsingFixedVPD=TRUE)
		if( resOpt$convergence == 0){
			opt[[iparms]] <- resOpt$par
			optSSE[iparms] <- resOpt$value
		}
		.tmp.plot <- function(){
			plot(Rg.V.n,-1*NEEDay.V.n)
			thetaB <- resOpt$par
			#thetaB <-  opt[[iBest <- which.min(optSSE)]]	
			points(Rg.V.n, tmp <- partRHLightResponse(thetaB,
							Rg = Rg.V.n, 
							Temp=Temp_C.V.n,
							VPD = VPD.V.n,
							E0 = E_0.n,
							fixVPD = FALSE,
							)$NEP, col="green")
		}
	}
	if( sum(!is.na(optSSE)) == 0L ){
		# none of the intial fits yielded result
		opt.parms.V <- se.parms.V <- rep(NA_real_, npars)
		names(opt.parms.V) <- names(se.parms.V) <- colnames(theta.V.n)
	} else {
		opt.parms.V<-opt[[iBest <- which.min(optSSE)]]
		#+++++++ Compute parameters uncertainty Uncertainty by bootstrap
		unc_parm<-rep(NA,npars)
		unc_parm_matrix<-matrix(NA, nrow=nBoot.i, ncol=npars, dimnames=list(NULL,names(opt.parms.V)))
		isUsingFixedVPD <- (opt.parms.V[1] <= 0) 
		#
		#z <- 1L
		for (z in c(1:nBoot.i)){
			idx <- sample(length(Rg.V.n), replace=TRUE)
			try({
						resOptBoot <- optimLRC(opt.parms.V, isUsingFixedVPD=isUsingFixedVPD, idx=idx)
						if( resOptBoot$convergence == 0L )	
							#TODO: also remove the bery bad cases 
							unc_parm_matrix[z,]<-resOptBoot$par
					})
		}
		se.parms.V<-apply(unc_parm_matrix, MARGIN=2, FUN=sd, na.rm=TRUE)
	}
	##value<< a list, If none of the optimizations from different starting conditions converged,
	## the parameters are NA
	ans <- list(
			opt.parms.V=opt.parms.V		##<< numeric vector of optimized parameters
			,se.parms.V=se.parms.V		##<< numeric vector of their standard deviation
			,initialGuess.parms.V.n=theta.V.n[1,]	##<< the initial guess
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

.partRHLightResponseCost <- function(
		### Computing residual sum of sqares for predictions vs. data of NEE
		theta 		##<< theta [numeric] -> parameter vector with positions as in argument of \code{\link{partRHLightResponse}}
		,Fc=NA 		##<< Fc [numeric] ->  NEE time series [umolCO2/m2/s]
		,Fc_unc=NA 	##<< Fc_unc [numeric] -> Uncertainty of fluxes [umolCO2/m2/s]
		,betaPrior	##<< prior estimate of beta parameter (plateau)
		,...		##<< other arguments to \code{\link{partRHLightResponse}}
) {
	resPred <- partRHLightResponse(theta, ...)
	NEP_mod <- resPred$NEP
	#Converting NEE in NEP
	NEP<-(-1)*Fc
	##details<< the beta parameter is quite well defined. Hence put a prior with a standard deviation of x 
	misFitPrior <- (((theta[2] - betaPrior))/(0.3*betaPrior))^2
	misFitObs <- sum(((NEP_mod-NEP)/Fc_unc)^2)
	RSS <- misFitObs + misFitPrior*length(Fc)
	#if( !is.finite(RSS) ) recover()	# debugging the fit
	RSS
}

.partRHLightResponseCost_NoVPD <- function(
		### calling \code{\link{fRHRF_VPDRdFit}} with a parameter vector that omits first component, which is assumed zero 
		theta	##<< parameter vector with first component omitted
		,...	##<< further parameters to \code{\link{fRHRF_VPDRdFit}}
){
	##details<<
	## necessary for fitting only a subset of parameters, with fixing the first parameter to zero
	.partRHLightResponseCost( c(0,theta), ..., fixVPD=TRUE)
}



partGLBoundParameters <- function(
		### Check if parameters are in the range
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
	isGoodParameterSet <- TRUE	# FALSE means parameters are outside range and no computation uncertainties
	if( any(is.na(opt.parms.V)) ){
		isGoodParameterSet <- FALSE
	} else {
		if (opt.parms.V[1] < 0 ){	# k
			opt.parms.V[1]<-0
			isGoodParameterSet <- FALSE
		}
		if (opt.parms.V[2] < 0 ){	# beta
			opt.parms.V[2]<-0
			isGoodParameterSet <- FALSE
		}
		if (opt.parms.V[3] <= 0 | opt.parms.V[3] > 0.22){ #set to alpha values of the latest good parameters set
			opt.parms.V[3]<-last_good[3]	
			isGoodParameterSet <- FALSE
		}
		#whole par set not used, if beta > 250 or (beta > 100 and sdBeta >= beta) or rb < 0 
		if ((opt.parms.V[2] > 250 | (opt.parms.V[2] > 100 && se.parms.V[2] >= opt.parms.V[2] ) | opt.parms.V[4] < 0  )){ 
			opt.parms.V[]<-NA
			isGoodParameterSet <- FALSE
		}
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
){
	##details<< 
	## \code{resLRC$iFirstRecInCentralDay} must denote the row for which the LRC parameters are representative, 
	## here, the first record of the center day
	# create a dataframe with index of rows of estimates before and after and correponding weights
	nLRC <- nrow(resLRC)
	nRec <- length(Rg) 
	Temp_Kelvin <- Temp+273.15
	# for each original record merge parameters assicated with previous fit or next fit respectively
	dsAssoc <- .partGPAssociateSpecialRows(resLRC$iFirstRecInCentralDay,nRec)	
	dsBefore <- merge( data.frame(iFirstRecInCentralDay=dsAssoc$iBefore), resLRC[,c("iFirstRecInCentralDay","R_ref","E_0","a","b","k")])
	dsAfter <- merge( data.frame(iFirstRecInCentralDay=dsAssoc$iAfter), resLRC[,c("iFirstRecInCentralDay","R_ref","E_0","a","b","k")])
	if( nrow(dsBefore) != nrow(dsAssoc) ) stop("error in merging parameters to original records.")
	Reco2 <- lapply( list(dsBefore,dsAfter), function(dsi){
		tmp <- fLloydTaylor(dsi$R_ref, dsi$E_0, Temp_Kelvin, T_ref.n=273.15+15)
	})
	#dsi <- dsBefore
	GPP2 <- lapply( list(dsBefore,dsAfter), function(dsi){
				theta <- as.matrix(dsi[,c("k","b","a","R_ref")])
				tmp <- partRHLightResponse(theta, Rg, VPD, Temp=Temp, E0=dsi$E_0)$GPP
		})
	# interpolate between previous and next fit
	Reco <- (dsAssoc$wBefore*Reco2[[1]] + dsAssoc$wAfter*Reco2[[2]]) / (dsAssoc$wBefore+dsAssoc$wAfter)  
	GPP <- (dsAssoc$wBefore*GPP2[[1]] + dsAssoc$wAfter*GPP2[[2]]) / (dsAssoc$wBefore+dsAssoc$wAfter)
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
		## the weight is inversely proportional to the distance in rows
		if( currRec-prevRec > 1L){
			ans[(prevRec+1L):(currRec-1L),"iAfter"] <- currRec 
			ans[(prevRec+1L):(currRec-1L),"wAfter"] <- 1/((currRec-prevRec-1L):1)  #...,3,2,1	 
		}
		if( nextRec-currRec > 1L){
			ans[(currRec+1L):(nextRec-1L),"iBefore"] <- currRec
			ans[(currRec+1L):(nextRec-1L),"wBefore"] <- 1/(1:(nextRec-currRec-1))  #1,2,3,...
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




