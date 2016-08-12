#TODO remove dependency on minpack (nlsLM)
# renamve functions
# look with Mirco at why testcases not producing good results.

partRHLightResponse <- function(
		###Rectungular Hyperbolic Light Response function: (Xu & Baldocchi, 2004; Falge et al., 2001; Lasslop et al., 2010)
		theta, 	##<< theta [numeric] -> parameter vector (theta[1]=kVPD (k), theta[2]=beta0 (beta), theta[3]=alfa, theta[4]=Rref (rb))
		Rg,   	##<< ppfd [numeric] -> photosynthetic flux density [umol/m2/s] or Global Radiation
		VPD, 	##<< VPD [numeric] -> Vapor Pressure Deficit [hPa]
		Fc, 	##<< Fc [numeric] -> GPP or NEE time series [umolCO2/m2/s], required for inverse run, else may set to NA
		Fc_unc, ##<< Fc_unc [numeric] -> Uncertainty of fluxes [umolCO2/m2/s]
		Temp, 	##<< Temp [degC] -> Temperature [degC] 
		E0, 	##<< Temperature sensitivity ("activation energy") in Kelvin (degK) #get("testparams", envir=environment(foo)
		VPD0 = 10, 			##<< VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa according to Lasslop et al 2010
		fixVPD = FALSE,   	##<< fixVPD TRUE or FALSE -> if TRUE the VPD effect is not considered
		#run = 'forward'   	##<< run [character] -> 'forward' or 'inverse' if run the model forward or in inverse mode for optimization #twutz: string comparison can be slow, avoid in cost function  
		isInverse = FALSE  	##<< if TRUE, return the residual sum of squares instead of prediction  
) {
	##details<<
	## with fixes E0 for the estimation of Rd
	## VPD effect included according to Lasslop et al., 2010
	kVPD<-theta[1]
	beta0<-theta[2]
	alfa<-theta[3]
	Rref<-theta[4]
	Amax <- if( isTRUE(fixVPD) ) beta0 else {
		ifelse(VPD > VPD0, beta0*exp(-kVPD*(VPD-VPD0)), beta0)
	} 
	Reco<-Rref*exp(E0*(1/((273.15+10)-227.13)-1/(Temp+273.15-227.13)))
	NEP_mod<-(Amax*alfa*Rg)/(alfa*Rg+Amax) - Reco
	ans <- if(isInverse){
		#Conerting NEE in NEP
		NEP<-(-1)*Fc
		RSS<-sum(((NEP_mod-NEP)/Fc_unc)^2)
	} else NEP_mod
	ans
}

.partRHLightResponse_NoVPD <- function(
		### calling \code{\link{fRHRF_VPDRdFit}} with a parameter vector that omits first component, which is assumed zero 
		theta	##<< parameter vector with first component omitted
		,...	##<< further parameters to \code{\link{fRHRF_VPDRdFit}}
){
	##details<<
	## necessary for fitting the subset of parameters
	partRHLightResponse( c(0,theta), ...,fixVPD=TRUE)
}



sEddyProc$methods(
		sGLFluxPartition=function(
				##title<<
				## sGLFluxPartition - Flux partitioning after Lasslop et al. (2010)
				##description<<
				## Nighttime-based partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco)
				FluxVar.s=paste0('NEE',suffixDash.s,'_f')       ##<< Variable of net ecosystem fluxes
				,QFFluxVar.s=paste0('NEE',suffixDash.s,'_fqc')  ##<< Quality flag of variable
				,QFFluxValue.n=0         						##<< Value of quality flag for _good_ (original) data
				,TempVar.s=paste0('Tair',suffixDash.s,'_f')     ##<< Filled air or soil temperature variable (degC)
				,QFTempVar.s=paste0('Tair',suffixDash.s,'_fqc') ##<< Quality flag of filled temperature variable
				,QFTempValue.n=0       ##<< Value of temperature quality flag for _good_ (original) data
				,RadVar.s='Rg'         ##<< Unfilled (original) radiation variable
				,VPDVar.s=paste0('VPD',suffixDash.s,'_f')     ##<< Filled Vapor Pressure Deficit - VPD - (hPa)
				,QFVPDVar.s=paste0('VPD',suffixDash.s,'_fqc') ##<< Quality flag of filled VPD variable    
				,QFVPDValue.n=0       ##<< Value of VPD quality flag for _good_ (original) data
				,Lat_deg.n             ##<< Latitude in (decimal) degrees
				,Long_deg.n            ##<< Longitude in (decimal) degrees
				,TimeZone_h.n          ##<< Time zone (in hours)
				,Suffix.s = ""		   ##<< string inserted into column names before identifier (see \code{\link{sMDSGapFillUStar}}).
				,debug.l=list(		   ##<< list with debugging control, see \code{\link{sRegrE0fromShortTerm}}.
						##describe<< 
						useLocaltime.b=FALSE	##<< by default corrects hour (given in local winter time) for latitude to solar time
				##<< where noon is exactly at 12:00. Set this to TRUE to compare to code that uses local winter time
				##end<< 
				)        
				,T_ref.n=273.15+15       ##<< Reference temperature in Kelvin (degK) used in \code{fLloydTaylor} for regressing Flux and Temperature  
				,parsE0Regression=list() ##<< list with further parameters passed down to \code{\link{sRegrE0fromShortTerm}} and \code{\link{fRegrE0fromShortTerm}}, such as \code{TempRange.n} 
		)
		##author<<
		## MM
		##references<<
		## Lasslop G, Reichstein M, Papale D, et al. (2010) Separation of net ecosystem exchange into assimilation and respiration using 
		## a light response curve approach: critical issues and global evaluation. Global Change Biology, Volume 16, Issue 1, Pages 187–208
		{
			suffixDash.s <- paste( (if(fCheckValString(suffix.s)) "_" else ""), suffix.s, sep="")
			'Partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco) using Lasslop et al., 2010'
			# Check if specified columns exist in sDATA or sTEMP and if numeric and plausible. Then apply quality flag
			ds <- cbind(sDATA,sTEMP)
			fCheckColNames(ds, c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sGLFluxPartition')
			fCheckColNum(ds, c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sGLFluxPartition')
			fCheckColPlausibility(ds, c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sGLFluxPartition')
			Var.V.n <- fSetQF(ds, FluxVar.s, QFFluxVar.s, QFFluxValue.n, 'sGLFluxPartition')
			
			message('Start daytime flux partitioning for variable ', FluxVar.s, ' with temperature ', TempVar.s, '.')
			
			# Calculate potential radiation
			#! New code: Local time and equation of time accounted for in potential radiation calculation
			DoY.V.n <- as.numeric(format(sDATA$sDateTime, '%j'))
			Hour.V.n <- as.numeric(format(sDATA$sDateTime, '%H')) + as.numeric(format(sDATA$sDateTime, '%M'))/60
			sTEMP$NEW_PotRad <<- fCalcPotRadiation(DoY.V.n, Hour.V.n, Lat_deg.n, Long_deg.n, TimeZone_h.n
					, useSolartime.b=!isTRUE(debug.l$useLocaltime.b) )
			
			# Filter night time values only
			#! Note: Rg <= 4 congruent with Lasslop et al., 2010 to define Night for the calculation of E0
			# Should be unfilled (original) radiation variable, therefore dataframe set to sDATA only
			sTEMP$FP_VARnight <<- ifelse(sDATA[,RadVar.s] > 4 | sTEMP$NEW_PotRad != 0, NA,  Var.V.n)
			attr(sTEMP$FP_VARnight, 'varnames') <<- paste(attr(Var.V.n, 'varnames'), '_night', sep='')
			attr(sTEMP$FP_VARnight, 'units') <<- attr(Var.V.n, 'units')
			
			# Filter day time values only
			#! Note: Rg > 4 congruent with Lasslop et al., 2010 to define Day for the calculation of paremeters of Light Response Curve 
			# Should be unfilled (original) radiation variable, therefore dataframe set to sDATA only
			sTEMP$FP_VARday <<- ifelse(sDATA[,RadVar.s] < 4 | sTEMP$NEW_PotRad == 0, NA,  Var.V.n)
			attr(sTEMP$FP_VARday, 'varnames') <<- paste(attr(Var.V.n, 'varnames'), '_day', sep='')
			attr(sTEMP$FP_VARday, 'units') <<- attr(Var.V.n, 'units')
			
			#! New code: Slightly different subset than PV-Wave due to time zone correction (avoids timezone offset between Rg and PotRad)
			
			# Apply quality flag for temperature
			sTEMP$NEW_FP_Temp <<- fSetQF(ds, TempVar.s, QFTempVar.s, QFTempValue.n, 'sGLFluxPartition')
			
			# Apply quality flag for VPD
			sTEMP$NEW_FP_VPD <<- fSetQF(ds, VPDVar.s, QFVPDVar.s, QFVPDValue.n, 'sGLFluxPartition')
			
			#Estimate Parameters of light response curve: R_ref, alpha, beta and k according to Table A1 (Lasslop et al., 2010)
			
			# save(ds, file="tmp/dsTestPartitioningLasslop10.RData")
recover()
			resLRC <- tmp <- partGLFitLRCWindows(sTEMP$FP_VARnight, sTEMP$FP_VARday, Temp.V.n=sTEMP$NEW_FP_Temp
					, VPD.V.n=sTEMP$NEW_FP_VPD
					, Rg.V.n=ds[[RadVar.s]]
					, CallFunction.s='sGLFluxPartition'
					, nRecInDay=sINFO$DTS
			)
			#sTEMP$NEW_R_ref_DT <<- tmp

			# create a dataframe with parameter estimates before and after
			dss <- data.frame(T=ds[,TempVar.s], RbBefore=NA, E0Before=NA, RbAfter=NA, E0After=NA, wBefore=NA_integer_, wAfter=NA_integer_)
			dss[resLRC$MeanH,2:3] <- dss[resLRC$MeanH,4:5] <- resLRC[,c("R_ref","E_0")]
			dss[resLRC$MeanH,c("wBefore","wAfter")] <- 1L 
			# 
			#iLRC <- 2L
			for( iLRC in 2:(nLRC-1L) ){
				prevRec <- resLRC$MeanH[iLRC-1L]
				currRec <- resLRC$MeanH[iLRC]
				nextRec <- resLRC$MeanH[iLRC+1L]
				#resLRC$MeanH[iLRC+(-1L:1L)]
				# weights inverse to the distance in records
				dss[(prevRec+1L):(currRec-1L),"wAfter"] <- 1/((currRec-prevRec-1L):1)  #...,3,2,1	 
				dss[(currRec+1L):(nextRec-1L),"wBefore"] <- 1/(1:(nextRec-currRec-1))  #1,2,3,...	
				dss[(prevRec+1L):(currRec-1L),4:5] <- dss[(currRec+1L):(nextRec-1L),2:3] <- resLRC[iLRC,c("R_ref","E_0"),drop=FALSE]
			}
			#set ends both before and after to edge parameters
			nLRC <- nrow(resLRC)
			dss[1:resLRC$MeanH[1],2:5] <- resLRC[1,c("R_ref","E_0"),drop=FALSE] 
			dss[1:resLRC$MeanH[1],c("wBefore","wAfter")] <- 1L 
			dss[resLRC$MeanH[nLRC]:nrow(dss),2:5] <- resLRC[nLRC,c("R_ref","E_0"),drop=FALSE]
			dss[resLRC$MeanH[nLRC]:nrow(dss),c("wBefore","wAfter")] <- 1L 
			# fill in first and last record to the before and after periods
			dss[(resLRC$MeanH[1]+1L):(resLRC$MeanH[2]-1L),2:3] <- resLRC[1,c("R_ref","E_0"),drop=FALSE]	 
			dss[(resLRC$MeanH[1]+1L):(resLRC$MeanH[2]-1L),"wBefore"] <- 1/(1:(diff(resLRC$MeanH[1:2])-1L))	 
			dss[(resLRC$MeanH[nLRC-1L]+1L):(resLRC$MeanH[nLRC]-1L),4:5] <- resLRC[nLRC,c("R_ref","E_0"),drop=FALSE]	 
			dss[(resLRC$MeanH[nLRC-1L]+1L):(resLRC$MeanH[nLRC]-1L),"wAfter"] <- 1/((diff(resLRC$MeanH[nLRC-(1:0)])-1L):1)
			#
			dss$REcoBefore <- fLloydTaylor(dss$RbBefore, dss$E0Before, dss$T, T_ref.n=273.15+15)
			dss$REcoAfter  <- fLloydTaylor(dss$RbAfter,  dss$E0After,  dss$T, T_ref.n=273.15+15)
			dss$REco <- (dss$REcoBefore*dss$wBefore + dss$REcoAfter*dss$wAfter) / (dss$wBefore + dss$wAfter)
			plot(dss$REco, ylim=c(-2,1000), type="l")
			lines(dss$REcoBefore, ylim=c(-2,1000), type="l", col="red")
			lines(dss$REcoAfter, ylim=c(-2,1000), type="l", col="blue")
			
			
			##sRegrE0fromShortTerm('FP_VARnight', 'NEW_FP_Temp')
			sTEMP$NEW_E_0 <<- tmp <- .self$sRegrE0fromShortTerm('FP_VARnight', 'NEW_FP_Temp', CallFunction.s='sGLFluxPartition', debug.l=debug.l)
			#if( sum(sTEMP$NEW_E_0==-111) != 0 )
			#  return(invisible(-111)) # Abort flux partitioning if regression of E_0 failed
			#E_0.V.n <- subset(sTEMP[,E_0.s], Subset.b) # (Constant value)
			
			# Calculate the ecosystem respiration Reco
			sTEMP$Reco_DT <<- tmp <- fLloydTaylor(sTEMP$NEW_R_ref_DT, sTEMP$NEW_E_0, fConvertCtoK(cbind(sDATA,sTEMP)[,TempVar.s]), T_ref.n=273.15+15)
			attr(sTEMP$Reco_DT, 'varnames') <<- 'Reco_DT'
			attr(sTEMP$Reco_DT, 'units') <<- attr(Var.V.n, 'units')
			
			# Calculate the gross primary production GPP_f
			sTEMP$GPP_DT_f <<- -cbind(sDATA,sTEMP)[,FluxVar.s] + sTEMP$Reco_DT
			sTEMP$GPP_DT_fqc <<- cbind(sDATA,sTEMP)[,QFFluxVar.s]
			#! New code: MDS gap filling information are not copied from NEE_fmet and NEE_fwin to GPP_fmet and GPP_fwin
			#           (since not known within this pure partitioning function)
			attr(sTEMP$GPP_f, 'varnames') <<- 'GPP_DT_f'
			attr(sTEMP$GPP_f, 'units') <<- attr(Var.V.n, 'units')
			
			# TODO: Adjust all output columns to account for suffix?
			
			# Rename new columns generated during flux partitioning
			colnames(sTEMP) <<- gsub('_VAR', '_NEE', colnames(sTEMP))
			colnames(sTEMP) <<- gsub('NEW_', '', colnames(sTEMP))
			# may have introduced duplicate columns, delete first one, side effect: triple column names will be renamed .<nr>
			duplColNames <- names(which(table(colnames(sTEMP)) > 1))
			if( length(duplColNames) ) sTEMP <<- sTEMP[, -match( duplColNames, colnames(sTEMP) )]
			
			##details<<
			## Description of newly generated variables with partitioning results: \cr
			## PotRad - Potential radiation \cr
			## FP_NEEnight - Good (original) NEE nighttime fluxes  used for flux partitioning \cr
			## FP_Temp - Good (original) temperature measurements used for flux partitioning \cr
			## E_0 - Estimated temperature sensitivity \cr
			## R_ref - Estimated reference respiration \cr
			## Reco_DT - Estimated ecosystem respiration from daytime partitioning (Lasslop et al., 2010) \cr
			## GPP_f - Estimated gross primary production from daytime partitioning (Lasslop et al., 2010) \cr
			
			return(invisible(NULL))
			##value<< 
			## Flux partitioning results in sTEMP data frame (with renamed columns).
		}
)


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
			,control=nls.lm.control(maxiter = 20))
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
		NEEDay.V.n, NEENight.V.n, Rg.V.n, Temp_degK.V.n, VPD.V.n, E_0.V.n
){
	#Definition of initial guess theta, theta2 and theta3. Three initial guess vectors are defined according to Lasslop et al., 2010
	theta.V.n<-matrix(NA, 3,4, dimnames=list(NULL,c("k","beta0", "alfa", "Rb" )))
	theta.V.n[1,]<-c(0,
			as.numeric(abs(quantile(NEEDay.V.n, 0.03)-quantile(NEEDay.V.n, 0.97))),
			0.1,
			mean(NEENight.V.n, na.rm=T))   #theta [numeric] -> parameter vector (theta[1]=kVPD, theta[2]-beta0, theta[3]=alfa, theta[4]=Rref)
	theta.V.n[2,]<-theta.V.n[1,]/2
	theta.V.n[3,]<-theta.V.n[1,]*2
	#
	npars<-dim(theta.V.n)[2]
	nobs<-length(NEEDay.V.n)
	opt<-list(opt1=rep(NA,npars), opt2=rep(NA,npars), opt3=rep(NA,npars))
	optSSE<-list(opt1=NA, opt2=NA, opt3=NA)
	optimLRC <- function(theta, isUsingFixedVPD=FALSE, idx=TRUE){
		# one fit of the light response curve, as function to avoid duplication
		if( isUsingFixedVPD){
			resOptim <- optim(theta[-1], .partRHLightResponse_NoVPD, 
					Rg = Rg.V.n[idx], 
					Fc = NEEDay.V.n[idx], 
					# TODO: think about uncertainties use NEE_fsd
					#Fc_unc = abs(0.0001*NEEday.V.n)/abs(0.0001*NEEday.V.n),  
					Fc_unc = abs(0.05*NEEDay.V.n[idx]),	  
					Temp=Temp_degK.V.n[idx]-273.15,
					VPD = VPD.V.n[idx],
					E0 = E_0.V.n,
					#run = 'inverse',
					isInverse = TRUE,
					method="BFGS", hessian=FALSE)
			resOptim$par=c(k=0,resOptim$par)	# add VPD parameter again
			resOptim
		} else {
			optim(theta, partRHLightResponse, 
					Rg = Rg.V.n[idx], 
					Fc = NEEDay.V.n[idx], 
					# TODO: think about uncertainties use NEE_fsd
					#Fc_unc = abs(0.0001*NEEday.V.n)/abs(0.0001*NEEday.V.n),  
					Fc_unc = abs(0.05*NEEDay.V.n[idx]),	  
					Temp=Temp_degK.V.n[idx]-273.15,
					VPD = VPD.V.n[idx],
					E0 = E_0.V.n,
					#run = 'inverse',
					isInverse = TRUE,
					method="BFGS", hessian=FALSE)
		}
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
			points(Rg.V.n, partRHLightResponse(theta=resOpt$par,
							Rg = Rg.V.n, 
							Fc = NEEDay.V.n, 
							Fc_unc = abs(0.01*NEEDay.V.n),  
							Temp=Temp_degK.V.n-273.15,
							VPD = VPD.V.n,
							E0 = E_0.V.n,
							fixVPD = FALSE,
							#run = 'forward'
							isInverse=FALSE
							), col="Red")
		}
	}
	if( sum(!is.na(optSSE)) == 0L ){
		# none of the intial fits yielded result
		opt.parms.V <- se.parms.V <- rep(NA_real_, npars)
		names(opt.parms.V) <- names(se.parms.V) <- colnames(theta.V.n)
	} else {
		opt.parms.V<-opt[[iBest <- which.min(optSSE)]]		
		#+++++++ Compute parameters uncertainty Uncertainty by bootstrap
		nboot<-10  #move this at the top of the call
		unc_parm<-rep(NA,npars)
		unc_parm_matrix<-matrix(NA, nrow=nboot, ncol=npars, dimnames=list(NULL,names(opt.parms.V)))
		isUsingFixedVPD <- (opt.parms.V[1] <= 0) 
		#
		#z <- 1L
		for (z in c(1:nboot)){
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


partGLFitLRCWindows=function(
		### estimateLRCParms - Estimation of the parameters of the Rectangular Hyperbolic Light Response Curve function (a,b,R_ref, k)
		NEENight.V.n        ##<< numeric vector of (original) nighttime ecosystem carbon flux, i.e. respiration
		,NEEDay.V.n         ##<< numeric vector of (original) daytime ecosystem carbon flux, i.e. Net Ecosystem Exchange
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
			, E_0=NA_real_			##<< temperature sensitivty
			, E_0_SD=NA_real_		##<< standard deviation of temperature sensitivity
			, R_ref=NA_real_		##<< respiration at reference temperature
			, R_ref_SD=NA_real_		##< standard deviation of reference temperature
			,a=NA_real_, a_SD=NA_real_
			,b=NA_real_, b_SD=NA_real_
			,k=NA_real_, k_SD=NA_real_
			,parms_out_range=NA_integer_)
	lastGoodParameters.V.n <- rep(NA_real_, 5)		# indicate no good parameter set found yet
	E_0.V.n <- NA
	CountRegr.i <- 0L
	for (iDay in seq_along(middleDays.V.i)) {   #not sure is correct to me should be 
		#TEST: iDay<-1L
		DayMiddle.i <- middleDays.V.i[iDay]
		message(",",DayMiddle.i, appendLF = FALSE)
		DayStart.i <- DayMiddle.i-WinDays.i
		DayEnd.i <- DayMiddle.i+WinDays.i
		DayStart.Night.i <- DayMiddle.i-WinNight.i
		DayEnd.Night.i <- DayMiddle.i+WinNight.i
		#! Window size of 2 days corresponds to a full window length of 5 days, non-congruent with PV-Wave code of 4 days, in paper not mentioned
		#! New code: Last window has minimum of window size
		Subset.b <- DayCounter.V.i >= DayStart.i & DayCounter.V.i <= DayEnd.i & 
				!is.na(NEEDay.V.n) & !is.na(Temp.V.n) & !is.na(VPD.V.n)
		Subset.Night.b <- DayCounter.V.i >= DayStart.Night.i & DayCounter.V.i <= DayEnd.Night.i & 
				!is.na(NEENight.V.n)
		MeanHour.i <- round(mean(which(Subset.b))) # the rownumber in the entire dataset representing the center of the period 
		NEENightInPeriod.V.n <- subset(NEENight.V.n, Subset.Night.b)
		NEEDayInPeriod.V.n <- subset(NEEDay.V.n, Subset.b)
		# note that 
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
			resE0 <- partGLEstimateTempSensInBounds(NEENightInPeriod.V.n, TempInNightPeriod_degK.V.n, prevE0=E_0.V.n)
			E_0.V.n <- resE0$E_0
			#
			#tryCatch({
			resOpt <- resOpt0 <- partGLFitLRC(NEEDayInPeriod.V.n, NEENightInPeriod.V.n, RgInPeriod.V.n, TempInPeriod_degK.V.n, VPDInPeriod.V.n, E_0.V.n)
			#if( DayMiddle.i >= 5 ) recover()
			.tmp.plot <- function(){
				plot( -NEEDayInPeriod.V.n ~ RgInPeriod.V.n )
				tmp <- partRHLightResponse(resOpt$opt.parms.V, RgInPeriod.V.n, VPDInPeriod.V.n, NEEDayInPeriod.V.n, 1, TempInPeriod_degK.V.n-273.15,  E_0.V.n)
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
					Start=DayStart.i, End=DayEnd.i, Num=length(NEEDayInPeriod.V.n), MeanH=MeanHour.i,
					E_0=E_0.V.n, E_0_SD=resE0$E_0_SD,   
					R_ref=resOptBounded$opt.parms.V[4], R_ref_SD=resOptBounded$se.parms.V[4],
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
	OPTRes.LRC <- resDf[!is.na(resDf$End),]
#	#! New code: Omit regressions with R_ref <0, in PV-Wave smaller values are set to 0.000001, not mentioned in paper
#	#TODO later: Flag for long distances between R_refs, especially if long distance in the beginning - twutz: may make use of resDf$End == NA 
#	#TODO later: Provide some kind of uncertainty estimate from R_ref_SD
}




