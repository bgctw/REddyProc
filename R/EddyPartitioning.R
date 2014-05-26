#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc methods for flux partitioning +++
#+++ Flux partitionig algorithm, adapted after the PV-Wave code and paper by Markus Reichstein +++
#+++ Dependencies: Eddy.R, DataFunctions.R
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#TEST: FluxVar.s <- 'NEE_f'; QFFluxVar.s <- 'NEE_fqc'; QFFluxValue.n <- 0; TempVar.s <- 'Tair_f'; QFTempVar.s <- 'Tair_fqc'; QFTempValue.n <- 0
#TEST: RadVar.s <- 'Rg'; Lat_deg.n <- 51.0; Long_deg.n <- 13.6; TimeZone_h.n <- 1.0; CallFunction.s='test'
#TEST: NightFlux.s='FP_VARnight';  TempVar.s='NEW_FP_Temp'; WinDays.i=7; DayStep.i=5; TempRange.n=5; NumE_0.n=3; Trim.n=5
#TEST: NightFlux.s='FP_VARnight';  TempVar.s='NEW_FP_Temp'; E_0.s='NEW_E_0'; WinDays.i=4; DayStep.i=4;

sEddyProc$methods(
  sRegrE0fromShortTerm = function(
    ##title<<
    ## sEddyProc$sRegrE0fromShortTerm - Estimation of the temperature sensitivity E_0
    ##description<<
    ## Estimation of the temperature sensitivity E_0 from regression of \code{\link{fLloydTaylor()}} for short periods
    NightFlux.s           ##<< Variable with (original) nighttime ecosystem carbon flux, i.e. respiration
    ,TempVar.s            ##<< Variable with (original) air or soil temperature (degC)
    ,WinDays.i=7          ##<< Window size for \code{\link{fLloydTaylor()}} regression in days
    ,DayStep.i=5          ##<< Window step for \code{\link{fLloydTaylor()}} regression in days
    ,TempRange.n=5        ##<< Threshold temperature range to start regression (#! Could be larger for Tair)
    ,NumE_0.n=3           ##<< Number of best E_0's to average over
    ,Trim.n=5             ##<< Percentile to trim residual (%)
    ,CallFunction.s=''    ##<< Name of function called from
  )
  ##author<<
  ## AMM
{
    'Estimation of the temperature sensitivity E_0 from regression of fLloydTaylor() for short periods'
    
    # Check if specified columns are numeric
    SubCallFunc.s <- paste(CallFunction.s,'sRegrE0fromShortTerm', sep=':::')
    fCheckColNames(cbind(sDATA,sTEMP), c(NightFlux.s, TempVar.s), SubCallFunc.s)
    fCheckColNum(cbind(sDATA,sTEMP), c(NightFlux.s, TempVar.s), SubCallFunc.s)
    
    # Regression settings
    NLSRes.F <- data.frame(NULL) #Results of non-linear regression
    NLSRes_trim.F <- data.frame(NULL) #Results of non-linear regression
    MinData.n <- 6 # Minimum number of data points
    
    # Write into vectors since cbind of data frames is so slow (factor of ~20 (!))
    NightFlux.V.n <- cbind(sDATA,sTEMP)[,NightFlux.s]
    TempVar.V.n <- cbind(sDATA,sTEMP)[,TempVar.s]
    
    # Loop regression periods
    DayCounter.V.i <- c(1:sINFO$DIMS) %/% sINFO$DTS
    CountRegr.i <- 0
    for (DayMiddle.i in seq(WinDays.i+1, max(DayCounter.V.i), DayStep.i)) {
      #TEST: DayMiddle.i <- 8
      DayStart.i <- DayMiddle.i-WinDays.i
      DayEnd.i <- DayMiddle.i+WinDays.i
      #! Window size of 7 days corresponds to full window length of 15 days as in paper, non-congruent with PV-Wave code of 14 days
      #! New code: Last window has minimum width of WinDays.i
      
      Subset.b <- DayCounter.V.i >= DayStart.i & DayCounter.V.i <= DayEnd.i & !is.na(NightFlux.V.n) & !is.na(TempVar.V.n)
      NEEnight.V.n <- subset(NightFlux.V.n, Subset.b)
      Temp.V.n <- subset(TempVar.V.n, Subset.b)
      Temp_degK.V.n <- fConvertCtoK(Temp.V.n)
      
      if( length(NEEnight.V.n) > MinData.n && diff(range(Temp_degK.V.n)) >= TempRange.n ) {
        CountRegr.i <- CountRegr.i+1
        tryCatch({
          # Non-linear regression
          NLS.L <- nls(formula=R_eco ~ fLloydTaylor(R_ref, E_0, Temp, T_ref.n=273.15+15), algorithm='default', trace=FALSE,
                       data=as.data.frame(cbind(R_eco=NEEnight.V.n,Temp=Temp_degK.V.n)), start=list(R_ref=2,E_0=200))        
          # Remove points with residuals outside Trim.n quantiles
          Residuals.V.n <- fLloydTaylor(R_ref=coef(summary(NLS.L))['R_ref',1], E_0=coef(summary(NLS.L))['E_0',1],
                                        Temp_degK.V.n, T_ref.n=273.15+15) - NEEnight.V.n
          t.b <- Residuals.V.n >= quantile(Residuals.V.n, probs=c(Trim.n/100)) & Residuals.V.n <= quantile(Residuals.V.n, probs=c(1-Trim.n/100))
          # Trimmed non-linear regression
          NLS_trim.L <- nls(formula=R_eco ~ fLloydTaylor(R_ref, E_0, Temp, T_ref.n=273.15+15), algorithm='default', trace=FALSE,
                            data=as.data.frame(cbind(R_eco=NEEnight.V.n[t.b], Temp=Temp_degK.V.n[t.b])), start=list(R_ref=2,E_0=200))
          
          NLSRes.F <- rbind(NLSRes.F, cbind(Start=DayStart.i, End=DayEnd.i, Num=length(NEEnight.V.n), TRange=diff(range(Temp_degK.V.n)),
                                            R_ref=coef(summary(NLS.L))['R_ref',1], R_ref_SD=coef(summary(NLS.L))['R_ref',2], 
                                            E_0=coef(summary(NLS.L))['E_0',1], E_0_SD=coef(summary(NLS.L))['E_0',2], 
                                            E_0_trim=coef(summary(NLS_trim.L))['E_0',1], E_0_trim_SD=coef(summary(NLS_trim.L))['E_0',2]))
          #! PV-Wave code has NLR with trimming: calculation of cost function and iterative trimming of the 10% highest absolute residuals.
          #! New code: Implemented as trimming of residuals outside 5% percentiles. Generally trimming questionable for non-linear regression.
          
          # Note on other tested algorithms:
          # Standard require(stats) nls with PORT algorithm and lower and upper bounds
          # require(FME) for modFit and modCost, has PORT algorithm included (and other algorithms like MCMC)
          # require(robustbase) for ltsReg but only linear regression
          # require(nlme) for heteroscedastic and mixed NLR but no port algo with upper and lower bounds
          # require(nlstools) for bootstrapping with nlsBoot(nls...)      
        }, error = function(e) {
          NLSRes.F <- rbind(NLSRes.F, cbind(Start=DayStart.i, End=DayEnd.i, Num=length(NEEnight.V.n), TRange=diff(range(Temp_degK.V.n)),
                                            R_ref=NA, R_ref_SD=NA, E_0=NA, E_0_SD=NA, E_0_trim=NA, E_0_trim_SD=NA))
        }   ) #Spaces between brackets required to avoid replacement on documentation generation
        
        if( F ) { # Plots for testing
          plot(NEEnight.V.n ~ Temp.V.n)
          curve(fLloydTaylor(coef(NLS.L)['R_ref'], coef(NLS.L)['E_0_trim'], fConvertCtoK(x), T_ref.n=273.15+15), add=T, col='red')
        }      
      }
    }
    
    # Check for validity of E_0 regression results
    if( grepl('Tair', TempVar.s) ) {
      #Limits in PV-Wave code for Tair
      MinE_0.n <- 30; MaxE_0.n <- 350
    } else if( grepl('Tsoil', TempVar.s) ) {
      #Limits in PV-Wave code for Tsoil
      MinE_0.n <- 30; MaxE_0.n <- 550 # Higher values due to potentially high Q10 values
    } else {
      #Default limits taken from paper
      MinE_0.n <- 30; MaxE_0.n <- 450
    }
    Limits.b <- ( NLSRes.F$E_0_trim - NLSRes.F$E_0_trim_SD > MinE_0.n & NLSRes.F$E_0_trim + NLSRes.F$E_0_trim_SD < MaxE_0.n )
    #! New code: Check validity with SD (standard deviation) limits, in PV-Wave without SD, in paper if E_0_SD < (E_0 * 50%)
    NLSRes.F$E_0_trim_ok <- ifelse( Limits.b, NLSRes.F$E_0_trim, NA)
    NLSRes.F$E_0_trim_SD_ok <- ifelse( Limits.b, NLSRes.F$E_0_trim_SD, NA)
    
    # Sort data frame for smallest standard deviation
    NLSsort.F <- NLSRes.F[order(NLSRes.F$E_0_trim_SD_ok),] # ordered data.frame  
    
    ##details<< 
    ## Take average of the three E_0 with lowest standard deviation
    E_0_trim.n <- round(mean(NLSsort.F$E_0_trim_ok[1:NumE_0.n]), digits=2) 
    
    # Abort flux partitioning if regression of E_0 failed
    if( is.na(E_0_trim.n) ) {
      warning(CallFunction.s, ':::sRegrE0fromShortTerm::: Less than ', NumE_0.n, ' valid values for E_0 after regressing ', 
              CountRegr.i, ' periods!')
      return(-111)
    }
    
    message('Estimate of the temperature sensitivity E_0 from short term data: ', format(E_0_trim.n, digits=5), '.')
    
    # Add constant value of E_0 as column vector to sTEMP
    E_0.V.n <- rep(E_0_trim.n, nrow(sTEMP))
    attr(E_0.V.n, 'varnames') <- 'E_0'
    attr(E_0.V.n, 'units') <- 'degK'
    
    E_0.V.n
    ##value<< 
    ## Data vector with (constant) temperature sensitivity (E_0, degK)
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sRegrRref = function(
    ##title<<
    ## sEddyProc$sRegrRref - Estimation of the reference respiration Rref
    ##description<<
    ## Estimation of the reference respiration Rref of \code{\link{fLloydTaylor()}} for successive periods
    NightFlux.s           ##<< Variable with (original) nighttime ecosystem carbon flux, i.e. respiration
    ,TempVar.s            ##<< Variable with (original) air or soil temperature (degC)
    ,E_0.s                ##<< Temperature sensitivity E_0 estimated with \code{\link{sRegrE0fromShortTerm}}
    ,WinDays.i=3          ##<< Window size for \code{\link{fLloydTaylor()}} regression in days
    ,DayStep.i=4          ##<< Window step for \code{\link{fLloydTaylor()}} regression in days
    ,CallFunction.s=''    ##<< Name of function called from
  )
  ##author<<
  ## AMM
{
    'Estimation of the reference respiration Rref of fLloydTaylor() for successive periods'
    
    # Check if specified columns are numeric
    SubCallFunc.s <- paste(CallFunction.s,'sRegrRref', sep=':::')
    fCheckColNames(cbind(sDATA,sTEMP), c(NightFlux.s, TempVar.s, E_0.s), SubCallFunc.s)
    fCheckColNum(cbind(sDATA,sTEMP), c(NightFlux.s, TempVar.s, E_0.s), SubCallFunc.s)
    
    # Regression settings
    LMRes.F <- data.frame(NULL) #Results of linear regression
    MinData.n <- 2 # Minimum number of data points for regression
    
    # Write into vectors since cbind of data frames is so slow (factor of ~20 (!))
    NightFlux.V.n <- cbind(sDATA,sTEMP)[,NightFlux.s]
    TempVar.V.n <- cbind(sDATA,sTEMP)[,TempVar.s]
    
    # Loop regression periods
    DayCounter.V.i <- c(1:sINFO$DIMS) %/% sINFO$DTS
    CountRegr.i <- 0
    for (DayMiddle.i in seq(WinDays.i+1, max(DayCounter.V.i), DayStep.i)) {
      #TEST: DayMiddle.i <- 8
      DayStart.i <- DayMiddle.i-WinDays.i
      DayEnd.i <- DayMiddle.i+WinDays.i
      #! Window size of 4 days corresponds to a full window length of 9 days, non-congruent with PV-Wave code of 8 days, in paper not mentioned
      #! New code: Last window has minimum of window size
      
      Subset.b <- DayCounter.V.i >= DayStart.i & DayCounter.V.i <= DayEnd.i & !is.na(NightFlux.V.n) & !is.na(TempVar.V.n)
      MeanHour.i <- round(mean(which(Subset.b))) # Weighted middle of the time period
      NEEnight.V.n <- subset(NightFlux.V.n, Subset.b)
      Temp.V.n <- subset(TempVar.V.n, Subset.b)
      Temp_degK.V.n <- fConvertCtoK(Temp.V.n)
      E_0.V.n <- subset(sTEMP[,E_0.s], Subset.b) # (Constant value)
      
      if( length(NEEnight.V.n) > MinData.n ) {
        CountRegr.i <- CountRegr.i+1
        tryCatch({
          LM.L <- lm(R_eco ~ 0 + fLloydTaylor(R_ref, E_0, Temp_degK, T_ref.n=273.15+15), data=as.data.frame(cbind(R_eco=NEEnight.V.n, R_ref=1, E_0=E_0.V.n, Temp_degK=Temp_degK.V.n)))
          LMRes.F <- rbind(LMRes.F, cbind(Start=DayStart.i, End=DayEnd.i, Num=length(NEEnight.V.n), MeanH=MeanHour.i, 
                                          R_ref=coef(summary(LM.L))[1], R_ref_SD=coef(summary(LM.L))[2]))
          #! Note on PV-Wave code: trimmed linear regression not used in the end, i.e. in online webtool
          
          if( F ) { # Plot for testing
            plot(NEEnight.V.n ~ fLloydTaylor(1, E_0.V.n, Temp_degK.V.n, T_ref.n=273.15+15))
            curve(coef(LM.L)[1] * x, add=T, col='green')
          }  
        }, error = function(e) {
          LMRes.F <- rbind(LMRes.F, cbind(Start=DayStart.i, End=DayEnd.i, Num=length(NEEnight.V.n), MeanH=MeanHour.i, 
                                          R_ref=NA, R_ref_SD=NA))
        }   )  #Spaces between brackets required to avoid replacement on documentation generation
      }
    }
    
    # Check for validity of regression results
    LMRes.F$R_ref_ok <- ifelse( LMRes.F$R_ref < 0 , NA, LMRes.F$R_ref )
    #! New code: Omit regressions with R_ref <0, in PV-Wave smaller values are set to 0.000001, not mentioned in paper
    #TODO later: Flag for long distances between R_refs, especially if long distance in the beginning
    #TODO later: Provide some kind of uncertainty estimate from R_ref_SD
    
    # Interpolate R_ref periods linearly between MeanHour.i and keep constant at beginning/end
    Rref.V.n <- rep(NA, length(NightFlux.V.n))
    Rref.V.n[LMRes.F$MeanH] <- LMRes.F$R_ref_ok
    Rref.V.n <- fInterpolateGaps(Rref.V.n)
    attr(Rref.V.n, 'varnames') <- 'R_ref'
    attr(Rref.V.n, 'units') <- attr(sTEMP[,NightFlux.s], 'units')
    
    message('Regression of reference temperature R_ref for ', sum(!is.na(LMRes.F$R_ref_ok)), ' periods.')
    
    Rref.V.n
    ##value<< 
    ## Data vector with reference respiration (R_ref, flux units)
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sMRFluxPartition = function(
    ##title<<
    ## sMRFluxPartition - Flux partitioning after Reichstein et al. (2005)
    ##description<<
    ## Nighttime-based partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco)
    FluxVar.s=paste0('NEE',suffix.s,'_f')       ##<< Variable of net ecosystem fluxes
    ,QFFluxVar.s=paste0('NEE',suffix.s,'_fqc')  ##<< Quality flag of variable
    ,QFFluxValue.n=0       						##<< Value of quality flag for _good_ (original) data
    ,TempVar.s=paste0('Tair',suffix.s,'_f')     ##<< Filled air or soil temperature variable (degC)
    ,QFTempVar.s=paste0('Tair',suffix.s,'_fqc') ##<< Quality flag of filled temperature variable
    ,QFTempValue.n=0       ##<< Value of temperature quality flag for _good_ (original) data
    ,RadVar.s='Rg'         ##<< Unfilled (original) radiation variable
    ,Lat_deg.n             ##<< Latitude in (decimal) degrees
    ,Long_deg.n            ##<< Longitude in (decimal) degrees
    ,TimeZone_h.n          ##<< Time zone (in hours)
	,suffix.s = ""		   ##<< string inserted into column names before identifier (see \code{\link{sMDSGapFillUStar}}). 
  )
  ##author<<
  ## AMM
  ##references<<
  ## Reichstein M, Falge E, Baldocchi D et al. (2005) On the separation of net ecosystem exchange 
  ## into assimilation and ecosystem respiration: review and improved algorithm. Global Change Biology, 11, 1424-1439.
{
    'Partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco)'
    # Check if specified columns exist in sDATA or sTEMP and if numeric and plausible. Then apply quality flag
    fCheckColNames(cbind(sDATA,sTEMP), c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sMRFluxPartition')
    fCheckColNum(cbind(sDATA,sTEMP), c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sMRFluxPartition')
    fCheckColPlausibility(cbind(sDATA,sTEMP), c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sMRFluxPartition')
    Var.V.n <- fSetQF(cbind(sDATA,sTEMP), FluxVar.s, QFFluxVar.s, QFFluxValue.n, 'sMRFluxPartition')
    
    message('Start flux partitioning for variable ', FluxVar.s, ' with temperature ', TempVar.s, '.')
    
    # Calculate potential radiation
    #! New code: Local time and equation of time accounted for in potential radiation calculation
    DoY.V.n <- as.numeric(format(sDATA$sDateTime, '%j'))
    Hour.V.n <- as.numeric(format(sDATA$sDateTime, '%H')) + as.numeric(format(sDATA$sDateTime, '%M'))/60
    sTEMP$NEW_PotRad <<- fCalcPotRadiation(DoY.V.n, Hour.V.n, Lat_deg.n, Long_deg.n, TimeZone_h.n)
    
    # Filter night time values only
    #! Note: Rg <= 10 congruent with MR PV-Wave, in paper Rg <= 20
    # Should be unfilled (original) radiation variable, therefore dataframe set to sDATA only
    sTEMP$FP_VARnight <<- ifelse(sDATA[,RadVar.s] > 10 | sTEMP$NEW_PotRad > 0, NA,  Var.V.n)
    attr(sTEMP$FP_VARnight, 'varnames') <<- paste(attr(Var.V.n, 'varnames'), '_night', sep='')
    attr(sTEMP$FP_VARnight, 'units') <<- attr(Var.V.n, 'units')
    #! New code: Slightly different subset than PV-Wave due to time zone correction (avoids timezone offset between Rg and PotRad)
    
    # Apply quality flag for temperature
    sTEMP$NEW_FP_Temp <<- fSetQF(cbind(sDATA,sTEMP), TempVar.s, QFTempVar.s, QFTempValue.n, 'sMRFluxPartition')
    
    # Estimate E_0 and R_ref (results are saved in sTEMP)
    sTEMP$NEW_E_0 <<- sRegrE0fromShortTerm('FP_VARnight', 'NEW_FP_Temp', CallFunction.s='sMRFluxPartition')
    if( sum(sTEMP$NEW_E_0==-111) != 0 )
      return(invisible(-111)) # Abort flux partitioning if regression of E_0 failed
    
    # Reanalyse R_ref with E_0 fixed
    sTEMP$NEW_R_ref <<- sRegrRref('FP_VARnight', 'NEW_FP_Temp', 'NEW_E_0', CallFunction.s='sMRFluxPartition')
    
    # Calculate the ecosystem respiration Reco
    sTEMP$NEW_Reco <<- fLloydTaylor(sTEMP$NEW_R_ref, sTEMP$NEW_E_0, fConvertCtoK(cbind(sDATA,sTEMP)[,TempVar.s]), T_ref.n=273.15+15)
    attr(sTEMP$NEW_Reco, 'varnames') <<- 'Reco'
    attr(sTEMP$NEW_Reco, 'units') <<- attr(Var.V.n, 'units')
    
    # Calculate the gross primary production GPP_f
    sTEMP$NEW_GPP_f <<- -cbind(sDATA,sTEMP)[,FluxVar.s] + sTEMP$NEW_Reco
    sTEMP$NEW_GPP_fqc <<- cbind(sDATA,sTEMP)[,QFFluxVar.s]
    #! New code: MDS gap filling information are not copied from NEE_fmet and NEE_fwin to GPP_fmet and GPP_fwin
    #           (since not known within this pure partitioning function)
    attr(sTEMP$NEW_GPP_f, 'varnames') <<- 'GPP_f'
    attr(sTEMP$NEW_GPP_f, 'units') <<- attr(Var.V.n, 'units')
    
    # Rename new columns generated during flux partitioning
    colnames(sTEMP) <<- gsub('_VAR', '_NEE', colnames(sTEMP))
    colnames(sTEMP) <<- gsub('NEW_', '', colnames(sTEMP))
    
	# TODO: Adjust all output columns to account for suffix
	
    ##details<<
    ## Description of newly generated variables with partitioning results: \cr
    ## PotRad - Potential radiation \cr
    ## FP_NEEnight - Good (original) NEE nighttime fluxes  used for flux partitioning \cr
    ## FP_Temp - Good (original) temperature measurements used for flux partitioning \cr
    ## E_0 - Estimated temperature sensitivity \cr
    ## R_ref - Estimated reference respiration \cr
    ## Reco - Estimated ecosystem respiration \cr
    ## GPP_f - Estimated gross primary production \cr
    
    
    return(invisible(NULL))
    ##value<< 
    ## Flux partitioning results in sTEMP data frame (with renamed columns).
  })
