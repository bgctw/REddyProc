#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc methods for flux partitioning +++
#+++ Flux partitionig algorithm, adapted after the PV-Wave code and paper by Markus Reichstein +++
#+++ Dependencies: Eddy.R, DataFunctions.R
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#TEST: FluxVar.s <- 'NEE_f'; QFFluxVar.s <- 'NEE_fqc'; QFFluxValue.n <- 0; TempVar.s <- 'Tair_f'; QFTempVar.s <- 'Tair_fqc'; QFTempValue.n <- 0
#TEST: RadVar.s <- 'Rg'; Lat_deg.n <- 51.0; Long_deg.n <- 13.6; TimeZone_h.n <- 1.0; CallFunction.s='test'
#TEST: NightFlux.s='VAR_night';  TempVar.s='NEW_TempFP'; WinDays.i=7; DayStep.i=5; TempRange.n=5; 
#TEST: NightFlux.s='VAR_night';  TempVar.s='NEW_TempFP'; E_0.s='NEW_E_0'; WinDays.i=4; DayStep.i=4;

sRegrE0fromShortTerm <- function(
  ##title<<
  ## sEddyProc$sRegrE0fromShortTerm - Estimation of the temperature sensitivity E_0 from short term data
  ##description<<
  ## Estimation of the temperature sensitivity E_0 from regression of \code{\link{fLloydTaylor()}} for short periods
  NightFlux.s           ##<< Variable with (original) nighttime ecosystem carbon flux, i.e. respiration
  ,TempVar.s            ##<< Variable with (original) air temperature (degC)
  ,WinDays.i=7          ##<< Window size for \code{\link{fLloydTaylor()}} regression in days
  ,DayStep.i=5          ##<< Window step for \code{\link{fLloydTaylor()}} regression in days
  ,TempRange.n=5        ##<< Threshold temperature range to start regression (#! Could be larger for Tair)
  ,CallFunction.s=''    ##<< Name of function called from
  ##author<<
  ## AMM
)
{
  'Estimation of the temperature sensitivity E_0 from regression of fLloydTaylor() for short periods'
  
  # Check if specified columns are numeric
  SubCallFunc.s <- paste(CallFunction.s,'sRegrE0fromShortTerm', sep=':::')
  fCheckColNames(cbind(sDATA,sTEMP), c(NightFlux.s, TempVar.s), SubCallFunc.s)
  fCheckColNum(cbind(sDATA,sTEMP), c(NightFlux.s, TempVar.s), SubCallFunc.s)
  
  # Regression settings
  NLSRes.F <- data.frame(NULL) #Results of non-linear regression
  MinData.n <- 6 # Minimum number of data points
  MinR_ref.n <- 0.000001 # Minimum reference temperature (non-zero limit for regression)
  MaxR_ref.n <- 200 # Maximum reference temperature 
  if( grepl('Tair', TempVar.s) ) {
    #Limits in PV-Wave code for Tair
    MinE_0.n <- 30
    MaxE_0.n <- 350
  } else if( grepl('Tsoil', TempVar.s) ) {
    #Limits in PV-Wave code for Tsoil
    MinE_0.n <- 30
    MaxE_0.n <- 550 # Higher values due to potentially high Q10 values
  } else {
    #Default limits taken from paper
    MinE_0.n <- 30 
    MaxE_0.n <- 450
  }
  
  # Write into vectors since cbind of data frames is so slow (factor of ~20 (!))
  NightFlux.V.n <- cbind(sDATA,sTEMP)[,NightFlux.s]
  TempVar.V.n <- cbind(sDATA,sTEMP)[,TempVar.s]
  
  # Loop regression periods
  DayCounter.V.i <- c(1:sINFO$DIMS) %/% sINFO$DTS
  for (DayMiddle.i in seq(WinDays.i+1, max(DayCounter.V.i), DayStep.i)) {
    #TEST: DayMiddle.i <- 8
    DayStart.i <- DayMiddle.i-WinDays.i
    DayEnd.i <- DayMiddle.i+WinDays.i
    #! Window size of 7 days corresponds to full window length of 15 days as in paper, non-congruent with PV-Wave code of 14 days
    #! New code: Last window has minimum width of WinDays.i
    
    Subset.b <- DayCounter.V.i >= DayStart.i & DayCounter.V.i <= DayEnd.i & !is.na(NightFlux.V.n) & !is.na(TempVar.V.n)
    NEEnight.V.n <- subset(NightFlux.V.n, Subset.b)
    Temp.V.n <- subset(TempVar.V.n, Subset.b)
    Temp_degK.V.n <- suppressMessages(fConvertCtoK(Temp.V.n))
    
    if( length(NEEnight.V.n) > MinData.n && diff(range(Temp_degK.V.n)) >= TempRange.n ) {
      tryCatch({
        NLS.L <- nls(formula=R_eco ~ fLloydTaylor(R_ref, E_0, Temp), algorithm='port', trace=FALSE,
                     data=as.data.frame(cbind(R_eco=NEEnight.V.n,Temp=Temp_degK.V.n)), start=list(R_ref=2,E_0=200), 
                     lower=list(R_ref=MinR_ref.n,E_0=MinE_0.n), upper=list(R_ref= MaxR_ref.n,E_0=MaxE_0.n))
        NLSRes.F <- rbind(NLSRes.F, cbind(Start=DayStart.i, End=DayEnd.i, Num=length(NEEnight.V.n), TRange=diff(range(Temp_degK.V.n)),
                                          R_ref=coef(summary(NLS.L))['R_ref',1], R_ref_SD=coef(summary(NLS.L))['R_ref',2], 
                                          E_0=coef(summary(NLS.L))['E_0',1], E_0_SD=coef(summary(NLS.L))['E_0',2]))
        
        #! PV-Wave code has NLR with trimming: calculation of cost function and iterative trimming of the 10% highest absolute residuals
        #! Omitted since not critical for the regression of finding the best three E_0 (and trimming questionable for non-linear regression)
        #! New code: NLR performed with PORT algorithm with lower and upper limit bounds, whereas in PV-Wave invalid values omitted after NLR
        
        # Note on other tested algorithms:
        # require(FME) for modFit and modCost, has PORT algorithm included (and other algorithms like MCMC)
        # require(robustbase) for ltsReg but only linear regression
        # require(nlme) for heteroscedastic and mixed NLR but no port algo with upper and lower bounds
        # require(nlstools) for bootstrapping with nlsBoot(nls...)
        
      }, error = function(e) {
        NLSRes.F <- rbind(NLSRes.F, cbind(Start=DayStart.i, End=DayEnd.i, Num=length(NEEnight.V.n), TRange=diff(range(Temp_degK.V.n)),
                                          R_ref=NA, R_ref_SD=NA, E_0=NA, E_0_SD=NA))
      })
      
      if( F ) { # Plots for testing
        plot(NEEnight.V.n ~ Temp.V.n)
        curve(fLloydTaylor(coef(NLS.L)['R_ref'], coef(NLS.L)['E_0'], suppressMessages(fConvertCtoK(x))), add=T, col='red')
      }      
    }
  }
  
  # Check for validity of regression results
  #! New code: Estimate of E_0 only valid if (E_0 - E_0_SD) > MinE_0.n, in paper if E_0_SD < (E_0 * 50%)
  NLSRes.F$E_0_ok <- ifelse(NLSRes.F$E_0 - NLSRes.F$E_0_SD > MinE_0.n, NLSRes.F$E_0, NA)
  NLSRes.F$E_0_SD_ok <- ifelse(NLSRes.F$E_0 - NLSRes.F$E_0_SD > MinE_0.n, NLSRes.F$E_0_SD, NA)
  
  # Sort data frame for smallest standard deviation
  NLSsort.F <- NLSRes.F[order(NLSRes.F$E_0_SD_ok),] # ordered data.frame  
  ##details<< 
  ## Take average of the three E_0 with lowest standard deviation
  E_0.n <- mean(NLSsort.F$E_0_ok[1:3])
  
  #TODO: !!! warning if E_0 is NA that regression did not work
  message('Estimate of the temperature sensitivity E_0 from short term data: ', format(E_0.n, digits=5), '.')
  
  # Add constant value of E_0 as column vector to sTEMP
  sTEMP$NEW_E_0 <<- rep(E_0.n, nrow(sTEMP))
  attr(sTEMP$NEW_E_0, 'varnames') <<- 'E_0'
  attr(sTEMP$NEW_E_0, 'units') <<- 'degK'
  
  return(invisible(NULL))
  ##value<< 
  ## Calculated value NEW_E_0 as column in sTEMP data frame.
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sRegrRref <- function(
  ##title<<
  ## sEddyProc$sRegrRref - Estimation of the reference respiration Rref
  ##description<<
  ## Estimation of the reference respiration Rref of \code{\link{fLloydTaylor()}} for successive periods
  NightFlux.s           ##<< Variable with (original) nighttime ecosystem carbon flux, i.e. respiration
  ,TempVar.s            ##<< Variable with (original) air temperature (degC)
  ,E_0.s                ##<< Temperature sensitivity E_0 estimated with \code{\link{sRegrE0fromShortTerm}}
  ,WinDays.i=4          ##<< Window size for \code{\link{fLloydTaylor()}} regression in days !!! ??? 3 days ???
  ,DayStep.i=4          ##<< Window step for \code{\link{fLloydTaylor()}} regression in days
  ,CallFunction.s=''    ##<< Name of function called from
  ##author<<
  ## AMM
)
{
  'Estimation of the reference respiration Rref of fLloydTaylor() for successive periods'
  
  # Check if specified columns are numeric
  SubCallFunc.s <- paste(CallFunction.s,'sRegrRref', sep=':::')
  fCheckColNames(cbind(sDATA,sTEMP), c(NightFlux.s, TempVar.s, E_0.s), SubCallFunc.s)
  fCheckColNum(cbind(sDATA,sTEMP), c(NightFlux.s, TempVar.s, E_0.s), SubCallFunc.s)
  
  # Regression settings
  LMRes.F <- data.frame(NULL) #Results of linear regression
  MinR_ref.n <- 0.000001 # Minimum reference temperature (positive, non-zero limit AFTER regression)
  MinData.n <- 2 # Minimum number of data points for regression
  
  # Write into vectors since cbind of data frames is so slow (factor of ~20 (!))
  NightFlux.V.n <- cbind(sDATA,sTEMP)[,NightFlux.s]
  TempVar.V.n <- cbind(sDATA,sTEMP)[,TempVar.s]
  
  # Loop regression periods
  DayCounter.V.i <- c(1:sINFO$DIMS) %/% sINFO$DTS
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
    Temp_degK.V.n <- suppressMessages(fConvertCtoK(Temp.V.n))
    E_0.V.n <- subset(sTEMP[,E_0.s], Subset.b) # (Constant value)
    
    if( length(NEEnight.V.n) > MinData.n ) {
      tryCatch({
        if( F ) { #!!! To delete later, for testing with nls... (same speed, almost same results)
          # Linear regression of R_ref in fLloydTaylor (with R_ref=1 to calculate only temperature term of fLloydTaylor)
          NLS.L <- nls(formula=R_eco ~ R_ref_l * fLloydTaylor(R_ref=1, E_0, Temp_degK), algorithm='port', trace=FALSE,
                       data=as.data.frame(cbind(R_eco=NEEnight.V.n, E_0=E_0.V.n, Temp_degK=Temp_degK.V.n)), start=list(R_ref_l=2), 
                       lower=list(R_ref_l=MinR_ref.n))
          LMRes.F <- rbind(LMRes.F, cbind(Start=DayStart.i, End=DayEnd.i, Num=length(NEEnight.V.n), MeanH=MeanHour.i, 
                                          R_ref=coef(summary(NLS.L))[1], R_ref_SD=coef(summary(NLS.L))[2]))
        } else {
          LM.L <- lm(R_eco ~ 0 + fLloydTaylor(R_ref, E_0, Temp_degK), data=as.data.frame(cbind(R_eco=NEEnight.V.n, R_ref=1, E_0=E_0.V.n, Temp_degK=Temp_degK.V.n)))
          LMRes.F <- rbind(LMRes.F, cbind(Start=DayStart.i, End=DayEnd.i, Num=length(NEEnight.V.n), MeanH=MeanHour.i, 
                                          R_ref=coef(summary(LM.L))[1], R_ref_SD=coef(summary(LM.L))[2]))
        }
        
        #! Note on PV-Wave code: trimmed linear regression not used in the end, i.e. online tool
        
        if( F==T) { # Plot during tests
          plot(NEEnight.V.n ~ fLloydTaylor(1, E_0.V.n, Temp_degK.V.n))
          curve(coef(LM.L)[1] * x, add=T, col='green')
        }  
      }, error = function(e) {
        LMRes.F <- rbind(LMRes.F, cbind(Start=DayStart.i, End=DayEnd.i, Num=length(NEEnight.V.n), MeanH=MeanHour.i, 
                                        R_ref=NA, R_ref_SD=NA))
      })
    }
  }
  
  # Check for validity of regression results
  LMRes.F$R_ref_ok <- ifelse(LMRes.F$R_ref - LMRes.F$R_ref_SD > MinR_ref.n, LMRes.F$R_ref, NA)
  #! New code: Accounting for standard deviation !!! ??? same as E_0 ???
  #TODO later: Flag for long distances between R_refs, especially if long distance in the beginning
  #TODO later: Provide some kind of uncertainty estimate from R_ref_SD
  
  # Interpolate R_ref periods linearly between MeanHour.i and keep constant at beginning/end 
  sTEMP$NEW_R_ref <<- NA
  sTEMP$NEW_R_ref[LMRes.F$MeanH] <<- LMRes.F$R_ref_ok
  sTEMP$NEW_R_ref <<- fInterpolateGaps(sTEMP$NEW_R_ref)
  
  attr(sTEMP$NEW_R_ref, 'varnames') <<- 'R_ref'
  attr(sTEMP$NEW_R_ref, 'units') <<- attr(sTEMP[,NightFlux.s], 'units')
  
  message('Estimated reference temperature R_ref for ', nrow(LMRes.F), ' periods.')
  
  return(invisible(NULL))
  ##value<< 
  ## Reference temperature NEW_R_ref as column in sTEMP data frame.
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sMRFluxPartition <- function(
  ##title<<
  ## sMRFluxPartition - Flux partitioning after Reichstein et al. (2005)
  ##description<<
  ## Partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco)
  FluxVar.s='NEE_f'     ##<< Variable of net ecosystem fluxes
  ,QFFluxVar.s='NEE_fqc'##<< Quality flag of variable
  ,QFFluxValue.n=0      ##<< Value of quality flag for _good_ (original) data
  ,TempVar.s='Tair_f'     ##<< Temperature variable (degC)
  ,QFTempVar.s='Tair_fqc'##<< Quality flag of variable
  ,QFTempValue.n=0      ##<< Value of quality flag for _good_ (original) data
  ,RadVar.s='Rg'        ##<< (Unfilled) Radiation variable
  ,Lat_deg.n            ##<< Latitude in (decimal) degrees
  ,Long_deg.n           ##<< Longitude in (decimal) degrees
  ,TimeZone_h.n         ##<< Time zone (in hours)
  ##author<<
  ## AMM
  ##references<<
  ## Reichstein M, Falge E, Baldocchi D et al. (2005) On the separation of net ecosystem exchange 
  ## into assimilation and ecosystem respiration: review and improved algorithm. Global Change Biology, 11, 1424-1439.
)
{
  'Partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco)'
  # Check if specified columns are numeric and plausible and apply quality flag
  fCheckColNames(sDATA, c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sMRFluxPartition')
  fCheckColNum(sDATA, c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sMRFluxPartition')
  fCheckColPlausibility(sDATA, c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sMRFluxPartition')
  Var.V.n <- fSetQF(sDATA, FluxVar.s, QFFluxVar.s, QFFluxValue.n, 'sMRFluxPartition')
  
  # Calculate potential radiation
  #! New code: Local time and equation of time accounted for in potential radiation calculation
  DoY.V.n <- as.numeric(format(sDATA$sDateTime, '%j'))
  Hour.V.n <- as.numeric(format(sDATA$sDateTime, '%H')) + as.numeric(format(sDATA$sDateTime, '%M'))/60
  sTEMP$NEW_PotRad <- fCalcPotRadiation(DoY.V.n, Hour.V.n, Lat_deg.n, Long_deg.n, TimeZone_h.n)
  
  # Filter night time values only
  #! Note: Rg <= 10 congruent with MR PV-Wave, in paper Rg <= 20
  sTEMP$VAR_night <- ifelse(sDATA[,RadVar.s] < 10 | sTEMP$NEW_PotRad == 0, Var.V.n, NA)
  attr(sTEMP$VAR_night, 'varnames') <- paste(attr(Var.V.n, 'varnames'), '_night', sep='')
  attr(sTEMP$VAR_night, 'units') <- attr(Var.V.n, 'units')
  
  sTEMP$VAR_night_MR <- sDATA$NEENight #!!!
  
  # Apply quality flag for temperature
  sTEMP$NEW_TempFP <- fSetQF(sDATA, TempVar.s, QFTempVar.s, QFTempValue.n, 'sMRFluxPartition')
  
  # Estimate E_0 and R_ref (results are saved in sTEMP)
  sRegrE0fromShortTerm('VAR_night', 'NEW_TempFP', CallFunction.s='sMRFluxPartition')  
  sTEMP$NEW_E_0_MR <- rep(133, nrow(sTEMP)) #!!!
  
  sRegrRref('VAR_night', 'NEW_TempFP', 'NEW_E_0', CallFunction.s='sMRFluxPartition')
  #!!! sRegrRref('VAR_night_MR', 'NEW_TempFP', 'NEW_E_0_MR', WinDays.i=2, CallFunction.s='sMRFluxPartition')
  
  # Calculate the ecosystem respiration R_eco
  sTEMP$NEW_Reco <- fLloydTaylor(sTEMP$NEW_R_ref, sTEMP$NEW_E_0, suppressMessages(fConvertCtoK(sDATA[,TempVar.s])))  
  #!!! set units
  
  # Calculate the gross primary production GPP_f
  sTEMP$NEW_GPP_f <- -sDATA[,FluxVar.s] + sTEMP$NEW_Reco
  #!!! Set quality flag of NEE_f for GPP_f: GPP_fqc  GPP_fmet  GPP_fwin, same as NEE_fqc...
  #!!! set units
  
  # Rename new columns generated during flux partitioning
  colnames(sTEMP) <- gsub('VAR', 'NEE', colnames(sTEMP)) #!!! set proper variable name
  colnames(sTEMP) <- gsub('NEW_', '', colnames(sTEMP))
  
  message('... Flux partitioning...')
  
  return(invisible(NULL))
  ##value<< 
  ## Flux partitioning results in sTEMP data frame (with renamed columns).
}

#!!! ??? How similar should the results look ???
# ??? criteria for testing ???
