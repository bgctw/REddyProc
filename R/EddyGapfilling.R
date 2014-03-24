#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc methods for gap filling +++
#+++ MDS gap filling algorithm, adapted after the PV-Wave code and paper by Markus Reichstein +++
#+++ Dependencies: Eddy.R, DataFunctions.R
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TEST: sDATA <- EPTha.C$sDATA; sINFO <- EPTha.C$sINFO; sTEMP <- EPTha.C$sTEMP; Var.s <- 'NEE'; QFVar.s <- 'none'; QFValue.n <- NA_real_;
# TEST: V1.s <- 'Rg'; T1.n <- 50; V2.s <- 'VPD'; T2.n <- 5; V3.s <- 'Tair'; T3.n <- 2.5; FillAll.b <- TRUE; Verbose.b <- TRUE
# TEST: V4.s='none'; T4.n=NA_real_; V5.s='none'; T5.n=NA_real_; sTEMP <- NULL

sEddyProc$methods(
  sFillInit = function(
    ##title<<
    ## sEddyProc$sFillInit - Initialize gap filling
    ##description<<
    ## Initializes data frame sTEMP for newly generated gap filled data and qualifiers.
    Var.s                 ##<< Variable to be filled
    ,QFVar.s='none'       ##<< Quality flag of variable to be filled
    ,QFValue.n=NA_real_   ##<< Value of quality flag for _good_ (original) data, other data is set to missing
    ,FillAll.b=TRUE       ##<< Fill all values to estimate uncertainties
    ,QF.V.b = TRUE        ##<< boolean vector of length nRow(sData), to allow specifying bad data directly (those entries that are set to FALSE)
  )
    ##author<<
    ## AMM
  {
    'Initializes data frame sTEMP for newly generated gap filled data and qualifiers.'
    
    # Check variable to fill and apply quality flag
    fCheckColNames(cbind(sDATA,sTEMP), c(Var.s, QFVar.s), 'sFillInit')
    Var.V.n <- fSetQF(cbind(sDATA,sTEMP), Var.s, QFVar.s, QFValue.n, 'sFillInit')
    Var.V.n[QF.V.b == FALSE] <- NA_real_
    
    # Abort if variable to be filled contains no data
    if( sum(!is.na(Var.V.n)) == 0 ) {
      warning('sFillInit::: Variable to be filled (', Var.s, ') contains no data at all!')
      return(-111)
    }
    ##details<<
    ## Description of newly generated variables with gap filled data and qualifiers: \cr
    ##details<<
    ## VAR\emph{_orig} - Original values used for gap filling \cr
    ## VAR\emph{_f   } - Original values and gaps filled with mean of selected datapoints (condition depending on gap filling method) \cr
    ## VAR\emph{_fqc} - Quality flag assigned depending on gap filling method and window length (0 = original data, 1 = most reliable, 2 = medium, 3 = least reliable) \cr
    ## VAR\emph{_fall} - All values considered as gaps (for uncertainty estimates) \cr
    ## VAR\emph{_fall_qc} - Quality flag assigned depending on gap filling method and window length (1 = most reliable, 2 = medium, 3 = least reliable) \cr
    ## VAR\emph{_fnum} - Number of datapoints used for gap-filling \cr
    ## VAR\emph{_fsd} - Standard deviation of datapoints used for gap filling (uncertainty) \cr
    ## VAR\emph{_fmeth} - Method used for gap filling (1 = similar meteo condition (sFillLUT with Rg, VPD, Tair), 2 = similar meteo (sFillLUT with Rg only), 3 = mean diurnal course (sFillMDC)) \cr
    ## VAR\emph{_fwin} - Full window length used for gap filling \cr

    lTEMP <- data.frame(
      VAR_orig=Var.V.n               # Original values of variable VAR used for gap filling
      ,VAR_f=NA_real_                # Original values and filled gaps
      ,VAR_fqc=NA_real_              # Quality flag assigned depending on gap filling method and window length
      ,VAR_fall=NA_real_             # All values considered as gaps (for uncertainty estimates)
      ,VAR_fall_qc=NA_real_          # Quality flag assigned depending on gap filling method and window length
      ,VAR_fnum=NA_real_             # Number of datapoints used for gap-filling
      ,VAR_fsd=NA_real_              # Standard deviation of data points used for filling
      ,VAR_fmeth=NA_real_            # Method used for gap filling
      ,VAR_fwin=NA_real_             # Full window length used for gap filling
    )

    # Set fqc to zero for original values
    lTEMP$VAR_f <- lTEMP$VAR_orig
    lTEMP$VAR_fqc <- ifelse(!is.na(lTEMP$VAR_orig), 0, NA_real_)
    
    # Set filling of only gaps
    if( FillAll.b==FALSE) lTEMP$VAR_fall <- lTEMP$VAR_orig #"prefill" with original data
    
    # Add units
    attr(lTEMP$VAR_f, 'units') <- attr(Var.V.n, 'units')
    attr(lTEMP$VAR_fall, 'units') <- attr(Var.V.n, 'units')
    attr(lTEMP$VAR_fsd, 'units') <- attr(Var.V.n, 'units')
    attr(lTEMP$VAR_fwin, 'units') <- 'days'
    
    ##details<<
    ## Long gaps (larger than 60 days) are not filled.
    #! Not congruent with PV-Wave, there the code is performed on single years only with long gaps of 60 days in the beginning or end skipped.
    GapLength.V.n <- fCalcLengthOfGaps(lTEMP$VAR_orig)
    kMaxGap.n <- sINFO$DTS * 60 #Halfhours in 60 days
    while ( max(GapLength.V.n) > kMaxGap.n ) {
      #Flag long gap with -9999.0
      End.i <- which(GapLength.V.n == max(GapLength.V.n))
      Start.i <- End.i - max(GapLength.V.n) + 1
      lTEMP$VAR_fall[Start.i:End.i] <- -9999.0 #Set to -9999.0 as a flag for long gaps
      GapLength.V.n[Start.i:End.i] <- -1 #Set to -1 since accounted for
      warning('sMDSGapFill::: The long gap between position ', Start.i, ' and ', End.i, ' will not be filled!')
    }
    
    if( FillAll.b==T ) {
      message('Initialized variable \'', Var.s, '\' with ', sum(is.na(lTEMP$VAR_orig)), 
              ' real gaps for gap filling of all ', sum(is.na(lTEMP$VAR_fall)) ,' values (to estimate uncertainies).')
    } else {
      message('Initialized variable \'', Var.s, '\' with ', sum(is.na(lTEMP$VAR_orig)),
              ' real gaps for gap filling.')
    }
    
    sTEMP <<- data.frame(c(sTEMP, lTEMP))
    return(invisible(NULL))
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sFillLUT = function(
    ##title<<
    ## sEddyProc$sFillLUT - Gap filling with Look-Up Table (LUT)
    ##description<<
    ## Look-Up Table (LUT) algorithm of up to five conditions within prescribed window size
    WinDays.i             ##<< Window size for filling in days
    ,V1.s='none'          ##<< Condition variable 1
    ,T1.n=NA_real_        ##<< Tolerance interval 1
    ,V2.s='none'          ##<< Condition variable 2
    ,T2.n=NA_real_        ##<< Tolerance interval 2
    ,V3.s='none'          ##<< Condition variable 3
    ,T3.n=NA_real_        ##<< Tolerance interval 3
    ,V4.s='none'          ##<< Condition variable 4
    ,T4.n=NA_real_        ##<< Tolerance interval 4
    ,V5.s='none'          ##<< Condition variable 5
    ,T5.n=NA_real_        ##<< Tolerance interval 5
    ,Verbose.b=TRUE       ##<< Print status information to screen
  )
  ##author<<
  ## AMM
  #TEST: WinDays.i <- 7; Pos.i <- 18;
{
    'Look-Up Table (LUT) algorithm of up to five conditions within prescribed window size'
    
    #! Attention: For performance reasons, gap filled values and properties are first written to single variables and local matrix lGF.M
    #! (rather than changing single values in sTEMP which copies the data frame each time!)
    #! Improved algorithm speed by more than a factor of 10 (maybe even 100...)
    lGF.M <- matrix(NA_real_, nrow=0, ncol=7, dimnames=list(NULL,c('index','mean','fnum','fsd','fmeth','fwin','fqc')))
    
    # Check if sTEMP has been initialized with new VAR_ columns
    if( !exists('VAR_f', sTEMP) )
        stop('sFillLUT::: Temporal data frame sTEMP for processing results has not been initalized with sFillInit!')
    
    # Determine gap positions
    ToBeFilled.V.i <- which(is.na(sTEMP$VAR_fall))
    if( length(ToBeFilled.V.i) > 0 ) {
      for( Pos.i in 1:length(ToBeFilled.V.i) ) {
        # Message on progress if wanted
        NoneCols.b <- c(V1.s, V2.s, V3.s, V4.s, V5.s) %in% 'none' 
        if( Verbose.b && Pos.i ==1 )  cat('Look up table with window size of ', WinDays.i, ' days with ', 
                                          paste(c(V1.s, V2.s, V3.s, V4.s, V5.s)[!NoneCols.b], collapse=' '), '\n.', sep='')
        # Set window size
        Gap.i   <- ToBeFilled.V.i[Pos.i]
        if (T==T) {
          #! Non-congruent with MR PV-Wave
          Start.i <- Gap.i - (WinDays.i*sINFO$DTS)
          End.i   <- Gap.i + (WinDays.i*sINFO$DTS)
        } else {
          #! Code congruent with MR PV-Wave --> window size minus 1
          Start.i <- Gap.i - (WinDays.i*sINFO$DTS - 1)
          End.i   <- Gap.i + (WinDays.i*sINFO$DTS - 1)
        }
        
        if( Start.i <= 0 ) Start.i <- 1
        if( End.i > nrow(sTEMP) ) End.i <- nrow(sTEMP)
        
        #! Special treatment of Rg to be congruent with MR PV-Wave, in paper not mentioned 
        T1red.n <- if( grepl('Rg', V1.s) ) {
          # Reduce tolerance of radiation if variable name contains 'Rg' to [20,50] depending on measurement
          max(min(T1.n, sDATA[Gap.i,V1.s], na.rm=T), 20, na.rm=T)
        } else {
          T1.n
        }
        
        # For performance reasons, write sDATA subrange into vectors (speed up about factor of 1.5)
        V1.V.n <- sDATA[Start.i:End.i, V1.s]
        V2.V.n <- sDATA[Start.i:End.i, V2.s]
        V3.V.n <- sDATA[Start.i:End.i, V3.s]
        V4.V.n <- sDATA[Start.i:End.i, V4.s]
        V5.V.n <- sDATA[Start.i:End.i, V5.s]
        SubGap.i <- Gap.i - (Start.i-1)
        
        # Set LUT fill condition
        Rows.V.b <- !is.na(sTEMP$VAR_orig[Start.i:End.i])
        if( V1.s != 'none' )
          Rows.V.b <- Rows.V.b & abs(V1.V.n - V1.V.n[SubGap.i]) < T1red.n  & !is.na(V1.V.n)
        if( V2.s != 'none' )
          Rows.V.b <- Rows.V.b & abs(V2.V.n - V2.V.n[SubGap.i]) < T2.n  & !is.na(V2.V.n)
        if( V3.s != 'none' )
          Rows.V.b <- Rows.V.b & abs(V3.V.n - V3.V.n[SubGap.i]) < T3.n  & !is.na(V3.V.n)
        if( V4.s != 'none' )
          Rows.V.b <- Rows.V.b & abs(V4.V.n - V4.V.n[SubGap.i]) < T4.n  & !is.na(V4.V.n)
        if( V5.s != 'none' )
          Rows.V.b <- Rows.V.b & abs(V5.V.n - V5.V.n[SubGap.i]) < T5.n  & !is.na(V5.V.n)
        lLUT.V.n <- subset(sTEMP$VAR_orig[Start.i:End.i], Rows.V.b)
        
        # If enough available data, fill gap
        if( length(lLUT.V.n) > 1 ){
          lVAR_index.i <- Gap.i
          lVAR_mean.n <- mean(lLUT.V.n)
          lVAR_fnum.n <- length(lLUT.V.n)
          lVAR_fsd.n <- sd(lLUT.V.n)
          
          #Set window size and quality flag
          lVAR_fwin.n  <- 2*WinDays.i                      #! Full window length, congruent with MR PV-Wave, in paper single window sizes stated
          lVAR_fmeth.n <- NA_real_; lVAR_fqc.n <- NA_real_;
          if( V1.s != 'none' && V2.s != 'none' && V3.s != 'none') { #Three conditions
            lVAR_fmeth.n <- 1
            if( lVAR_fwin.n <= 14 ) lVAR_fqc.n <- 1        #! Limit '14' congruent with MR PV-Wave, in paper different limit of '28' (stated as single window size of 14 days)
            if( lVAR_fwin.n >  14 & lVAR_fwin.n <= 56 ) lVAR_fqc.n <- 2
            if( lVAR_fwin.n >  56 ) lVAR_fqc.n <- 3
          }
          if( V1.s != 'none' && V2.s == 'none' && V3.s == 'none') { #One condition only
            lVAR_fmeth.n <- 2
            if( lVAR_fwin.n <= 14 ) lVAR_fqc.n <- 1
            if( lVAR_fwin.n >  14 & lVAR_fwin.n <= 28 ) lVAR_fqc.n <- 2
            if( lVAR_fwin.n >  28 ) lVAR_fqc.n <- 3          
          } 
          lGF.M <- rbind(lGF.M, c(lVAR_index.i, lVAR_mean.n, lVAR_fnum.n, lVAR_fsd.n, lVAR_fmeth.n, lVAR_fwin.n, lVAR_fqc.n))
        }
        if( Verbose.b && Pos.i%%100 == 0 )  cat('.')
        if( Verbose.b && Pos.i%%6000 == 0 ) cat('\n.')
      }
      if( Verbose.b ) cat('', nrow(lGF.M), '\n')
    }
    # Copy gap filled values and properties to sTEMP
    if( nrow(lGF.M) > 0 ) {
      # Fill all rows in VAR_fall and co
      sTEMP[lGF.M[,'index'],c('VAR_fall','VAR_fnum','VAR_fsd','VAR_fmeth','VAR_fwin','VAR_fall_qc')] <<- 
        lGF.M[,c('mean','fnum','fsd','fmeth','fwin','fqc')]
      # Only fill gaps in VAR_f and VAR_fqc
      Gaps.b <- is.na(sTEMP[lGF.M[,'index'],'VAR_f'])
      sTEMP[lGF.M[,'index'],c('VAR_f','VAR_fqc')][Gaps.b,] <<- as.data.frame(lGF.M[,c('mean','fqc') ,drop=FALSE])[Gaps.b,] 
    }
    
    return(invisible(sTEMP[,c('VAR_orig','VAR_f','VAR_fall','VAR_fnum','VAR_fsd','VAR_fwin')])) #Other columns are specific for full MR MDS algorithm 
    ##value<< 
    ## LUT filling results in sTEMP data frame.
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sFillMDC = function(
    ##title<<
    ## sEddyProc$sFillMDC - Gap filling with Mean Diurnal Course (MDC)
    ##description<<
    ## Mean Diurnal Course (MDC) algorithm based on average values within +/- one hour of adjacent days
    WinDays.i           ##<< Window size for filling in days
    ,Verbose.b=TRUE     ##<< Print status information to screen
  )
    ##author<<
    ## AMM 
    # TEST: WinDays.i <- 2; Pos.i <- 100
  {
    'Mean Diurnal Course (MDC) algorithm based on average values within +/- one hour of adjacent days'
    
    #! Attention: For performance reasons, gap filled values and properties are first written to single 
    #! variables and local matrix lGF.M
    #! (rather than changing single values in sTEMP which copies the data frame each time!)
    #! Improved algorithm speed by more than a factor of 10 (maybe even 100...)
    lGF.M <- matrix(NA_real_, nrow=0, ncol=7, dimnames=list(NULL,c('index','mean','fnum','fsd','fmeth','fwin','fqc')))
    
    # Determine gap positions
    ToBeFilled.V.i <- which(is.na(sTEMP$VAR_fall))
    if( length(ToBeFilled.V.i) > 0 ) {
      for(Pos.i in 1:length(ToBeFilled.V.i)){
        # Message on progress if wanted
        if( Verbose.b && Pos.i == 1 ) cat('Mean diurnal course with window size of ', WinDays.i, ' days: \n.', sep='')
        
        # Set index within window size
        Gap.i   <- ToBeFilled.V.i[Pos.i]
        Index.V.i <- numeric(0)
        for (Day.i in (0:WinDays.i))
          if( Day.i == 0 ) { 
            Index.V.i <- c(Index.V.i, Gap.i+(-2:2))
          } else {
            Index.V.i <- c(Index.V.i, Gap.i+c(-Day.i*sINFO$DTS +(-2:2)), Gap.i+c(Day.i*sINFO$DTS +(-2:2)))
          }
        Index.V.i <- Index.V.i[Index.V.i>0 & Index.V.i<=nrow(sTEMP)]
        
        # If enough available data, fill gap
        lMDC.V.n <- subset(sTEMP$VAR_orig[Index.V.i], !is.na(sTEMP$VAR_orig[Index.V.i]))
        
        if( length(lMDC.V.n) > 1 ){
          lVAR_index.i <- Gap.i
          lVAR_mean.n <- mean(lMDC.V.n)
          lVAR_fnum.n  <- length(lMDC.V.n)
          lVAR_fsd.n  <- sd(lMDC.V.n)
          lVAR_fmeth.n  <- 3

          #Set window size and quality flag
          if (T==T) {
            #! Non-congruent with MR PV-Wave
            lVAR_fwin.n <- 2*WinDays.i + 1                 #! Full window length, not congruent with MR PV-Wave (see below), in paper single window sizes stated
          } else { 
            #! Code if required to be congruent with MR PV-Wave --> window calculation changes depending on size
            lVAR_fwin.n <- if( WinDays.i < 7 ) { 
               2*WinDays.i + 1
            } else { 
               WinDays.i + 1
            }
          }
          
          if( lVAR_fwin.n <= 1 ) lVAR_fqc.n <- 1
          if( lVAR_fwin.n >  1 & lVAR_fwin.n <= 5 ) lVAR_fqc.n <- 2  
          if( lVAR_fwin.n >  5 ) lVAR_fqc.n <- 3
          
          lGF.M <- rbind(lGF.M, c(lVAR_index.i, lVAR_mean.n, lVAR_fnum.n, lVAR_fsd.n, lVAR_fmeth.n, lVAR_fwin.n, lVAR_fqc.n))
        }
        if( Verbose.b && Pos.i%%100 == 0 )  cat('.')
        if( Verbose.b && Pos.i%%6000 == 0 ) cat('\n.')
      }
      if( Verbose.b ) cat('', nrow(lGF.M), '\n')
    }
    # Copy gap filled values and properties to sTEMP
    if( nrow(lGF.M) > 0 ) {
      # Fill all rows in VAR_fall and co
      sTEMP[lGF.M[,'index'],c('VAR_fall','VAR_fnum','VAR_fsd','VAR_fmeth','VAR_fwin','VAR_fall_qc')] <<- 
        lGF.M[,c('mean','fnum','fsd','fmeth','fwin','fqc')]
      # Only fill gaps in VAR_f and VAR_fqc
      Gaps.b <- is.na(sTEMP[lGF.M[,'index'],'VAR_f'])
      # twutz: inserted drop=FALSE, otherwise one-row matrix was not converted to data.frame correctly
      sTEMP[lGF.M[,'index'],c('VAR_f','VAR_fqc')][Gaps.b,] <<- as.data.frame(lGF.M[,c('mean','fqc') ,drop=FALSE])[Gaps.b,] 
    }
    
    return(invisible(sTEMP[,c('VAR_orig','VAR_f','VAR_fall','VAR_fnum','VAR_fsd','VAR_fwin')])) #Other columns are specific for full MR MDS algorithm
    ##value<< 
    ## MDC filling results in sTEMP data frame.
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sMDSGapFill = function(
    ##title<< 
    ## sEddyProc$sMDSGapFill - MDS gap filling algorithm
    ##description<<
    ## MDS gap filling algorithm adapted after the PV-Wave code and paper by Markus Reichstein.
    Var.s                 ##<< Variable to be filled
    ,QFVar.s='none'       ##<< Quality flag of variable to be filled
    ,QFValue.n=NA_real_   ##<< Value of quality flag for _good_ (original) data, other data is set to missing
    ,V1.s='Rg'            ##<< Condition variable 1 (default: Global radiation 'Rg' in  W m-2)
    ,T1.n=50              ##<< Tolerance interval 1 (default: 50 W m-2)
    ,V2.s='VPD'           ##<< Condition variable 2 (default: Vapour pressure deficit 'VPD' in hPa)
    ,T2.n=5               ##<< Tolerance interval 2 (default: 5 hPa)
    ,V3.s='Tair'          ##<< Condition variable 3 (default: Air temperature in degC)
    ,T3.n=2.5             ##<< Tolerance interval 3 (default: 2.5 degC)
    ,FillAll.b=TRUE       ##<< Fill all values to estimate uncertainties
    ,Verbose.b=TRUE       ##<< Print status information to screen
    ,suffix=""	          ##<< Scalar string scalar to be appended to Var.s before the qualifiers  
    ,QF.V.b = TRUE        ##<< boolean vector of length nRow(sData), to allow specifying bad data directly (those entries that are set to FALSE)
)
  ##author<<
  ## AMM
  ##references<<
  ## Reichstein M, Falge E, Baldocchi D et al. (2005) On the separation of net ecosystem exchange 
  ## into assimilation and ecosystem respiration: review and improved algorithm. Global Change Biology, 11, 1424-1439.
{
    'MDS gap filling algorithm adapted after the PV-Wave code and paper by Markus Reichstein.'

    TimeStart.p <- Sys.time()
    ##details<<
    ## Initialize temporal data frame sTEMP for newly generated gap filled data and qualifiers, see \code{\link{sFillInit}} for explanations on suffixes.
    if ( !is.null(sFillInit(Var.s, QFVar.s, QFValue.n, FillAll.b, QF.V.b = QF.V.b)) )
      return(invisible(-111)) # Abort gap filling if initialization of sTEMP failed
    
    #+++ Handling of special cases of meteo condition variables V1.s, V2.s, V3.s
    # If variables are at default values but do not exist as columns, set to 'none' (=disabled identifier).
    # This allows running MDS with less variables than prescribed in the default setting.
    # If meteo condition variable are same as variable to fill, also set to 'none'.
    # This prevents filling artificial gaps (for uncertainty estimates) with itself as meteo condition variable.
    #! Attention: Non-congruent with MR PV-Wave. There artificial gaps in Rg, VPD, Tair are filled with itself.
    if( (V1.s ==   'Rg' && !(V1.s %in% c(colnames(sDATA)))) || (V1.s == Var.s) )   V1.s <- 'none'
    if( (V2.s ==  'VPD' && !(V2.s %in% c(colnames(sDATA)))) || (V2.s == Var.s) )   V2.s <- 'none'
    if( (V3.s == 'Tair' && !(V3.s %in% c(colnames(sDATA)))) || (V3.s == Var.s) )   V3.s <- 'none'
    
    # Check column names (with 'none' as dummy)
    # (Numeric type and plausibility have been checked on initialization of sEddyProc)
    fCheckColNames(cbind(sDATA,sTEMP), c(V1.s, V2.s, V3.s), 'sMDSGapFill')
    
    # Check tolerance entries (if condition variable is not 'none')
    NoneCols.b <- c(V1.s, V2.s, V3.s) %in% 'none'
    if( !fCheckValNum(T1.n) || !fCheckValNum(T2.n) || !fCheckValNum(T3.n) ) {
      stop('sMDSGapFill::: T1.n, T2.n, T3.n, T4.n, T5.n must be numeric (if not specified, set to NA_real_)!')
    }
    if( sum(is.na(c(T1.n, T2.n, T3.n)[!NoneCols.b])) )
      stop('sMDSGapFill::: If condition variable is specified (dummy name is \'none\'), the tolerance interval must be specified!')
    
    # Run gap filling scheme depending on auxiliary meteo data availability
    ##details<<
    ## MDS gap filling algorithm calls the subroutines Look Up Table \code{\link{sFillLUT}} 
    ## and Mean Diurnal Course \code{\link{sFillMDC}} with different window sizes as described in the reference.
    ##details<<
    ## To run dataset only with MDC algorithm \code{\link{sFillMDC}}, set condition variable V1.s to 'none'.
    
    # Check availablility of meteorological data for LUT
    Met.n <- 
      if( V1.s != 'none' && V2.s != 'none' && V3.s != 'none' 
          && sum(!is.na(sDATA[,V1.s]))!=0 && sum(!is.na(sDATA[,V2.s]))!=0 && sum(!is.na(sDATA[,V3.s]))!=0 ) {  
        #All three meteo conditions are available and valid to use:
        message('Full MDS algorithm for gap filling of \'', attr(sTEMP$VAR_f,'varnames'), '\' with LUT(',V1.s,',',V2.s,',',V3.s,') and MDC.')
        3
        } else if( V1.s != 'none' && sum(!is.na(sDATA[,V1.s]))!=0 ) {
        #Only one meteo condition available for LUT
        message('Limited MDS algorithm for gap filling of \'', attr(sTEMP$VAR_f,'varnames'), '\' with LUT(',V1.s,' only) and MDC.')
        1
      } else {
        #No meteo condition available (use MDC only)
        message('Restriced MDS algorithm for gap filling of \'', attr(sTEMP$VAR_f,'varnames'), '\' with no meteo conditions available and hence only MDC.')
        warning('sMDSGapFill::: No meteo conditions available for MDS gap filling!')
        0
      } 
    
    #+++ Full MDS algorithm
    # Step 1 and 2; method 1; 7, 14 days
    if( Met.n == 3 ) sFillLUT(7, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n, Verbose.b=Verbose.b)
    if( Met.n == 3 ) sFillLUT(14, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n, Verbose.b=Verbose.b)
    # Step 3; method 2; 7 days
    if( Met.n == 3 || Met.n == 1) sFillLUT(7, V1.s, T1.n, Verbose.b=Verbose.b)
    # Step 4 and 5; method 3; 0, 1, 2 days  
    sFillMDC(0, Verbose.b=Verbose.b)
    sFillMDC(1, Verbose.b=Verbose.b)
    sFillMDC(2, Verbose.b=Verbose.b)
    # Step 6; method 1; >= 7 days  
    if( Met.n == 3 ) for( WinDays.i in seq(21,70,7) ) sFillLUT(WinDays.i, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n, Verbose.b=Verbose.b)
    # Step 7; method 2; >= 14 days  
    if( Met.n == 3 || Met.n == 1) for( WinDays.i in seq(14,70,7) ) sFillLUT(WinDays.i, V1.s, T1.n, Verbose.b=Verbose.b)
    # Step 8; method 3; >= 7 days  
    for( WinDays.i in seq(7,210,7) ) sFillMDC(WinDays.i, Verbose.b=Verbose.b)
    
    # Set long gaps again to NA
    sTEMP$VAR_fall <<- suppressMessages(fConvertGapsToNA(sTEMP$VAR_fall))
    
    # Message on gap filling
    TimeDiff.p <- as.numeric(Sys.time()) - as.numeric(TimeStart.p)
    message('Finished gap filling of \'', Var.s, '\' in ', floor(TimeDiff.p), ' seconds. Artificial gaps filled: ', length(sTEMP$VAR_fall) - sum(is.na(sTEMP$VAR_fall)),
            ', real gaps filled: ', sum(is.na(sTEMP$VAR_orig)), 
            ', unfilled (long) gaps: ', sum(is.na(sTEMP$VAR_fall)), '.')
    
    # Rename new columns generated during gap filling
    colnames(sTEMP) <<- gsub('VAR_', paste(Var.s, suffix, '_', sep=''), colnames(sTEMP))
    
    return(invisible(NULL))
    ##value<< 
    ## Gap filling results in sTEMP data frame (with renamed columns).
  })
  
  
  sEddyProc$methods(
  sMDSGapFillUStar = structure(function(
          ##title<< 
          ## sEddyProc$sMDSGapFillUStar - calling sMDSGapFill for several filters of friction velocity Ustar
          ##description<<
          ## MDS gap filling algorithm adapted after the PV-Wave code and paper by Markus Reichstein.
          Var.s                 ##<< Variable to be filled
          ,ustar.v = quantile( sDATA[,UstarVar.s], probs=0.25, na.rm=T)       ##<< Numeric vector of ustar thresholds to apply before gap filling
                ### Defaults to single value at the 25% quantile of UStar column
          ,suffix.v = ""        ##<< String vector of length of ustar.v of column suffixes to distinguish results for different ustar.v
          ,UstarVar.s='Ustar'   ##<< Friction velocity ustar (ms-1)
          ,...                  ##<< other arguments to sMDSGapFill
  )
  ##author<<
  ## TW
  {
      nUStar <- length(ustar.v)
      if( length(suffix.v) != nUStar ) error("sMDSGapFillUStar: number of suffixes must correspond to number of uStar-thresholds")
      # possibly parallelize, but difficult with R5 Classes
      lapply( seq(1:nUStar), function(i){
                  QF.V.b <- rep( TRUE, nrow(sDATA) )
                  QF.V.b[ sDATA[,UstarVar.s] < ustar.v[i] ] <- FALSE
                  .self$sMDSGapFill( Var.s, ..., QF.V.b=QF.V.b, suffix = suffix.v[i] )
              })
      return(invisible(NULL))
      ##value<< 
      ## Gap filling results in sTEMP data frame (with renamed columns).
  }, ex=function(){
      Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
      EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
      ds <- subset(EddyData.F, DoY >= 150 & DoY <= 250)
      EddyDataWithPosix.F <- fConvertTimeToPosix(ds, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
      EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))   
      probs <- c(0.1,0.25)
      ustar.v <- quantile( EddyProc.C$sDATA[,"Ustar"], probs=probs, na.rm=T)
      EddyProc.C$sMDSGapFillUStar('NEE', FillAll.b=TRUE, ustar.v = ustar.v[1] )        # just one ustar, no result column suffix 
      #EddyProc.C$sMDSGapFillUStar('NEE', FillAll.b=TRUE, ustar.v = ustar.v, suffix=paste0("_",probs*100)) 
      dsf <- EddyProc.C$sExportResults()
      colnames(dsf)
      #plot( NEE_25_f ~ NEE_10_f, dsf)
  }))
  