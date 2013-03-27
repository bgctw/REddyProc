#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc methods for gap filling +++
#+++ MDS gap filling algorithm, adapted after the PV-Wave code and paper by Markus Reichstein +++
#+++ Dependencies: Eddy.R, DataFunctions.R
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sFillLUT = function(
    ##title<<
    ## sEddyProc$sFillLUT - Gap filling with look up table
    ##description<<
    ## Look up table (LUT) algorithm based on up to five conditions within prescribed window size
    WinDays.i            ##<< window size for filling in days
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
    'Look up table (LUT) algorithm based on up to five conditions within prescribed window size'
    # Attention: For performance reasons, gap filled values and properties are first written to single variables and local matrix lGF.M
    # (rather than changing single values in sTEMP which copies the data frame each time!)
    # Improved algorithm speed by more than a factor of 10 (maybe even 100...)
    lGF.M <- matrix(NA, nrow=0, ncol=7, dimnames=list(NULL,c('index','mean','nums','sdev','meth','win','fqc')))
    
    # Determine gap positions
    ToBeFilled.V.i <- which(is.na(sTEMP$VAR_fall))
    if( length(ToBeFilled.V.i) >0 ) {
      for( Pos.i in 1:length(ToBeFilled.V.i) ) {
        # Message on progress if wanted
        NoneCols.b <- c(V1.s, V2.s, V3.s, V4.s, V5.s) %in% 'none' 
        if( Verbose.b && Pos.i ==1 )  cat('Look up table with window size of ', WinDays.i, ' days with ', 
                                          paste(c(V1.s, V2.s, V3.s, V4.s, V5.s)[!NoneCols.b], collapse=' '), '\n.', sep='')
        # Set window size
        Gap.i   <- ToBeFilled.V.i[Pos.i]
        Start.i <- Gap.i - (WinDays.i*sINFO$DTS -1) #TODO: !!!Delete later, -1 just to be congruent with MR PV-Wave
        End.i   <- Gap.i + (WinDays.i*sINFO$DTS -1) #TODO: !!!Delete later, -1 just to be congruent with MR PV-Wave
        if( Start.i <= 0 ) Start.i <- 1
        if( End.i > nrow(sTEMP) ) End.i <- nrow(sTEMP)
        
        # Special treatment of Rg to be congruent with MR PV-Wave     
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
          lVAR_fwin.n  <- 2*WinDays.i                        #Total size, congruent with MR PV-Wave
          
          if( V1.s != 'none' && V2.s != 'none' && V3.s != 'none' && V5.s == 'none' && V5.s == 'none' ) { #Three conditions as MR PV-Wave
            lVAR_fmeth.n <- 1
            if( lVAR_fwin.n <= 14 ) lVAR_fqc.n <- 1
            if( lVAR_fwin.n >  14 & lVAR_fwin.n <= 56 ) lVAR_fqc.n <- 2 #Congruent with MR PV-Wave, in paper "28"
            if( lVAR_fwin.n >  56 ) lVAR_fqc.n <- 3          #Congruent with MR PV-Wave, in paper "28"
          }
          if( V1.s != 'none' && V2.s == 'none' && V3.s == 'none' && V5.s == 'none' && V5.s == 'none' ){ #One condition only as MR PV-Wave
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
      if( Verbose.b ) cat('\n')
    }
    # Copy gap filled values and properties to sTEMP
    if( nrow(lGF.M) > 0 ) {
      sTEMP[lGF.M[,'index'],c('VAR_fall','VAR_fnum','VAR_fsd','VAR_fmeth','VAR_fwin','VAR_fqc')] <<- 
        lGF.M[,c('mean','nums','sdev','meth','win','fqc')]
    }
    ##value<< 
    ## LUT filling results in sTEMP data frame.
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sFillMDC = function(
    ##title<<
    ## sEddyProc$sFillMDC - Gap filling with mean diurnal course
    ##description<<
    ## Mean diurnal course (MDC) algorithm based on average values within ± one hour of adjacent days
    WinDays.i          ##<< Window size for filling in days
    ,Verbose.b=TRUE     ##<< Print status information to screen
  )
    ##author<<
    ## AMM 
    # TEST: WinDays.i <- 2; Pos.i <- 100
  {
    'Mean diurnal course (MDC) algorithm based on average values within ± one hour of adjacent days'
    # Attention: For performance reasons, gap filled values and properties are first written to single variables and local matrix lGF.M
    # (rather than changing single values in sTEMP which copies the data frame each time!)
    # Improved algorithm speed by more than a factor of 10 (maybe even 100...)
    lGF.M <- matrix(NA, nrow=0, ncol=7, dimnames=list(NULL,c('index','mean','nums','sdev','meth','win','fqc')))
    
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
          if( WinDays.i < 7 ) {
            lVAR_fwin.n  <- 2*WinDays.i + 1 #Total size, congruent with MR PV-Wave
          } else { lVAR_fwin.n  <- WinDays.i + 1 #TODO: !!!Delete later, just to be congruent with MR PV-Wave
          }
          
          if( lVAR_fwin.n <= 1 ) lVAR_fqc.n <- 1
          if( lVAR_fwin.n >  1 & lVAR_fwin.n <= 5 ) lVAR_fqc.n <- 2  
          if( lVAR_fwin.n >  5 ) lVAR_fqc.n <- 3
          
          lGF.M <- rbind(lGF.M, c(lVAR_index.i, lVAR_mean.n, lVAR_fnum.n, lVAR_fsd.n, lVAR_fmeth.n, lVAR_fwin.n, lVAR_fqc.n))
        }
        if( Verbose.b && Pos.i%%100 == 0 )  cat('.')
        if( Verbose.b && Pos.i%%6000 == 0 ) cat('\n.')
      }
      if( Verbose.b ) cat('\n')
    }
    # Copy gap filled values and properties to sTEMP
    if( nrow(lGF.M) > 0 ) {
      sTEMP[lGF.M[,'index'],c('VAR_fall','VAR_fnum','VAR_fsd','VAR_fmeth','VAR_fwin','VAR_fqc')] <<- 
        lGF.M[,c('mean','nums','sdev','meth','win','fqc')]
    }
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
    ,QFvar.s='none'       ##<< Quality flag of variable to be filled
    ,QFvalue.n=NA_real_   ##<< Value of quality flag for _good_ data, other data is set to missing
    ,V1.s='Rg'            ##<< Condition variable 1 (default: Global radiation 'Rg' in  W m-2)
    ,T1.n=50              ##<< Tolerance interval 1 (default: 50 W m-2)
    ,V2.s='VPD'           ##<< Condition variable 2 (default: Vapour pressure deficit 'VPD' in hPa)
    ,T2.n=5               ##<< Tolerance interval 2 (default: 5 hPa)
    ,V3.s='Tair'          ##<< Condition variable 3 (default: Air temperature in °C)
    ,T3.n=2.5             ##<< Tolerance interval 3 (default: 2.5 °C)
    ,V4.s='none'          ##<< Condition variable 4
    ,T4.n=NA_real_        ##<< Tolerance interval 4
    ,V5.s='none'          ##<< Condition variable 5
    ,T5.n=NA_real_        ##<< Tolerance interval 5
    ,Verbose.b=TRUE       ##<< Print status information to screen
  )
  ##author<<
  ## AMM
  ##references<<
  ## Reichstein M, Falge E, Baldocchi D et al. (2005) On the separation of net ecosystem exchange 
  ## into assimilation and ecosystem respiration: review and improved algorithm. Global Change Biology, 11, 1424-1439.
  # TEST: sDATA <- EPTha.C$sDATA; sINFO <- EPTha.C$sINFO; Var.s <- 'NEE'; QFvar.s <- 'none'; QFvalue.n <- NA_real_;
  # TEST: V1.s <- 'Rg'; T1.n <- 50; V2.s <- 'VPD'; T2.n <- 5; 
  # TEST: V3.s <- 'Tair'; T3.n <- 2.5; V4.s <- 'none'; T4.n <- NA_real_; V5.s <- 'none'; T5.n <- NA_real_; Verbose.b <- TRUE
{
    'MDS gap filling algorithm adapted after the PV-Wave code and paper by Markus Reichstein.'
    # Check variable to fill and apply quality flag
    Var.V.n <- fSetQF(sDATA, Var.s, QFvar.s, QFvalue.n, 'fMDSGapFill')
    
    # Check if variable to be filled contains any data
    if( sum(!is.na(Var.V.n)) == 0 )
      stop('Variable to be filled (', Var.s, ') contains no data at all!')
    
    # Check if specified condition columns exist and are numeric (with 'none' as dummy)
    fCheckColNames(sDATA, c(V1.s, V2.s, V3.s, V4.s, V5.s), 'fMDSGapFill')
    fCheckColNum(sDATA, c(V1.s, V2.s, V3.s, V4.s, V5.s), 'fMDSGapFill')
    fCheckColPlausibility(sDATA, c(Var.s, c(V1.s, V2.s, V3.s, V4.s, V5.s)), 'fMDSGapFill')
    
    # Check tolerance entries (if condition variable is not 'none')
    NoneCols.b <- c(V1.s, V2.s, V3.s, V4.s, V5.s) %in% 'none'
    if( !fCheckValNum(T1.n) || !fCheckValNum(T2.n) || !fCheckValNum(T3.n) || !fCheckValNum(T4.n) || !fCheckValNum(T5.n) ) {
      stop("T1.n, T2.n, T3.n, T4.n, T5.n must be numeric (if not specified, set to NA_real_)!")
    }
    if( sum(is.na(c(T1.n, T2.n, T3.n, T4.n, T5.n)[!NoneCols.b])) )
      stop('If condition variable is specified (dummy name is \'none\'), the tolerance interval must be specified!')
    
    message('Gap filling of variable: \'', Var.s, '\' with ', sum(is.na(Var.V.n)), ' real gaps.')
    TimeStart.p <- Sys.time()
    
    # Generate gap filling columns
    ##details<<
    ## sTEMP data frame for temporally generated gap filled data and qualifiers.
    sTEMP <<- data.frame(
      VAR_orig=Var.V.n               # Variable with original values of VAR
      ,VAR_f=NA                      # Variable with gaps filled
      ,VAR_fall=NA                   # Variable with all datapoints filled (for uncertainty estimates)
      ,VAR_fnum=NA                   # Number of datapoints used for filling
      ,VAR_fsd=NA                    # Standard deviation of data points used for filling
      ,VAR_fmeth=NA                  # Method used for filling
      ,VAR_fwin= NA                  # Window size used for filling
      ,VAR_fqc= NA                   # Quality flag used for filling
    )
    
    ##details<<
    ## Long gaps (larger than 60 days) are not filled.
    # Not congruent with PV-Wave. There the code is performed on single years where it drops long gaps of 60 days in the beginning or end.
    GapLength.V.n <- fCalcLengthOfGaps(sTEMP$VAR_orig)
    kMaxGap.n <- 48 * 60 #Halfhours in 60 days
    while ( max(GapLength.V.n) > kMaxGap.n ) {
      #Flag long gap with -9999.0
      End.i <- which(GapLength.V.n == max(GapLength.V.n))
      Start.i <- End.i - max(GapLength.V.n) + 1
      sTEMP$VAR_fall[Start.i:End.i] <<- -9999.0 #Set to -9999.0 as a flag for long gaps
      GapLength.V.n[Start.i:End.i] <- -1 #Set to -1 since accounted for
      warning('The long gap between position ', Start.i, ' and ', End.i, ' will not be filled.')
    }
    
    # Run gap filling scheme depending on auxiliary data availability
    # !!!Attention: Artificial gaps are filled with itself if Var.s %in%  == c(V1.s, V2.s, V3.s, V4.s, V5.s), this is congruent to PV-Wave code.
    ##details<<
    ## To run only MDC, set condition variable V1.s to 'none'
    if( 
      V1.s == 'none'               # There is no first condition defined
      || sum(!is.na(sDATA[,V1.s]))==0 # First condition variable is empty
    ) {
      #+++ Fill with MDC only
      # Step 4 and 5; 0, 1, 2 days
      sFillMDC(0, Verbose.b=Verbose.b)
      sFillMDC(1, Verbose.b=Verbose.b)
      sFillMDC(2, Verbose.b=Verbose.b)
      # Step 8; >= 7 days
      for (WinDays.i in seq(7,210,7)) sFillMDC(WinDays.i, Verbose.b=Verbose.b)
    } else { 
      
      #+++ Full algorithm
      # Step 1 and 2; method 1; 7, 14 days
      sFillLUT(7, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n, Verbose.b=Verbose.b)
      sFillLUT(14, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n, Verbose.b=Verbose.b)
      # Step 3; method 2; 7 days
      sFillLUT(7, V1.s, T1.n, Verbose.b=Verbose.b)
      # Step 4 and 5; 0, 1, 2 days  
      sFillMDC(0, Verbose.b=Verbose.b)
      sFillMDC(1, Verbose.b=Verbose.b)
      sFillMDC(2, Verbose.b=Verbose.b)
      # Step 6; method 1; >= 7 days  
      for (WinDays.i in seq(21,70,7)) sFillLUT(WinDays.i, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n, Verbose.b=Verbose.b)
      # Step 7; method 2; >= 14 days  
      for (WinDays.i in seq(14,70,7)) sFillLUT(WinDays.i, V1.s, T1.n, Verbose.b=Verbose.b)
      # Step 8; >= 7 days  
      for (WinDays.i in seq(7,210,7)) sFillMDC(WinDays.i, Verbose.b=Verbose.b)    
    }
    # Set long gaps again to NA
    sTEMP$VAR_fall <<- suppressMessages(fConvertGapsToNA(sTEMP$VAR_fall))
    # Generate filled variable column
    sTEMP$VAR_f <<- ifelse(is.na(sTEMP$VAR_orig), sTEMP$VAR_fall, sTEMP$VAR_orig)
    # Message on gap filling
    TimeDiff.p <- as.numeric(Sys.time()) - as.numeric(TimeStart.p)
    message('Finished gap filling in ', floor(TimeDiff.p), ' seconds. Artificial gaps filled: ', length(sTEMP$VAR_fall) - sum(is.na(sTEMP$VAR_fall)),
            ', real gaps filled: ', sum(is.na(sTEMP$VAR_orig)), 
            ', unfilled (long) gaps: ', sum(is.na(sTEMP$VAR_fall)), '.')
    
    # Rename new columns generated during gap filling
    colnames(sTEMP) <<- gsub("VAR_", paste(Var.s, '_', sep=''), colnames(sTEMP))
    # Attach to sDATA data frame
    sDATA <<- cbind(sDATA,sTEMP)
    
    ##value<< 
    ## Gap filling columns added to sDATA frame.
  })
