#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc methods for gap filling
#+++ MDS gap filling algorithm, adapted after the PV-Wave code and paper by
#Markus Reichstein
#+++ Dependencies: Eddy.R, DataFunctions.R, EddyUStarFilerDP.R
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @include aEddy.R
#' @export
sEddyProc_sFillInit <- function(
  ### Initializes data frame sTEMP for newly generated gap filled data and qualifiers.
  ##title<< sEddyProc$sFillInit - Initialize gap filling
  Var.s                   ##<< Variable to be filled
  , QFVar.s = 'none'      ##<< Quality flag of variable to be filled
  , QFValue.n = NA_real_  ##<< Value of quality flag for _good_ (original) data,
  ## other data is set to missing
  , FillAll.b = TRUE      ##<< Fill all values to estimate uncertainties
  #! , QF.V.b = TRUE      ##<< boolean vector of length nRow(sData), to allow
  # # specifying bad data directly (those entries that are set to FALSE)
) {
  ##author<< AMM
  'Initializes data frame sTEMP for newly generated gap filled data and qualifiers.'

  # Check variable to fill and apply quality flag
  fCheckColNames(cbind(sDATA, sTEMP), c(Var.s, QFVar.s), 'sFillInit')
  Var.V.n <- fSetQF(cbind(sDATA, sTEMP), Var.s, QFVar.s, QFValue.n, 'sFillInit')
  #! Var.V.n[QF.V.b == FALSE]   <-   NA_real_

  # Abort if variable to be filled contains no data
  if (sum(!is.na(Var.V.n)) == 0) {
    warning('sFillInit::: Variable to be filled (', Var.s, ') contains no data at all!')
    return(-111)
  }
  ##details<<
  ## Description of newly generated variables with gap filled data and qualifiers: \cr
  ##details<<
  ## VAR\emph{_orig} - Original values used for gap filling \cr
  ## VAR\emph{_f   } - Original values and gaps filled with mean of selected
  ## datapoints (condition depending on gap filling method) \cr
  ## VAR\emph{_fqc} - Quality flag assigned depending on gap filling method and
  ## window length (0 = original data, 1 = most reliable, 2 = medium, 3 = least reliable) \cr
  ## VAR\emph{_fall} - All values considered as gaps (for uncertainty estimates) \cr
  ## VAR\emph{_fall_qc} - Quality flag assigned depending on gap filling method
  ## and window length (1 = most reliable, 2 = medium, 3 = least reliable) \cr
  ## VAR\emph{_fnum} - Number of datapoints used for gap-filling \cr
  ## VAR\emph{_fsd} - Standard deviation of datapoints used for gap
  ## filling (uncertainty) \cr
  ## VAR\emph{_fmeth} - Method used for gap filling (1 = similar meteo
  ## condition (sFillLUT with Rg, VPD, Tair), 2 = similar meteo
  ## (sFillLUT with Rg only), 3 = mean diurnal course (sFillMDC)) \cr
  ## VAR\emph{_fwin} - Full window length used for gap filling \cr

  lTEMP <- data.frame(
    VAR_orig = Var.V.n      # Original values of variable VAR used for gap filling
    , VAR_f = NA_real_       # Original values and filled gaps
    , VAR_fqc = NA_real_     # Quality flag assigned depending on gap filling
    ## method and window length
    , VAR_fall = NA_real_    # All values considered as gaps
    ## (for uncertainty estimates)
    , VAR_fall_qc = NA_real_ # Quality flag assigned depending on gap filling
    ## method and window length
    , VAR_fnum = NA_real_    # Number of datapoints used for gap-filling
    , VAR_fsd = NA_real_     # Standard deviation of data points used for filling
    , VAR_fmeth = NA_real_   # Method used for gap filling
    , VAR_fwin = NA_real_    # Full window length used for gap filling
  )

  # Set fqc to zero for original values
  lTEMP$VAR_f <- lTEMP$VAR_orig
  lTEMP$VAR_fqc <- ifelse(!is.na(lTEMP$VAR_orig), 0, NA_real_)

  # Set filling of only gaps
  if (FillAll.b == FALSE) lTEMP$VAR_fall <- lTEMP$VAR_orig #"prefill" with original data

  # Add units
  attr(lTEMP$VAR_f, 'units') <- attr(Var.V.n, 'units')
  attr(lTEMP$VAR_fall, 'units') <- attr(Var.V.n, 'units')
  attr(lTEMP$VAR_fsd, 'units') <- attr(Var.V.n, 'units')
  attr(lTEMP$VAR_fwin, 'units') <- 'days'

  ##details<<
  ## Long gaps (larger than 60 days) are not filled.
  #! Not congruent with PV-Wave, there the code is performed on single years
  #only with long gaps of 60 days in the beginning or end skipped.
  GapLength.V.n <- fCalcLengthOfGaps(lTEMP$VAR_orig)
  kMaxGap.n <- sINFO$DTS * 60 #Halfhours in 60 days
  while (max(GapLength.V.n) > kMaxGap.n) {
    #Flag long gap with -9999.0
    End.i <- which(GapLength.V.n == max(GapLength.V.n))
    Start.i <- End.i - max(GapLength.V.n) + 1
    lTEMP$VAR_fall[Start.i:End.i] <- -9999.0 #Set to -9999.0 as a flag for long gaps
    GapLength.V.n[Start.i:End.i] <- -1 #Set to -1 since accounted for
    warning(
      'sMDSGapFill::: The long gap between position ', Start.i, ' and '
      , End.i, ' will not be filled!')
  }

  if (FillAll.b == T) {
    message(
      'Initialized variable \'', Var.s, '\' with ', sum(is.na(lTEMP$VAR_orig))
      , ' real gaps for gap filling of all ', sum(is.na(lTEMP$VAR_fall))
      , ' values (to estimate uncertainties).')
  } else {
    message('Initialized variable \'', Var.s, '\' with ', sum(is.na(lTEMP$VAR_orig)),
            ' real gaps for gap filling.')
  }

  # twutz: error prone if sTEMP already contains columns of lTEMP
  sTEMP <<- data.frame(c(sTEMP, lTEMP))
  return(invisible(NULL))
}
sEddyProc$methods(sFillInit = sEddyProc_sFillInit)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sFillLUT <- function(
  ### Look-Up Table (LUT) algorithm of up to five conditions within prescribed window size
  WinDays.i             ##<< Window size for filling in days
  , V1.s = 'none'          ##<< Condition variable 1
  , T1.n = NA_real_        ##<< Tolerance interval 1
  , V2.s = 'none'          ##<< Condition variable 2
  , T2.n = NA_real_        ##<< Tolerance interval 2
  , V3.s = 'none'          ##<< Condition variable 3
  , T3.n = NA_real_        ##<< Tolerance interval 3
  , V4.s = 'none'          ##<< Condition variable 4
  , T4.n = NA_real_        ##<< Tolerance interval 4
  , V5.s = 'none'          ##<< Condition variable 5
  , T5.n = NA_real_        ##<< Tolerance interval 5
  , Verbose.b = TRUE       ##<< Print status information to screen
) {
  ##author<< AMM
  #! Attention: For performance reasons, gap filled values and properties are
  #first written to single variables and local matrix lGF.M
  #! (rather than changing single values in sTEMP which copies the data frame each time!)
  #! Improved algorithm speed by more than a factor of 10 (maybe even 100...)
  lGF.M <- matrix(NA_real_, nrow = 0, ncol = 7, dimnames = list(
    NULL, c('index', 'mean', 'fnum', 'fsd', 'fmeth', 'fwin', 'fqc')))

  # Check if sTEMP has been initialized with new VAR_ columns
  if (!exists('VAR_f', sTEMP) ) stop(
    'sFillLUT::: Temporal data frame sTEMP for processing results has not '
    , 'been initalized with sFillInit!')

  # Determine gap positions
  ToBeFilled.V.i <- which(is.na(sTEMP$VAR_fall))
  if (length(ToBeFilled.V.i) > 0) {
    for (Pos.i in 1:length(ToBeFilled.V.i) ) {
      # Message on progress if wanted
      NoneCols.b <- c(V1.s, V2.s, V3.s, V4.s, V5.s) %in% 'none'
      if (Verbose.b && Pos.i == 1)  message(
        'Look up table with window size of ', WinDays.i, ' days with '
        , paste(c(V1.s, V2.s, V3.s, V4.s, V5.s)[!NoneCols.b], collapse = ' '))
      # Set window size
      Gap.i   <- ToBeFilled.V.i[Pos.i]
      if (T == T) {
        #! Non-congruent with MR PV-Wave
        Start.i <- Gap.i - (WinDays.i * sINFO$DTS)
        End.i   <- Gap.i + (WinDays.i * sINFO$DTS)
      } else {
        #! Code congruent with MR PV-Wave --> window size minus 1
        Start.i <- Gap.i - (WinDays.i * sINFO$DTS - 1)
        End.i   <- Gap.i + (WinDays.i * sINFO$DTS - 1)
      }

      if (Start.i <= 0) Start.i <- 1
      if (End.i > nrow(sTEMP) ) End.i <- nrow(sTEMP)

      #! Special treatment of Rg to be congruent with MR PV-Wave, in paper not mentioned
      T1red.n <- if (grepl('Rg', V1.s) ) {
        # Reduce tolerance of radiation if variable name contains 'Rg' to
        # [20, 50] depending on measurement
        max(min(T1.n, sDATA[Gap.i, V1.s], na.rm = T), 20, na.rm = T)
      } else {
        T1.n
      }

      # For performance reasons, write sDATA subrange into vectors
      # (speed up about factor of 1.5)
      V1.V.n <- sDATA[Start.i:End.i, V1.s]
      V2.V.n <- sDATA[Start.i:End.i, V2.s]
      V3.V.n <- sDATA[Start.i:End.i, V3.s]
      V4.V.n <- sDATA[Start.i:End.i, V4.s]
      V5.V.n <- sDATA[Start.i:End.i, V5.s]
      SubGap.i <- Gap.i - (Start.i - 1)

      # Set LUT fill condition
      Rows.V.b <- !is.na(sTEMP$VAR_orig[Start.i:End.i])
      if (V1.s != 'none')
        Rows.V.b <- Rows.V.b & abs(V1.V.n - V1.V.n[SubGap.i]) < T1red.n  & !is.na(V1.V.n)
      if (V2.s != 'none')
        Rows.V.b <- Rows.V.b & abs(V2.V.n - V2.V.n[SubGap.i]) < T2.n  & !is.na(V2.V.n)
      if (V3.s != 'none')
        Rows.V.b <- Rows.V.b & abs(V3.V.n - V3.V.n[SubGap.i]) < T3.n  & !is.na(V3.V.n)
      if (V4.s != 'none')
        Rows.V.b <- Rows.V.b & abs(V4.V.n - V4.V.n[SubGap.i]) < T4.n  & !is.na(V4.V.n)
      if (V5.s != 'none')
        Rows.V.b <- Rows.V.b & abs(V5.V.n - V5.V.n[SubGap.i]) < T5.n  & !is.na(V5.V.n)
      lLUT.V.n <- subset(sTEMP$VAR_orig[Start.i:End.i], Rows.V.b)

      # If enough available data, fill gap
      if (length(lLUT.V.n) > 1) {
        lVAR_index.i <- Gap.i
        lVAR_mean.n <- mean(lLUT.V.n)
        lVAR_fnum.n <- length(lLUT.V.n)
        lVAR_fsd.n <- sd(lLUT.V.n)

        #Set window size and quality flag
        ##details<< \describe{\item{Quality flags}{
        ## \itemize{
        ## \item 1: at least one variable and nDay <= 14
        ## \item 2: three variables and nDay in [14,56)
        ## or one variable and nDay in  [14,28)
        ## \item 3: three variables and nDay > 56
        ## or one variable and nDay > 28
        ## }
        ## }}
        #! Full window length, congruent with MR PV-Wave, in paper
        #single window sizes stated
        lVAR_fwin.n  <- 2 * WinDays.i
        lVAR_fmeth.n <- NA_real_; lVAR_fqc.n <- NA_real_;
        if (V1.s != 'none' && V2.s != 'none' && V3.s != 'none') {
          #Three conditions
          lVAR_fmeth.n <- 1
          #! Limit '14' congruent with MR PV-Wave, in paper different limit
          #of '28' (stated as single window size of 14 days)
          if (lVAR_fwin.n <= 14) lVAR_fqc.n <- 1
          if (lVAR_fwin.n >  14 & lVAR_fwin.n <= 56) lVAR_fqc.n <- 2
          if (lVAR_fwin.n >  56) lVAR_fqc.n <- 3
        }
        if (V1.s != 'none' && V2.s == 'none' && V3.s == 'none') {
          #One condition only
          lVAR_fmeth.n <- 2
          if (lVAR_fwin.n <= 14) lVAR_fqc.n <- 1
          if (lVAR_fwin.n >  14 & lVAR_fwin.n <= 28) lVAR_fqc.n <- 2
          if (lVAR_fwin.n >  28) lVAR_fqc.n <- 3
        }
        lGF.M <- rbind(lGF.M, c(
          lVAR_index.i, lVAR_mean.n, lVAR_fnum.n, lVAR_fsd.n
          , lVAR_fmeth.n, lVAR_fwin.n, lVAR_fqc.n))
      }
      if (Verbose.b && Pos.i %% 100 == 0)  message('.', appendLF = FALSE)
      if (Verbose.b && Pos.i %% 6000 == 0) message('\n.', appendLF = FALSE)
    }
    if (Verbose.b) message('', nrow(lGF.M))
  }
  # Copy gap filled values and properties to sTEMP
  if (nrow(lGF.M) > 0) {
    # Fill all rows in VAR_fall and co
    sTEMP[lGF.M[, 'index'], c(
      'VAR_fall', 'VAR_fnum', 'VAR_fsd', 'VAR_fmeth', 'VAR_fwin', 'VAR_fall_qc')] <<-
      lGF.M[, c('mean', 'fnum', 'fsd', 'fmeth', 'fwin', 'fqc')]
    # Only fill gaps in VAR_f and VAR_fqc
    Gaps.b <- is.na(sTEMP[lGF.M[, 'index'], 'VAR_f'])
    sTEMP[lGF.M[, 'index'], c('VAR_f', 'VAR_fqc')][Gaps.b, ] <<-
      as.data.frame(lGF.M[, c('mean', 'fqc') , drop = FALSE])[Gaps.b, ]
  }

  #Other columns are specific for full MR MDS algorithm
  return(invisible(sTEMP[, c(
    'VAR_orig', 'VAR_f', 'VAR_fall', 'VAR_fnum', 'VAR_fsd', 'VAR_fwin')]))
  ##value<<
  ## LUT filling results in sTEMP data frame.
}
sEddyProc$methods(sFillLUT = sEddyProc_sFillLUT)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sFillMDC <- function(
  ### Mean Diurnal Course (MDC) algorithm based on average values within +/- one hour of adjacent days
  WinDays.i            ##<< Window size for filling in days
  , Verbose.b = TRUE   ##<< Print status information to screen
) {
  ##author<< AMM
  #! Attention: For performance reasons, gap filled values and properties are first written to single
  #! variables and local matrix lGF.M
  #! (rather than changing single values in sTEMP which copies the data frame each time!)
  #! Improved algorithm speed by more than a factor of 10 (maybe even 100...)
  lGF.M <- matrix(NA_real_, nrow = 0, ncol = 7, dimnames = list(NULL, c(
    'index', 'mean', 'fnum', 'fsd', 'fmeth', 'fwin', 'fqc')))

  # Determine gap positions
  ToBeFilled.V.i <- which(is.na(sTEMP$VAR_fall))
  if (length(ToBeFilled.V.i) > 0) {
    for (Pos.i in 1:length(ToBeFilled.V.i)) {
      # Message on progress if wanted
      if (Verbose.b && Pos.i == 1) message(
        'Mean diurnal course with window size of ', WinDays.i, ' days: .', sep = '')

      # Set index within window size
      Gap.i   <- ToBeFilled.V.i[Pos.i]
      Index.V.i <- numeric(0)
      for (Day.i in (0:WinDays.i))
        if (Day.i == 0) {
          Index.V.i <- c(Index.V.i, Gap.i + (-2:2))
        } else {
          Index.V.i <- c(
            Index.V.i, Gap.i + c(-Day.i * sINFO$DTS + (-2:2))
            , Gap.i + c(Day.i * sINFO$DTS + (-2:2)))
        }
      Index.V.i <- Index.V.i[Index.V.i > 0 & Index.V.i <= nrow(sTEMP)]

      # If enough available data, fill gap
      #iRecsSimilar <- Index.V.i[!is.na(sTEMP$VAR_orig[Index.V.i]) ]
      lMDC.V.n <- subset(sTEMP$VAR_orig[Index.V.i], !is.na(sTEMP$VAR_orig[Index.V.i]))

      if (length(lMDC.V.n) > 1) {
        #if (Gap.i == 15167  ) recover()

        lVAR_index.i <- Gap.i
        lVAR_mean.n <- mean(lMDC.V.n)
        lVAR_fnum.n  <- length(lMDC.V.n)
        lVAR_fsd.n  <- sd(lMDC.V.n)
        lVAR_fmeth.n  <- 3

        #Set window size and quality flag
        if (T == T) {
          #! Non-congruent with MR PV-Wave
          ##! Full window length, not congruent with MR PV-Wave (see below),
          ##in paper single window sizes stated
          lVAR_fwin.n <- 2 * WinDays.i + 1
        } else {
          #! Code if required to be congruent with MR PV-Wave -->
          #window calculation changes depending on size
          lVAR_fwin.n <- if (WinDays.i < 7) {
            2 * WinDays.i + 1
          } else {
            WinDays.i + 1
          }
        }

        ##details<< \describe{\item{Quality flag}{
        ## \itemize{
        ## \item 1: nDay <= 1
        ## \item 2: nDay [2,5)
        ## \item 3: nDay > 5
        ## }
        ## }}
        if (lVAR_fwin.n <= 1) lVAR_fqc.n <- 1
        if (lVAR_fwin.n >  1 & lVAR_fwin.n <= 5) lVAR_fqc.n <- 2
        if (lVAR_fwin.n >  5) lVAR_fqc.n <- 3

        lGF.M <- rbind(lGF.M, c(
          lVAR_index.i, lVAR_mean.n, lVAR_fnum.n, lVAR_fsd.n
          , lVAR_fmeth.n, lVAR_fwin.n, lVAR_fqc.n))
      }
      if (Verbose.b && Pos.i %% 100 == 0)  message('.', appendLF = FALSE)
      if (Verbose.b && Pos.i %% 6000 == 0) message('\n.', appendLF = FALSE)
    }
    if (Verbose.b) message('', nrow(lGF.M))
  }
  # Copy gap filled values and properties to sTEMP
  if (nrow(lGF.M) > 0) {
    # Fill all rows in VAR_fall and co
    sTEMP[lGF.M[, 'index'], c(
      'VAR_fall', 'VAR_fnum', 'VAR_fsd', 'VAR_fmeth', 'VAR_fwin', 'VAR_fall_qc')] <<-
      lGF.M[, c('mean', 'fnum', 'fsd', 'fmeth', 'fwin', 'fqc')]
    # Only fill gaps in VAR_f and VAR_fqc
    Gaps.b <- is.na(sTEMP[lGF.M[, 'index'], 'VAR_f'])
    # twutz: inserted drop = FALSE, otherwise one-row matrix was not
    # converted to data.frame correctly
    sTEMP[lGF.M[, 'index'], c('VAR_f', 'VAR_fqc')][Gaps.b, ] <<-
      as.data.frame(lGF.M[, c('mean', 'fqc') , drop = FALSE])[Gaps.b, ]
  }

  #Other columns are specific for full MR MDS algorithm
  return(invisible(sTEMP[, c(
    'VAR_orig', 'VAR_f', 'VAR_fall', 'VAR_fnum', 'VAR_fsd', 'VAR_fwin')]))
  ##value<<
  ## MDC filling results in sTEMP data frame.
}
sEddyProc$methods(sFillMDC = sEddyProc_sFillMDC)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sMDSGapFill <- function(
  ### MDS gap filling algorithm adapted after the PV-Wave code and paper by Markus Reichstein.
  Var = Var.s                 ##<< Variable to be filled
  , QFVar = if (!missing(QFVar.s)) QFVar.s else 'none'       ##<<
  ## Quality flag of variable to be filled
  , QFValue = if (!missing(QFValue.n)) QFValue.n else NA_real_   ##<<
  ## Value of quality flag for _good_ (original) data, other data is set to missing
  , V1 = if (!missing(V1.s)) V1.s else 'Rg'            ##<< Condition variable 1
  ## (default: Global radiation 'Rg' in  W m-2)
  , T1 = if (!missing(T1.n)) T1.n else 50              ##<< Tolerance interval 1
  ## (default: 50 W m-2)
  , V2 = if (!missing(V2.s)) V2.s else 'VPD'           ##<< Condition variable 2
  ## (default: Vapour pressure deficit 'VPD' in hPa)
  , T2 = if (!missing(T2.n)) T2.n else 5               ##<< Tolerance interval 2
  ## (default: 5 hPa)
  , V3 = if (!missing(V3.s)) V3.s else 'Tair'          ##<< Condition variable 3
  ## (default: Air temperature 'Tair' in degC)
  , T3 = if (!missing(T3.n)) T3.n else 2.5             ##<< Tolerance interval 3
  ## (default: 2.5 degC)
  , FillAll = if (!missing(FillAll.b)) FillAll.b else TRUE       ##<< Fill
  ##  all values to estimate uncertainties
  , isVerbose = if (!missing(Verbose.b)) Verbose.b else TRUE       ##<< Print
  ##  status information to screen
  , suffix = if (!missing(Suffix.s)) Suffix.s else ''	      ##<< String
  ##  suffix needed for different processing setups on the same dataset
  ## (for explanations see below)
  , minNWarnRunLength = ##<< scalar integer:
    ## warn if number of subsequent
    ## numerically equal values exceeds this number.
    ## Set to Inf or NA for no warnings.
    ## defaults for "NEE" to records across 4 hours and no warning for others.
    if (Var == "NEE") 4 * .self$sINFO$DTS/24 else NA_integer_
  , Var.s      ##<< deprecated
  , QFVar.s    ##<< deprecated
  , QFValue.n  ##<< deprecated
  , V1.s ##<< deprecated
  , T1.n ##<< deprecated
  , V2.s ##<< deprecated
  , T2.n ##<< deprecated
  , V3.s ##<< deprecated
  , T3.n ##<< deprecated
  , FillAll.b ##<< deprecated
  , Verbose.b ##<< deprecated
  , Suffix.s ##<< deprecated
  #! , QF.V.b = TRUE        ##<< boolean vector of length nRow(sData),
  ## to allow specifying bad data directly (those entries that are set to FALSE)
) {
  varNamesDepr <- c(
    "Var.s","QFVar.s","QFValue.n","V1.s","T1.n"
    ,"V2.s","T2.n","V3.s","T3.n","FillAll.b","Verbose.b","Suffix.s")
  varNamesNew <- c(
    "Var","QFVar","QFValue","V1","T1"
    ,"V2","T2","V3","T3","FillAll","isVerbose","suffix")
  iDepr = which(!c(
    missing(Var.s),missing(QFVar.s),missing(QFValue.n),missing(V1.s)
    ,missing(T1.n),missing(V2.s),missing(T2.n),missing(V3.s),missing(T3.n)
    ,missing(FillAll.b),missing(Verbose.b),missing(Suffix.s)))
  if (length(iDepr)) warning(
    "Arguments names ",paste(varNamesDepr[iDepr], collapse = ",")
    ," have been deprecated."
    ," Please, use instead ", paste(varNamesNew[iDepr], collapse = ","))
  ##author<< AMM, TW
  ##references<<
  ## Reichstein, M. et al. (2005) On the separation of net ecosystem exchange
  ## into assimilation and ecosystem respiration: review and improved algorithm.
  ## Global Change Biology, 11, 1424-1439.
  TimeStart.p <- Sys.time()
  ##details<<
  ## Initialize temporal data frame sTEMP for newly generated gap filled data and
  ## qualifiers, see \code{\link{sEddyProc_sFillInit}} for explanations on suffixes.
  # sTEMP <<- sTEMP[, 1L, drop = FALSE]
  if (!is.null(sFillInit(Var, QFVar, QFValue, FillAll)) ) #! , QF.V.b = QF.V.b)) )
    return(invisible(-111)) # Abort gap filling if initialization of sTEMP failed
  ##details<<
  ## Runs of numerically equal numbers hint to problems of the data and cause
  ## unreasonable estimates of uncertainty. This routine warns the user.
  if (is.finite(minNWarnRunLength) & (nrow(sTEMP) >= minNWarnRunLength)) {
    rl <- .runLength(as.vector(sTEMP$VAR_orig), minNRunLength = minNWarnRunLength)
    if (length(rl$index)) {
      rlSorted <- rl[rev(order(rl$nRep)),,drop = FALSE]
      warning(
        "Variable ", Var, " contains long runs of numerically equal numbers."
        , " Longest of ", rlSorted$nRep[1], " repeats of value "
        , sTEMP$VAR_orig[ rlSorted$index[1] ]
        , " starts at index ", rlSorted$index[1]
      )
    }
  }
  #+++ Handling of special cases of meteo condition variables V1, V2, V3
  # If variables are at default values but do not exist as columns, set to 'none'
  # (= disabled identifier).
  # This allows running MDS with less variables than prescribed in the default setting.
  # If meteo condition variable are same as variable to fill, also set to 'none'.
  # This prevents filling artificial gaps (for uncertainty estimates) with itself
  # as meteo condition variable.
  #! Attention: Non-congruent with MR PV-Wave. There artificial gaps in
  #Rg, VPD, Tair are filled with itself.
  if ( (V1 ==   'Rg' && !(V1 %in% c(colnames(sDATA)))) || (V1 == Var) )   V1 <- 'none'
  if ( (V2 ==  'VPD' && !(V2 %in% c(colnames(sDATA)))) || (V2 == Var) )   V2 <- 'none'
  if ( (V3 == 'Tair' && !(V3 %in% c(colnames(sDATA)))) || (V3 == Var) )   V3 <- 'none'
  # Check column names (with 'none' as dummy)
  # (Numeric type and plausibility have been checked on initialization of sEddyProc)
  fCheckColNames(cbind(sDATA, sTEMP), c(V1, V2, V3), 'sMDSGapFill')
  # Check tolerance entries (if condition variable is not 'none')
  NoneCols.b <- c(V1, V2, V3) %in% 'none'
  if (!fCheckValNum(T1) || !fCheckValNum(T2) || !fCheckValNum(T3) ) stop(
    'sMDSGapFill::: T1, T2, T3, T4.n, T5.n must be numeric '
    , '(if not specified, set to NA_real_)!')
  if (sum(is.na(c(T1, T2, T3)[!NoneCols.b])) ) stop(
    'sMDSGapFill::: If condition variable is specified (dummy name is \'none\'), '
    , 'the tolerance interval must be specified!')
  # Run gap filling scheme depending on auxiliary meteo data availability
  ##details<<
  ## MDS gap filling algorithm calls the subroutines Look Up Table
  ## \code{\link{sEddyProc_sFillLUT}}
  ## and Mean Diurnal Course \code{\link{sEddyProc_sFillMDC}} with different
  ## window sizes as described in the reference.
  ##details<<
  ## To run dataset only with MDC algorithm \code{\link{sEddyProc_sFillMDC}},
  ## set condition variable V1 to 'none'.
  # Check availablility of meteorological data for LUT
  Met.n <-
    if (
      V1 != 'none' && V2 != 'none' && V3 != 'none'
      && sum(!is.na(sDATA[, V1])) != 0 && sum(!is.na(sDATA[, V2])) != 0 &&
      sum(!is.na(sDATA[, V3])) != 0
    ) {
      #All three meteo conditions are available and valid to use:
      message(
        'Full MDS algorithm for gap filling of \''
        , attr(sTEMP$VAR_f, 'varnames'), '\' with LUT('
        , V1, ', ', V2, ', ', V3, ') and MDC.')
      3
    } else if (V1 != 'none' && sum(!is.na(sDATA[, V1])) != 0) {
      #Only one meteo condition available for LUT
      message(
        'Limited MDS algorithm for gap filling of \''
        , attr(sTEMP$VAR_f, 'varnames'), '\' with LUT(', V1, ' only) and MDC.')
      1
    } else {
      #No meteo condition available (use MDC only)
      message(
        'Restriced MDS algorithm for gap filling of \''
        , attr(sTEMP$VAR_f, 'varnames')
        , '\' with no meteo conditions and hence only MDC.')
      if (Var != 'Rg') warning(
        'sMDSGapFill::: No meteo available for MDS gap filling!')
      0
    }
  #+++ Full MDS algorithm
  # Step 1: Look-up table (method 1) with window size +-7 days
  if (Met.n == 3) sFillLUT(7, V1, T1, V2, T2, V3, T3, Verbose.b = isVerbose)
  # Step 2: Look-up table (method 1) with window size +-14 days
  if (Met.n == 3) sFillLUT(14, V1, T1, V2, T2, V3, T3, Verbose.b = isVerbose)
  # Step 3: Look-up table, Rg only (method 2) with window size +-7 days,
  if (Met.n == 3 || Met.n == 1) sFillLUT(7, V1, T1, Verbose.b = isVerbose)
  # Step 4: Mean diurnal course (method 3) with window size 0 (same day)
  sFillMDC(0, Verbose.b = isVerbose)
  # Step 5: Mean diurnal course (method 3) with window size +-1, +-2 days
  sFillMDC(1, Verbose.b = isVerbose)
  sFillMDC(2, Verbose.b = isVerbose)
  # Step 6: Look-up table (method 1) with window size +-21, +-28, ..., +-70
  if (Met.n == 3) for (WinDays.i in seq(21, 70, 7) ) sFillLUT(
    WinDays.i, V1, T1, V2, T2, V3, T3, Verbose.b = isVerbose)
  # Step 7: Look-up table (method 2) with window size +-14, +-21, ..., +-70
  if (Met.n == 3 || Met.n == 1) for (WinDays.i in seq(14, 70, 7) ) sFillLUT(
    WinDays.i, V1, T1, Verbose.b = isVerbose)
  # Step 8: Mean diurnal course (method 3) with window size +-7, +-14, ..., +-210 days
  for (WinDays.i in seq(7, 210, 7) ) sFillMDC(WinDays.i, Verbose.b = isVerbose)

  # Set long gaps again to NA
  sTEMP$VAR_fall <<- suppressMessages(fConvertGapsToNA(sTEMP$VAR_fall))

  # Message on gap filling
  TimeDiff.p <- as.numeric(Sys.time()) - as.numeric(TimeStart.p)
  message(
    'Finished gap filling of \'', Var, '\' in ', floor(TimeDiff.p)
    , ' seconds. Artificial gaps filled: '
    , length(sTEMP$VAR_fall) - sum(is.na(sTEMP$VAR_fall))
    , ', real gaps filled: ', sum(is.na(sTEMP$VAR_orig))
    , ', unfilled (long) gaps: ', sum(is.na(sTEMP$VAR_fall)), '.')

  ##details<< \describe{\item{Different processing setups on the same dataset}{
  ## Attention: When processing the same site data set with different setups for
  ## the gap filling or flux partitioning
  ## (e.g. due to different ustar filters),
  ## a string suffix is needed! This suffix is added to the result column names
  ## to distinguish the results of the different setups.
  ## }}
  # Rename new columns generated during gap filling
  colnames(sTEMP) <<- gsub('VAR_', paste(
    Var, (if (fCheckValString(suffix)) '_' else '')
    , suffix, '_', sep = ''), colnames(sTEMP))
  # Check for duplicate columns (to detect if different processing
  # setups were executed without different suffixes)
  if (length(names(which(table(colnames(sTEMP)) > 1))) )  warning(
    'sMDSGapFill::: Duplicated columns found! Please specify different suffix '
    , 'when processing different setups on the same dataset!')
  return(invisible(NULL))
  ##value<<
  ## Gap filling results in sTEMP data frame (with renamed columns).
}
sEddyProc$methods(sMDSGapFill = sEddyProc_sMDSGapFill)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sMDSGapFillAfterUstar <- function(
  ### sEddyProc$sMDSGapFillAfterUstar - MDS gap filling algorithm after u* filtering
  fluxVar                ##<< Flux variable to gap fill after ustar filtering
  , uStarVar = 'Ustar'   ##<< Column name of friction velocity u * (ms-1),
  ## default 'Ustar'
  , uStarTh =        ##<< data.frame with
    ##  first column, season names, and second column estimates of uStar Threshold.
    ## Alternatively, a single value to be used as threshold for all records
    ## If only one value is given, it is used for all records.
    #usGetAnnualSeasonUStarMap(sUSTAR_DETAILS$uStarTh)
    .self$sGetUstarScenarios()[,c("season",uStarSuffix), drop = FALSE]
  , uStarSuffix = 'uStar'   ##<< Different suffixes required are for
  ## different u * scenarios
  , isFlagEntryAfterLowTurbulence = FALSE  ##<< Set to TRUE for flagging the
  ## first entry after low turbulance as bad condition (by value of 2).
  , isFilterDayTime = FALSE		##<< Set to TRUE to also filter day-time values,
  ## default only filters night-time data
  , swThr = 10			  ##<< threshold of solar radiation below which data is
  ## marked as night time respiration.
  , RgColName = "Rg" ##<< Column name of incoming short wave radiation
  , ...              ##<< Other arguments passed to \code{\link{sEddyProc_sMDSGapFill}}
) {
  'Calling sMDSGapFill after filtering for (provided) friction velocity u * '
  ##author<<
  ## AMM, TW
  ##details<<
  ## Calling \code{\link{sEddyProc_sMDSGapFill}} after filtering for
  ## (provided) friction velocity u*
  ##
  ## The u* threshold(s) are provided with argument \code{uStarTh} for
  ## filtering the conditions of low turbulence.
  ## After filtering, the data is gap filled using the MDS algorithm
  ## \code{\link{sEddyProc_sMDSGapFill}}.
  #
  ##seealso<<
  ## \itemize{
  ## \item \code{\link{sEddyProc_sEstimateUstarScenarios}} and
  ## \code{link{sEddyProc_sEstUstarThold}} for estimating the
  ## u* threshold from the data.
  ## \item \code{\link{sEddyProc_sMDSGapFillUStarScens}} for
  ## automated gapfilling for several scenarios of u* threshold estimates.
  ## }
  #
  uStarThresVec <- if (is.numeric(uStarTh) ) {
    if (length(uStarTh) != 1L) stop(
      "Without seasons, only a single uStarThreshold can be provided,"
      , " but got a vector.")
    uStarThresVec <- rep(uStarTh, nrow(.self$sDATA) )
  } else {
    if (!("season" %in% colnames(sTEMP)) ) stop(
      "Seasons not defined yet. Provide argument seasonFactor to sEstUstarThold.")
    # make sure merge will work
    colnames(uStarTh) <- c("season", "uStarThreshold")
    if (any(!is.finite(uStarTh$uStarThreshold))) stop(
      "must provide finite uStarThresholds")
    iMissingLevels <- which(!(levels(.self$sTEMP$season) %in% uStarTh$season))
    if (length(iMissingLevels) ) stop(
      "missing uStarTrheshold for seasons "
      , paste(levels(.self$sTEMP$season)[iMissingLevels], collapse = ", "))
    tmpDs <- merge(subset(sTEMP, select = "season"), uStarTh, all.x = TRUE)
    uStarThresVec <- tmpDs[, 2L]
  }
  # Check column names (with 'none' as dummy)
  # (Numeric type and plausibility have been checked on initialization of sEddyProc)
  fCheckColNames(sDATA, c(fluxVar, uStarVar), 'sMDSGapFillAfterUstar')

  # Filter data
  uStar <- sDATA[, uStarVar]
  qfUStar <- integer(nrow(sDATA) )	# 0L
  # if not filtering dayTimeValues, create a vector that is TRUE only for nightTime
  isRowFiltered <- if (isFilterDayTime) TRUE else
    (!is.finite(sDATA[, RgColName]) | sDATA[, RgColName] < swThr)
  # mark low uStar or bad uStar as 1L
  qfUStar[
    isRowFiltered &
      !is.na(uStarThresVec) &
      (sDATA[[uStarVar]] < uStarThresVec)
    ] <- 1L
  if (isTRUE(isFlagEntryAfterLowTurbulence) ) {
    ##details<<
    ## With \code{isFlagEntryAfterLowTurbulence set to TRUE}, to be more
    ## conservative, in addition
    ## to the data acquired when uStar is below the threshold,
    ## the first half hour measured with good turbulence conditions
    ## after a period with low turbulence is also removed (Papale et al. 2006).
    qfUStar[which(diff(qfUStar) == 1) + 1] <- 2L
  }
  # mark those conditions as bad, when no threshold is defined
  qfUStar[isRowFiltered & !is.finite(uStarThresVec) ]	<- 3L
  # mark those recods as bad, where uStar is not defined
  qfUStar[isRowFiltered & !is.finite(uStar) ]	<- 4L
  message(
    'Ustar filtering (u * Th_1 = ', uStarThresVec[1], '), marked '
    , (signif(sum(qfUStar != 0) / length(qfUStar), 2)) * 100
    , '% of the data as gap'  )
  if (isTRUE(isFlagEntryAfterLowTurbulence) ) {
    message(
      '(including removal of the first half-hour after a '
      , 'period of low turbulence).')
  }

  # Add filtering step to (temporal) results data frame
  suffixDash.s <- paste(
    (if (fCheckValString(uStarSuffix)) '_' else ''), uStarSuffix, sep = '')
  attr(uStarThresVec, 'varnames') <- paste(
    'Ustar', suffixDash.s, '_Thres', sep = '')
  attr(uStarThresVec, 'units') <- 'ms-1'
  attr(qfUStar, 'varnames') <- paste(
    'Ustar', suffixDash.s, '_fqc', sep = '')
  attr(qfUStar, 'units') <- '-'
  sTEMP$USTAR_Thres <<- uStarThresVec
  sTEMP$USTAR_fqc <<- qfUStar
  colnames(sTEMP) <<- gsub(
    'USTAR_', paste('Ustar', suffixDash.s, '_', sep = ''), colnames(.self$sTEMP))
  # Check for duplicate columns (to detect if different processing setups
  # were executed without different suffix)
  if (length(names(which(table(colnames(.self$sTEMP)) > 1))) )  {
    warning('sMDSGapFillAfterUstar::: Duplicated columns found!'
            , ' Please specify different suffix when processing different"
            , " setups on the same dataset!')
  }
  # Gap fill data after applying ustar filtering
  sMDSGapFill(
    fluxVar, QFVar = attr(qfUStar, 'varnames'), QFValue = 0, ...
    , suffix = uStarSuffix)
  ##value<<
  ## Vector with quality flag from filtering (here 0: good data
  ## , 1: low turbulence, 2: first half hour after low turbulence
  ## , 3: no threshold available, 4: missing uStar value)
  ## Gap filling results are in sTEMP data frame (with renamed columns)
  ## that can be retrieved by \code{\link{sEddyProc_sExportResults}}.
  return(invisible(qfUStar))
  # example in Eddy.R sEddyProc.example
  }
sEddyProc$methods(sMDSGapFillAfterUstar = sEddyProc_sMDSGapFillAfterUstar)


#' @export
sEddyProc_sMDSGapFillAfterUStarDistr <- function(
  ### GapFilling for several filters of estimated friction velocity Ustar thresholds.
  ...                 ##<< other arguments to
  ## \code{\link{sEddyProc_sMDSGapFillAfterUstar}} and \code{\link{sEddyProc_sMDSGapFill}}
  ## such as \code{fluxVar}
  , uStarTh		  ##<< data.frame with first column, season names,
  ## and remaining columns different estimates of uStar Threshold.
  ## If the data.frame has only one row, then each uStar threshold estimate
  ## is applied to the entire dataset.
  ## Entries in first column must match levels in argument \code{seasonFactor}
  , uStarSuffixes = colnames(uStarTh)[-1]  ##<< String vector
  ## to distinguish result columns for different ustar values.
  ## Its length must correspond to column numbers in \code{UstarThres.m.n}.
  # return value function \code{\link{sEddyProc_sEstUstarThresholdDistribution}}
) {
  ##details<< This method is superseedec by
  ##\code{\link{sEddyProc_sMDSGapFillUStarScens}} and only there
  ## for backward portability.
  warning(
    "Method sEddyProc_sMDSGapFillAfterUStarDistr has been deprecated. "
    , "Please, replace it by calls to 'sEddyProc_sMDSGapFillUStarScens' "
    , "and if applicable calling before 'sEddyProc_sSetUstarScenarios'."
    , "see ?sEddyProc_sMDSGapFillUStarScens and vignette('uStarCases')"
    )
  if (missing(uStarTh)) stop(
    "Need to provide argument uStarTh. Since version 1.1.6 "
    , " Please, use new methods sEddyProc_sSetUstarScenarios and"
    , " sEddyProc_sMDSGapFillUStarScens instead of this method."
    , " see ?sEddyProc_sMDSGapFillUStarScens and ?")
  .self$sSetUstarScenarios(uStarTh, uStarSuffixes)
  .self$sMDSGapFillUStarScens(...)
}
sEddyProc$methods(
  sMDSGapFillAfterUStarDistr = sEddyProc_sMDSGapFillAfterUStarDistr)

#' @export
sEddyProc_sMDSGapFillUStarScens <- function(
  ### GapFilling for several filters of estimated friction velocity Ustar thresholds.
  ...                 ##<< other arguments to
  ## \code{\link{sEddyProc_sMDSGapFillAfterUstar}} and
  ## \code{\link{sEddyProc_sMDSGapFill}}
  ## such as \code{fluxVar}
) {
  ##author<< TW
  ##details<<
  ## sEddyProc$sMDSGapFillUStarDistr: calling
  ## \code{\link{sEddyProc_sMDSGapFillAfterUstar}} for several filters of
  ## friction velocity Ustar.
  ##
  ## The scenarios need to be set before by
  ## \code{\link{sEddyProc_sSetUstarScenarios}} or accepting the defaults
  ## annual estimates of \code{link{sEddyProc_sEstimateUstarScenarios}}.
  ##
  ## Then the difference between output columns NEE_U05_f and NEE_U95_f
  ## corresponds to the uncertainty
  ## introduced by the uncertain estimate of the u* threshold.
  #
  ##seealso<<
  ## \href{../doc/useCase.html}{useCase vignette}
  uStarTh <- .self$sUSTAR_SCEN
  uStarSuffixes <- colnames(.self$sUSTAR_SCEN)[-1]
  nEstimates <- ncol(uStarTh) - 1L
  #iCol <- 1L
  filterCols <- lapply(seq(1L:nEstimates), function(iCol) {
    .self$sMDSGapFillAfterUstar(
      ...
      , uStarTh = uStarTh[, c(1L, 1L + iCol)]
      , uStarSuffix = uStarSuffixes[iCol]
    )
  })
  filterMat <- do.call(cbind, filterCols)
  return(invisible(filterMat))
  ##value<<
  ## Matrix (columns correspond to u* Scenarios) with quality flag from
  ## filtering ustar (0 - good data, 1 - filtered data)
  ##
  ## Gap filling results in sTEMP data frame (with renamed columns), that
  ## can be retrieved by \code{\link{sEddyProc_sExportResults}}.
  ## Each of the outputs is calculated for several u* r-estimates and
  ## distinguished by a suffix after the variable.
  ## E.g. with an an entry "U05" in \code{uStarSuffixes} in
  ## \code{\link{sEddyProc_sSetUstarScenarios}}
  ## the corresponding filled NEE can be found in output column "NEE_U05_f".
}
sEddyProc$methods(sMDSGapFillUStarScens = sEddyProc_sMDSGapFillUStarScens)
