
#' @export
sEddyProc_sGLFluxPartitionUStarScens <- function(
  ### Flux partitioning after Lasslop et al. (2010)
  ...  ##<< arguments to \code{\link{sEddyProc_sGLFluxPartition}}
  , uStarScenKeep = character(0) ##<< Scalar string specifying the scenario
  ## for which to keep parameters (see \code{\link{sEddyProc_sApplyUStarScen}}.
  ## Defaults to the first scenario.
) {
  ##details<<
  ## Daytime-based partitioning of measured net ecosystem fluxes into
  ## gross primary production (GPP) and ecosystem respiration (Reco)
  ## for all u* threshold scenarios.
  tmp <- sApplyUStarScen(
    .self$sGLFluxPartition, ..., uStarScenKeep = uStarScenKeep)
  NULL
}
sEddyProc$methods(
  sGLFluxPartitionUStarScens = sEddyProc_sGLFluxPartitionUStarScens)

#' @export
sEddyProc_sGLFluxPartition <- function(
  ### Daytime-based Flux partitioning after Lasslop et al. (2010)
  ...		##<< arguments to \code{\link{partitionNEEGL}} in addition to the dataset
  ## such as \code{suffix}
  , debug = list(   ##<< List with debugging control.
    ##describe<<
    useLocaltime = FALSE	##<< if TRUE use local time zone instead of
    ## geo-solar time to compute potential radiation
    ##end<<
  )
  , debug.l ##<< deprecated, renamed to debug
  , isWarnReplaceColumns = TRUE		##<< set to FALSE to avoid the warning on
  ## replacing output columns
) {
  if (!missing(debug.l)) {
    warning(
      "sEddyProc_sGLFluxPartition: argument name debug.l is deprecated. "
      , "use debug instead.")
    debug <- debug.l
  }
  ##details<<
  ## Daytime-based partitioning of measured net ecosystem fluxes into gross
  ## primary production (GPP)
  ## and ecosystem respiration (Reco)
  ##author<< MM, TW
  ##references<<
  ## Lasslop G, Reichstein M, Papale D, et al. (2010) Separation of net
  ## ecosystem exchange into assimilation and respiration using
  ## a light response curve approach: critical issues and global evaluation.
  ## Global Change Biology, Volume 16, Issue 1, Pages 187-208
  .self$sCalcPotRadiation(useSolartime = !isTRUE(debug$useLocaltime) )
  dsAns <- partitionNEEGL(cbind(.self$sDATA, .self$sTEMP), ...
                          , nRecInDay = sINFO$DTS
  )
  iExisting <- na.omit(match(colnames(dsAns), colnames(.self$sTEMP)  ))
  if (length(iExisting) ) {
    if (isWarnReplaceColumns) warning(
      "replacing existing output columns"
      , paste(colnames(.self$sTEMP)[iExisting], collapse = ", "))
    sTEMP <<- .self$sTEMP[, -iExisting]
  }
  sTEMP <<- cbind(.self$sTEMP, dsAns)
  return(invisible(NULL))
  ##value<<
  ## Flux partitioning results are in sTEMP data frame of the class.
}
sEddyProc$methods(sGLFluxPartition = sEddyProc_sGLFluxPartition)

#' @export
sEddyProc_sTKFluxPartitionUStarScens <- function(
  ### Flux partitioning after Lasslop 2015
  ...  ##<< arguments to \code{\link{sEddyProc_sTKFluxPartition}}
  , uStarScenKeep = character(0) ##<< Scalar string specifying the scenario
  ## for which to keep parameters (see \code{\link{sEddyProc_sApplyUStarScen}}.
  ## Defaults to the first scenario.
) {
  ##details<<
  ## Daytime-based partitioning of measured net ecosystem fluxes into
  ## gross primary production (GPP) and ecosystem respiration (Reco)
  ## for all u* threshold scenarios.
  tmp <- sApplyUStarScen(
    .self$sTKFluxPartition, ..., uStarScenKeep = uStarScenKeep )
  NULL
}
sEddyProc$methods(
  sTKFluxPartitionUStarScens = sEddyProc_sTKFluxPartitionUStarScens)

#' @export
sEddyProc_sTKFluxPartition <- function(
  ### Modified daytime-based Flux partitioning after Keenan et al. (2019)
  ...		##<< arguments to \code{\link{sEddyProc_sGLFluxPartition}}
  ## in addition to the dataset
  , controlGLPart = partGLControl()	##<< further default parameters,
  ## such as \code{suffix}
) {
  warning("Modified daytime partioning (Keenan 2019) is experimental. Use with caution.")
  controlGLPart$useNightimeBasalRespiration <- TRUE
  .self$sGLFluxPartition(..., controlGLPart = controlGLPart)
  ##value<<
  ## Flux partitioning results are in sTEMP data frame of the class.
}
sEddyProc$methods(sTKFluxPartition = sEddyProc_sTKFluxPartition)

#' @export
sEddyProc_sMRFluxPartitionUStarScens <- function(
  ### Flux partitioning after Reichstein et al. (2005)
  ...  ##<< arguments to \code{\link{sEddyProc_sMRFluxPartition}}
  , uStarScenKeep = character(0) ##<< Scalar string specifying the scenario
  ## for which to keep parameters (see \code{\link{sEddyProc_sApplyUStarScen}}.
  ## Defaults to the first scenario.
) {
  ##details<<
  ## Nighttime-based partitioning of measured net ecosystem fluxes into
  ## gross primary production (GPP) and ecosystem respiration (Reco)
  ## for all u* threshold scenarios.
  tmp <- sApplyUStarScen(
    .self$sMRFluxPartition, ..., uStarScenKeep = uStarScenKeep)
  ##value<< NULL, it adds output columns in the class
  invisible(tmp)
}
sEddyProc$methods(
  sMRFluxPartitionUStarScens = sEddyProc_sMRFluxPartitionUStarScens)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc methods for flux partitioning +++
#+++ Flux partitionig algorithm, adapted after the PV-Wave code and paper by
#+++ Markus Reichstein +++
#+++ Dependencies: Eddy.R, DataFunctions.R
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
sEddyProc_sMRFluxPartition <- function(
  ### Nighttime-based partitioning of net ecosystem fluxes into gross fluxes GPP and REco
  FluxVar = if (missing(FluxVar.s)) 'NEE_f' else FluxVar.s     ##<< Variable
  ##  name of column with original and
  ## filled net ecosystem fluxes (NEE)
  , QFFluxVar = if (missing(QFFluxVar.s)) 'NEE_fqc' else QFFluxVar.s ##<< Quality
  ##  flag of NEE variable
  , QFFluxValue = if (missing(QFFluxValue.n)) 0L else QFFluxValue.n  ##<< Value
  ##  of quality flag for _good_ (original) data
  , TempVar = if (missing(TempVar.s)) 'Tair_f' else TempVar.s    ##<< Filled
  ##  air- or soil temperature variable (degC)
  , QFTempVar = if (missing(QFTempVar.s)) 'Tair_fqc' else QFTempVar.s ##<< Quality
  ##  flag of filled temperature variable
  , QFTempValue = if (missing(QFTempValue.n)) 0 else QFTempValue.n ##<< Value
  ##  of temperature quality flag for _good_
  ## (original) data
  , RadVar = if (missing(RadVar.s)) 'Rg' else RadVar.s     ##<< Unfilled
  ##  (original) radiation variable
  , TRef = if (missing(T_ref.n)) 273.15 + 15 else T_ref.n     ##<< Reference
  ##  temperature in Kelvin (degK)
  ## used in \code{fLloydTaylor} for regressing Flux and Temperature
  , suffix = if (missing(Suffix.s)) '' else Suffix.s		     ##<< String
  ##  suffix needed for different processing
  ## setups on the same dataset (for explanations see below)
  , FluxVar.s ##<< deprecated
  , QFFluxVar.s ##<< deprecated
  , QFFluxValue.n ##<< deprecated
  , TempVar.s ##<< deprecated
  , QFTempVar.s ##<< deprecated
  , QFTempValue.n ##<< deprecated
  , RadVar.s ##<< deprecated
  , T_ref.n ##<< deprecated
  , Suffix.s ##<< deprecated
  , debug.l ##<< deprecated
  , debug = if (!missing(debug.l)) debug.l else list( ##<< List
    ## with debugging control
    ## (passed also to \code{sEddyProc_sRegrE0fromShortTerm}
    ## for providing \code{fixedE0 = myE0}).
    ##describe<<
    useLocaltime = FALSE	##<< see details on solar vs local time
    ##end<<
  )
  , parsE0Regression = list() ##<<list
  ## with further parameters passed down to
  ## \code{sEddyProc_sRegrE0fromShortTerm} and \code{fRegrE0fromShortTerm},
  ## such as \code{TempRange}
) {
  varNamesDepr <- c(
    "FluxVar.s","QFFluxVar.s","QFFluxValue.n","TempVar.s","QFTempVar.s"
    ,"QFTempValue.n","RadVar.s","T_ref.n","Suffix.s","debug.l")
  varNamesNew <- c(
    "FluxVar","QFFluxVar","QFFluxValue","TempVar","QFTempVar"
    ,"QFTempValue","RadVar","TRef","suffix","debug")
  iDepr = which(!c(
    missing(FluxVar.s),missing(QFFluxVar.s),missing(QFFluxValue.n)
    ,missing(TempVar.s),missing(QFTempVar.s),missing(QFTempValue.n)
    ,missing(RadVar.s),missing(T_ref.n),missing(Suffix.s),missing(debug.l)))
  if (length(iDepr)) warning(
    "Argument names ",paste(varNamesDepr[iDepr], collapse = ",")
    ," have been deprecated."
    ," Please, use instead ", paste(varNamesNew[iDepr], collapse = ","))
  ##references<<
  ## Reichstein M, Falge E, Baldocchi D et al. (2005) On the separation of
  ## net ecosystem exchange
  ## into assimilation and ecosystem respiration: review and improved algorithm.
  ## Global Change Biology, 11, 1424-1439.
  ##details<< \describe{\item{
  ##Description of newly generated variables with partitioning results:}{
  ## \itemize{
  ## \item PotRad - Potential radiation \cr
  ## \item FP_NEEnight - Good (original) NEE nighttime fluxes used for flux partitioning \cr
  ## \item FP_Temp - Good (original) temperature measurements used for flux partitioning \cr
  ## \item E_0 - Estimated temperature sensitivity \cr
  ## \item R_ref - Estimated reference respiration \cr
  ## \item Reco - Estimated ecosystem respiration \cr
  ## \item GPP_f - Estimated gross primary production \cr
  ## }
  ## }}
  #
  ##details<< \describe{\item{Background}{
  ## This partitioning is based on the regression of nighttime respiration with
  ## temperature using the Lloyd-Taylor-Function \code{\link{fLloydTaylor}}.
  ## First the temperature sensitivity E_0 is estimated from short term data,
  ## see \code{sEddyProc_sRegrE0fromShortTerm}.
  ## Next the reference temperature R_ref is estimated for successive periods
  ## throughout the whole dataset (see \code{sEddyProc_sRegrRref}).
  ## These estimates are then used to calculate the respiration during daytime
  ## and nighttime and with this GPP.
  ## Attention: Gap filling of the net ecosystem fluxes (NEE) and temperature
  ## measurements (Tair or Tsoil) is required
  ## prior to the partitioning!
  ## }}
  #
  # If the default variable names are used and a suffix is provided, then the
  # suffix is added to the variable name (see also comment below)
  if (FluxVar == 'NEE_f' && QFFluxVar == 'NEE_fqc' && fCheckValString(suffix) ) {
    FluxVar <- paste('NEE_', suffix, '_f', sep = '')
    QFFluxVar <- paste('NEE_', suffix, '_fqc', sep = '')
  }
  #
  # Check if specified columns exist in sDATA or sTEMP and if numeric and plausible.
  # Then apply quality flag
  # TODO: avoid repeated cbind
  # TODO: checking column names this does not need a full combined data.frame
  sDT <- cbind(.self$sDATA, .self$sTEMP)
  fCheckColNames(
    sDT
    , c(FluxVar, QFFluxVar, TempVar, QFTempVar, RadVar)
    , 'sMRFluxPartition')
  fCheckColNum(
    sDT
    , c(FluxVar, QFFluxVar, TempVar, QFTempVar, RadVar)
    , 'sMRFluxPartition')
  fCheckColPlausibility(
    sDT
    , c(FluxVar, QFFluxVar, TempVar, QFTempVar, RadVar)
    , 'sMRFluxPartition')
  Var.V.n <- fSetQF(
    sDT, FluxVar, QFFluxVar, QFFluxValue
    , 'sMRFluxPartition')

  message('Start flux partitioning for variable ', FluxVar
          , ' with temperature ', TempVar, '.')

  ##details<< \describe{\item{Selection of daytime data based on solar time}{
  ## The respiration-temperature regression is very
  ## sensitive to the selection of night- and daytime data.
  ## Nighttime is selected by a combined threshold of current solar radiation
  ## and potential radiation.
  ## The current implementation calculates potential radiation based on exact
  ## solar time, based on latitude and longitude.
  ## (see \code{\link{fCalcPotRadiation}})
  ## Therefore it might differ from implementations that use local winter
  ## clock time instead.
  ## }}
  # Calculate potential radiation
  #! New code: Local time and equation of time accounted for in potential
  #radiation calculation
  .self$sCalcPotRadiation(useSolartime = !isTRUE(debug$useLocaltime) )
  #
  # Filter night time values only
  #! Note: Rg <= 10 congruent with MR PV-Wave, in paper Rg <= 20
  # Should be unfilled (original) radiation variable, therefore dataframe set
  # to sDATA only
  sTEMP$FP_VARnight <<- ifelse(
    sDATA[, RadVar] > 10 | .self$sTEMP$PotRad_NEW > 0, NA,  Var.V.n)
  attr(sTEMP$FP_VARnight, 'varnames') <<- paste(
    attr(Var.V.n, 'varnames'), '_night', sep = '')
  attr(sTEMP$FP_VARnight, 'units') <<- attr(Var.V.n, 'units')
  #! New code: Slightly different subset than PV-Wave due
  #to time zone correction (avoids timezone offset between Rg and PotRad)
  #
  # Apply quality flag for temperature
  sTEMP$FP_Temp_NEW <<- fSetQF(
    sDT, TempVar, QFTempVar, QFTempValue
    , 'sMRFluxPartition')
  #
  # Estimate E_0 and R_ref (results are saved in sTEMP)
  # twutz1508: changed to do.call in order to allow passing further parameters
  # when calling sMRFluxPartition
  sTEMP$E_0_NEW <<- do.call(.self$sRegrE0fromShortTerm, c(
    list('FP_VARnight', 'FP_Temp_NEW', TRef = TRef
         , CallFunction = 'sMRFluxPartition', debug = debug)
    , parsE0Regression))
  if (sum(.self$sTEMP$E_0_NEW == -111) != 0)
    return(invisible(-111)) # Abort flux partitioning if regression of E_0 failed
  #
  # Reanalyse R_ref with E_0 fixed
  sTEMP$R_ref_NEW <<- .self$sRegrRref(
    'FP_VARnight', 'FP_Temp_NEW', 'E_0_NEW', TRef = TRef
    , CallFunction = 'sMRFluxPartition')
  #
  # Calculate the ecosystem respiration Reco
  sTEMP$Reco_NEW <<- fLloydTaylor(
    .self$sTEMP$R_ref_NEW, .self$sTEMP$E_0_NEW
    , fConvertCtoK(sDT[, TempVar])
    , TRef = TRef)
  attr(sTEMP$Reco_NEW, 'varnames') <<- 'Reco'
  attr(sTEMP$Reco_NEW, 'units') <<- attr(Var.V.n, 'units')

  # Calculate the gross primary production GPP_f
  sTEMP$GPP_NEW_f <<-
    -sDT[, FluxVar] + .self$sTEMP$Reco_NEW
  sTEMP$GPP_NEW_fqc <<- sDT[, QFFluxVar]
  #! New code: MDS gap filling information are not copied from NEE_fmet and
  #NEE_fwin to GPP_fmet and GPP_fwin
  #           (since not known within this pure partitioning function)
  attr(sTEMP$GPP_NEW_f, 'varnames') <<- 'GPP_f'
  attr(sTEMP$GPP_NEW_f, 'units') <<- attr(Var.V.n, 'units')
  #
  ##details<< \describe{\item{Different processing setups on the same dataset}{
  ## Attention: When processing the same site data set with different setups for
  ## the gap filling or flux partitioning
  ## (e.g. due to different ustar filters),
  ## a string suffix is needed! This suffix is added to the result column names
  ## to distinguish the results of the different setups.
  ## If a suffix is provided and if the defaults for FluxVar and QFFluxVar
  ## are used, the suffix will be added to their variable names
  ## (e.g. 'NEE_f' will be renamed to 'NEE_uStar_f' and 'NEE_fqc' to
  ## 'NEE_uStar_fqc' for the suffix = 'uStar').
  ## Currently, this works only with defaults of FluxVar = 'NEE_f'
  ## and QFFluxVar = 'NEE_fqc'.
  ## }}
  # Rename new columns generated during flux partitioning:
  # For nighttime NEE (FP_NEEnight or FP_NEEnight_Suffix)
  colnames(sTEMP) <<- gsub( '_VARnight', paste(
    '_NEEnight', (if (fCheckValString(suffix)) '_' else ''), suffix, sep = '')
    , colnames(.self$sTEMP))
  # For the results columns, the _NEW is dropped and the suffix added
  colnames(sTEMP) <<- gsub('_NEW', paste(
    (if (fCheckValString(suffix)) '_' else ''), suffix, sep = '')
    , colnames(.self$sTEMP))
  # Check for duplicate columns (to detect if different processing setups
  # were executed without different suffixes)
  if (length(names(iDupl <- which(table(colnames(.self$sTEMP)) > 1))) )  {
    warning(
      'sMRFluxPartition::: Duplicated columns found! (',
      paste(names(iDupl), collapse = ", ")
      , ')  Deleting each first of duplicate columns.'
      , ' Please use different suffix when processing different '
      , 'setups on the same dataset!')
    # need to remove columns else some tests fail
    for (cname in names(iDupl) ) sTEMP[cname] <<- NULL
  }

  return(invisible(NULL))
  ##value<<
  ## Flux partitioning results (see variables in details) in sTEMP data frame
  ## (with renamed columns).
  ## On success, return value is NULL. On failure an integer scalar error code
  ## is returned:
  ## -111 if regression of E_0 failed due to insufficient relationship in the data.
}
sEddyProc$methods(sMRFluxPartition = sEddyProc_sMRFluxPartition)


#' @export
sEddyProc_sCalcPotRadiation <- function(
  ### compute potential radiation from position and time
  useSolartime = TRUE	##<<
  ## by default corrects hour (given in local winter time)
  ## for latitude to solar time(where noon is exactly at 12:00).
  ## Set this to FALSE to directly use local winter time
  , useSolartime.b 	##<< by default corrects hour (given in local winter time)
) {
  if (!missing(useSolartime.b)) {
    useSolartime <- useSolartime.b
    warning(
      "sEddyProc_sCalcPotRadiation: argument name useSolartime.b is deprecated"
      , "use instead useSolartime")
  }
  # queriing $hour is timezone agnostic, Also works if sDateTime as GMT while
  # actual time being in another time zone
  DoY.V.n <- as.POSIXlt(sDATA$sDateTime)$yday + 1L
  Hour.V.n <-
    as.POSIXlt(sDATA$sDateTime)$hour + as.POSIXlt(sDATA$sDateTime)$min / 60
  # Check that location info has been set
  if (!(is.finite(sLOCATION$LatDeg) &
        is.finite(sLOCATION$LongDeg) &
        is.finite(sLOCATION$TimeZoneHour)))
    stop(
      "Need to set valid location information (sSetLocationInfo) before "
      , "calling sCalcPotRadiation.")
  ##value<< column PotRad_NEW in sTEMP
  sTEMP$PotRad_NEW <<- fCalcPotRadiation(
    DoY.V.n, Hour.V.n, sLOCATION$LatDeg, sLOCATION$LongDeg
    , sLOCATION$TimeZoneHour, useSolartime = useSolartime)
}
sEddyProc$methods(sCalcPotRadiation = sEddyProc_sCalcPotRadiation)


fOptimSingleE0 <- function(
  ##title<<
  ## Estimate temperature sensitivity E_0 using a Newton type optimization
  ##description<<
  ## Estimate temperature sensitivity E_0 of \code{\link{fLloydTaylor}} for a
  ## single time series
  ## using a Newton type optimization.
  NEEnight	  ##<< (Original) nighttime ecosystem carbon flux, i.e.
  ## respiration vector
  , TempKelvin	##<< (Original) air or soil temperature vector (degC)
  , TRef #= 273.15 + 15  ##<< Reference temperature in Kelvin (degK) used
  ## in \code{fLloydTaylor} for regressing Flux and Temperature
  , percTrim = 5       ##<< Percentile to trim residual (%)
  , recoverOnError = FALSE	##<< Set to TRUE to debug errors instead of catching them
  , algorithm = "default"  ##<< optimization algorithm used (see \code{\link{nls}})
) {
  # Original implementation by AMM
  res <- tryCatch({
    # Non-linear regression
    NLS.L <- nls(
      formula = REco ~ fLloydTaylor(RRef, E0, Temp, TRef = TRef)
      , trace = FALSE
      , data = data.frame(REco = NEEnight, Temp = TempKelvin)
      , start = list(RRef = 2, E0 = 200)
      , algorithm = algorithm
    )
    # Remove points with residuals outside percTrim quantiles
    Residuals.V.n <- resid(NLS.L)
    #Residuals.V.n <- fLloydTaylor(R_ref = coef(summary(NLS.L))['R_ref', 1]
    #, E_0 = coef(summary(NLS.L))['E_0', 1],
    #		TempKelvin, TRef = TRef) - NEEnight
    t.b <- Residuals.V.n >= quantile(Residuals.V.n, probs = c(percTrim / 100)) &
      Residuals.V.n <= quantile(Residuals.V.n, probs = c(1 - percTrim / 100))
    # Trimmed non-linear regression
    NLS_trim.L <- nls(
      formula = REco ~ fLloydTaylor(RRef, E0, Temp, TRef = TRef)
      , algorithm = 'default', trace = FALSE
      , data = data.frame(REco = NEEnight[t.b], Temp = TempKelvin[t.b])
      , start = list(RRef = 2, E0 = 200))
    ##value<< Numeric vector with following components:
    # TODO: clean up components in results
    res <- c(
      R_ref =           ##<< Estimate of respiration rate at reference temperature
        coef(summary(NLS.L))['RRef', 1]
      , R_ref_SD =      ##<< Standard deviation of RRef
        coef(summary(NLS.L))['RRef', 2]
      , E_0 =           ##<< Estimate of temperature sensitivity
        ## ("activation energy") in Kelvin (degK) for untrimmed dataset
        coef(summary(NLS.L))['E0', 1]
      , E_0_SD =        ##<< Standard deviation of E0
        coef(summary(NLS.L))['E0', 2]
      , E_0_trim =      ##<< Estimate of temperature sensitivity
        ## ("activation energy") in Kelvin (degK) for trimmed dataset
        coef(summary(NLS_trim.L))['E0', 1]
      , E_0_trim_SD =   ##<< Standard deviation of E_0_trim
        coef(summary(NLS_trim.L))['E0', 2]
      ##end<<
    )
    # Note on other tested algorithms:
    # Standard require(stats) nls with PORT algorithm and lower and upper bounds
    # require(FME) for modFit and modCost, has PORT algorithm included
    # (and other algorithms like MCMC)
    # require(robustbase) for ltsReg but only linear regression
    # require(nlme) for heteroscedastic and mixed NLR but no port algo with
    # upper and lower bounds
    # require(nlstools) for bootstrapping with nlsBoot(nls...)
  }, error = function(e) {
    if (isTRUE(recoverOnError) ) recover()
    res <- c(
      R_ref = NA, R_ref_SD = NA, E_0 = NA, E_0_SD = NA, E_0_trim = NA, E_0_trim_SD = NA)
  } ) #Spaces between brackets required to avoid replacement on documentation generation
  res
}

fOptimSingleE0_Lev <- function(
  ##title<<
  ## Estimate temperature sensitivity E_0 using Levenberg-Marquard optimization
  ##description<<
  ## Estimate temperature sensitivity E_0 of \code{\link{fLloydTaylor}} for a
  ## single time series
  ## using Levenberg-Marquard optimization.
  NEEnight	 	##<< (Original) nighttime ecosystem carbon flux, i.e.
  ## respiration vector
  , TempKelvin	##<< (Original) air or soil temperature vector (degC)
  , TRef #= 273.15 + 15   ##<< Reference temperature in Kelvin (degK) used
  ## in \code{fLloydTaylor} for regressing Flux and Temperature
  , percTrim = 5      	##<< Percentile to trim residual (%)
  , recoverOnError = FALSE	##<< Set to TRUE to debug errors instead of catching them
  , algorithm = 'LM' 	##<< optimization algorithm used (see nlsLM from package minpack.lm)
) {
  if (!requireNamespace('minpack.lm') ) stop(
    "Need to install package minpack.lm before using LM optimization.")
  res <- tryCatch({
    # Non-linear regression
    NLS.L <- minpack.lm::nlsLM(
      formula = R_eco ~ fLloydTaylor(R_ref, E_0, Temp, TRef = TRef)
      , trace = FALSE
      , data = data.frame(R_eco = NEEnight, Temp = TempKelvin)
      , start = list(R_ref = 2, E_0 = 200)
      , control = minpack.lm::nls.lm.control(maxiter = 20)
      , algorithm = algorithm
    )
    # Remove points with residuals outside percTrim quantiles
    Residuals.V.n <- resid(NLS.L)
    #plot(Residuals.V.n ~ TempKelvin)
    t.b <- Residuals.V.n >= quantile(Residuals.V.n, probs = c(percTrim / 100)) &
      Residuals.V.n <= quantile(Residuals.V.n, probs = c(1 - percTrim / 100))
    #points(Residuals.V.n[!t.b] ~ TempKelvin[!t.b], col = "red")
    # Trimmed non-linear regression
    NLS_trim.L <- minpack.lm::nlsLM(
      formula = R_eco ~ fLloydTaylor(R_ref, E_0, Temp, TRef = TRef)
      , algorithm = 'default', trace = FALSE
      , data = data.frame(R_eco = NEEnight[t.b], Temp = TempKelvin[t.b])
      , start = coef(NLS.L)
      #, start = list(R_ref = 2, E_0 = 200)
    )
    ##value<< Numeric vector with following components:
    res <- c(
      R_ref =           ##<< Estimate of respiration rate at reference temperature
        coef(summary(NLS.L))['R_ref', 1]
      , R_ref_SD =      ##<< Standard deviation of R_ref
        coef(summary(NLS.L))['R_ref', 2]
      , E_0 =           ##<< Estimate of temperature sensitivity
        ## ("activation energy") in Kelvin (degK) for untrimmed dataset
        coef(summary(NLS.L))['E_0', 1]
      , E_0_SD =        ##<< Standard deviation of E_0
        coef(summary(NLS.L))['E_0', 2]
      , E_0_trim =      ##<< Estimate of temperature sensitivity
        ## ("activation energy") in Kelvin (degK) for trimmed dataset
        coef(summary(NLS_trim.L))['E_0', 1]
      , E_0_trim_SD =   ##<< Standard deviation of E_0_trim
        coef(summary(NLS_trim.L))['E_0', 2]
      ##end<<
    )
  }, error = function(e) {
    if (isTRUE(recoverOnError) ) recover()
    res <- c(
      R_ref = NA, R_ref_SD = NA, E_0 = NA, E_0_SD = NA, E_0_trim = NA, E_0_trim_SD = NA)
  } ) #Spaces between brackets required to avoid replacement on documentation generation
  res
}

fRegrE0fromShortTerm = function(
  ##title<<
  ## Estimation of the temperature sensitivity E_0
  ##description<<
  ## Estimation of the temperature sensitivity E_0 from regression of
  ## \code{\link{fLloydTaylor}} for short periods
  NightFlux		    ##<< (Original) nighttime ecosystem carbon flux, i.e.
  ## respiration vector
  , Temp       ##<< (Original) air or soil temperature vector (degC)
  , DayCounter 	  ##<< Integer specifying the day of each record
  , WinDays = 7     ##<< Window size for \code{\link{fLloydTaylor}} regression in days
  , DayStep = 5     ##<< Window step for \code{\link{fLloydTaylor}} regression in days
  , TempRange = TempRange.n ##<< Threshold temperature range-width in Kelvin
  ## to start regression
  , TempRange.n = 5   ##<< deprecated, use TempRange
  , percTrim = 5        ##<< Percentile to trim residual (%)
  , NumE0 = 3      ##<< Number of best E_0's to average over
  , MinE0 = 30  		##<< Minimum E0 for validity check
  , MaxE0 = 450	  ##<< Maximum E0 for validity check
  , TRef	#= 273.15 + 15   ##<< Reference temperature in Kelvin (degK) used
  ## in \code{fLloydTaylor} for regressing Flux and Temperature
  , CallFunction = ''  ##<< Name of function called from
  , optimAlgorithm = 'default'   ##<< optimization algorithm used
  ## (see \code{\link{nls}}) or 'LM' for Levenberg-Marquard
  ## (see nlsLM from package minpack.lm)
) {
  if (!missing(TempRange.n)) warning(
    "Argument TempRange.n has been deprecated. Use TempRange instead.")
  ##details<<
  ##The coefficient E0 is estimated for windows with a length of
  ##\code{WinDays} days, for successive periods in steps of \code{DayStep}
  ##days. Only those windows with a sufficient number or records and with a
  ##sufficient temperature range \code{TempRange} are used for the
  ##\code{\link{fLloydTaylor}} regression of E0 using the optimization
  ##\code{\link{fOptimSingleE0}}. Unreasonable estimates are discarded (95%
  ##confidence interval inside \code{MinE0} and \code{MaxE0}) and the
  ##others are ordered by their standard deviations. The mean across the best (=
  ##lowest standard deviation) E0 estimates is reported with \code{NumE0}
  ##defining the number of best estimates to use.
  #
  # Regression settings
  #NLSRes.F <- data.frame(NULL) #Results of non-linear regression
  #NLSRes_trim.F <- data.frame(NULL) #Results of non-linear regression
  MinData.n <- 6 # Minimum number of data points
  #
  fOptim <- fOptimSingleE0
  if (optimAlgorithm == 'LM') fOptim <- fOptimSingleE0_Lev
  #tw: better use rbind with a list instead of costly repeated extending a data.frame
  NLSRes.F <- as.data.frame(do.call(
    rbind, NLSRes.l <- lapply(
      seq(WinDays + 1, max(DayCounter), DayStep), function(DayMiddle.i) {
    #TEST: DayMiddle.i <- 8
    DayStart.i <- DayMiddle.i - WinDays
    DayEnd.i <- DayMiddle.i + WinDays
    #! Window size of 7 days corresponds to full window length of 15 days as
    # in paper, non-congruent with PV-Wave code of 14 days
    #! New code: Last window has minimum width of WinDays
    #
    Subset.b <-
      DayCounter >= DayStart.i &
      DayCounter <= DayEnd.i &
      !is.na(NightFlux) &
      !is.na(Temp)
    NEEnight <- subset(NightFlux, Subset.b)
    Temp.V.n <- subset(Temp, Subset.b)
    TempKelvin <- fConvertCtoK(Temp.V.n)
    #
    if (length(NEEnight) > MinData.n && diff(range(TempKelvin)) >= TempRange) {
      #CountRegr.i <- CountRegr.i + 1
      resOptim <- fOptim(
        NEEnight, TempKelvin, algorithm = optimAlgorithm, TRef = TRef)
      NLSRes.F <- c(
        Start = DayStart.i, End = DayEnd.i, Num = length(NEEnight)
        , TRange = diff(range(TempKelvin)),
        resOptim)
    } else NULL
  }) ))
  Limits.b <- (NLSRes.F$E_0_trim - NLSRes.F$E_0_trim_SD > MinE0 &
                 NLSRes.F$E_0_trim + NLSRes.F$E_0_trim_SD < MaxE0)
  #! New code: Check validity with SD (standard deviation) limits, in PV-Wave
  #without SD, in paper if E_0_SD < (E_0 * 50%)
  NLSRes.F$E_0_trim_ok <- ifelse(Limits.b, NLSRes.F$E_0_trim, NA)
  NLSRes.F$E_0_trim_SD_ok <- ifelse(Limits.b, NLSRes.F$E_0_trim_SD, NA)
  #
  # Sort data frame for smallest standard deviation
  NLSsort.F <- NLSRes.F[order(NLSRes.F$E_0_trim_SD_ok), ] # ordered data.frame
  ##details<<
  ## Take average of the three E_0 with lowest standard deviation
  E_0_trim.n <- round(mean(NLSsort.F$E_0_trim_ok[1:NumE0]), digits = 2)
  #
  # Abort flux partitioning if regression of E_0 failed
  if (is.na(E_0_trim.n) ) {
    # twutz 150226: just warning and returning negative value gives problems
    # later on, maybe better stop and catch exception
    warning(CallFunction, ':::fRegrE0fromShortTerm::: Less than ', NumE0
            , ' valid values for E_0 after regressing ',
            nrow(NLSRes.F), ' periods! Aborting partitioning.\n'
            ,'You may try relaxing the temperature range constraint by '
            , 'setting TempRange = 3 (instead of default 5C). '
            , 'See argument parsE0Regression in sMRFluxPartitioning.')
    return(-111)
  }
  E_0_trim.n
  ##value<<
  ## Estimated scalar temperature sensitivity (E_0, degK)
}


sEddyProc_sRegrE0fromShortTerm <- function(
  ##title<<
  ## sEddyProc$sRegrE0fromShortTerm - Estimation of the temperature sensitivity E_0
  ##description<<
  ## Estimation of the temperature sensitivity E_0 from regression of
  ## \code{\link{fLloydTaylor}}
  ## for short periods by calling \code{fRegrE0fromShortTerm}
  NightFluxVar   ##<< Variable with (original) nighttime ecosystem carbon flux,
  ## i.e. respiration
  , TempVar   ##<< Variable with (original) air or soil temperature (degC)
  , ...				  ##<< Parameters passed to \code{fRegrE0fromShortTerm}
  , CallFunction = ''    ##<< Name of function called from
  , debug = list(fixedE0 = NA) ##<< List with controls for debugging, see details
) {
  ##details<< For further details see \code{fRegrE0fromShortTerm}.
  #
  # Check if specified columns are numeric
  SubCallFunc.s <- paste(CallFunction, 'sRegrE0fromShortTerm', sep = ':::')
  sDT <- cbind(.self$sDATA, .self$sTEMP)
  fCheckColNames(sDT, c(NightFluxVar, TempVar), SubCallFunc.s)
  fCheckColNum(sDT, c(NightFluxVar, TempVar), SubCallFunc.s)
  #
  ##details<< \describe{ \item{Debugging control}{
  ## When supplying a finite scalar value \code{debug$fixedE0}, then this value
  ## is used instead of the temperature sensitivity E_0 from short term data.
  ## }}
  if ( (length(debug$fixedE0) != 0) && is.finite(debug$fixedE0) ) {
    E_0_trim.n <- debug$fixedE0[1]
    message(
      'Using prescribed temperature sensitivity E0 of: '
      , format(E_0_trim.n, digits = 5), '.')
  }else{
    # Write into vectors since cbind of data frames is so slow (factor of ~20 (!))
    NightFlux <- sDT[[NightFluxVar]]
    Temp <- sDT[[TempVar]]
    DayCounter <- c(1:sINFO$DIMS) %/% sINFO$DTS
    #
    # Check for validity of E_0 regression results
    if (grepl('Tair', TempVar) ) {
      #Limits in PV-Wave code for Tair
      MinE0 <- 30; MaxE0 <- 350
    } else if (grepl('Tsoil', TempVar) ) {
      #Limits in PV-Wave code for Tsoil
      ## Higher values due to potentially high Q10 values
      MinE0 <- 30; MaxE0 <- 550
    } else {
      #Default limits taken from paper
      MinE0 <- 30; MaxE0 <- 450
    }
    #
    E_0_trim.n <- fRegrE0fromShortTerm(
      NightFlux, Temp, DayCounter, ...,
      MinE0 = MinE0, MaxE0 = MaxE0, CallFunction = SubCallFunc.s)
    message(
      'Estimate of the temperature sensitivity E_0 from short term data: '
      , format(E_0_trim.n, digits = 5), '.')
  }

  # Add constant value of E_0 as column vector to sTEMP
  E_0.V.n <- rep(E_0_trim.n, nrow(.self$sTEMP))
  attr(E_0.V.n, 'varnames') <- 'E_0'
  attr(E_0.V.n, 'units') <- 'degK'

  E_0.V.n
  ##value<<
  ## Data vector with (constant) temperature sensitivity (E_0, degK)
  ## On failure, all entries are set to -111
}
sEddyProc$methods(sRegrE0fromShortTerm = sEddyProc_sRegrE0fromShortTerm)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc_sRegrRref <- function(
  ### Estimation of the reference periods respiration Rref of \code{\link{fLloydTaylor}}
  NightFluxVar     ##<< Variable with (original) nighttime ecosystem carbon flux,
  ## i.e. respiration
  , TempVar     ##<< Variable with (original) air or soil temperature (degC)
  , E0Var         ##<< Temperature sensitivity E_0 estimated with
  ## \code{sEddyProc_sRegrE0fromShortTerm}
  , TRef #= 273.15 + 15   ##<< Reference temperature in Kelvin (degK)
  ## used in \code{fLloydTaylor} for regressing Flux and Temperature
  , WinDays = 3 ##<< Window size for \code{\link{fLloydTaylor}} regression in days
  , DayStep = 4 ##<< Window step for \code{\link{fLloydTaylor}} regression in days
  , CallFunction = ''    ##<< Name of function called from
) {
  ##details<<
  ## While parameter E0 in the Temperature-Respiration relationship
  ## (\code{\link{fLloydTaylor}}) is kept konstant,
  ## parameter Rref is allowed to change with time.
  ## This method estimates Rref for a series of time windows of length
  ## 2 *\code{WinDays} + 1 days
  ## shifted by \code{DayStep} days.
  ##
  ## For some of the windows, it maybe not be possible to estimate Rref.
  ## These missing values are filled by linear
  ## interpolation by function \code{\link{fInterpolateGaps}}.
  # Check if specified columns are numeric
  SubCallFunc <- paste(CallFunction, 'sRegrRref', sep = ':::')
  sDT <- cbind(.self$sDATA, .self$sTEMP)
  fCheckColNames(sDT, c(NightFluxVar, TempVar, E0Var), SubCallFunc)
  fCheckColNum(sDT, c(NightFluxVar, TempVar, E0Var), SubCallFunc)
  #
  # Regression settings
  LMRes.F <- data.frame(NULL) #Results of linear regression
  MinData.n <- 2 # Minimum number of data points for regression
  #
  # Write into vectors since cbind of data frames is so slow (factor of ~20 (!))
  NightFlux <- sDT[[NightFluxVar]]
  Temp <- sDT[[TempVar]]
  #
  # Loop regression periods
  DayCounter <- c(1:sINFO$DIMS) %/% sINFO$DTS
  CountRegr.i <- 0
  for (DayMiddle.i in seq(WinDays + 1, max(DayCounter), DayStep)) {
    #TEST: DayMiddle.i <- 8
    DayStart.i <- DayMiddle.i - WinDays
    DayEnd.i <- DayMiddle.i + WinDays
    #! Window size of 4 days corresponds to a full window length of 9 days,
    #non-congruent with PV-Wave code of 8 days, in paper not mentioned
    #! New code: Last window has minimum of window size
    #
    Subset.b <- DayCounter >= DayStart.i &
      DayCounter <= DayEnd.i &
      !is.na(NightFlux) &
      !is.na(Temp)
    MeanHour.i <- round(mean(which(Subset.b))) # Weighted middle of the time period
    NEEnight <- subset(NightFlux, Subset.b)
    TempSub <- subset(Temp, Subset.b)
    TempKelvin <- fConvertCtoK(TempSub)
    E0Sub <- subset(.self$sTEMP[[E0Var]], Subset.b) # (Constant value)
    #
    if (length(NEEnight) > MinData.n) {
      CountRegr.i <- CountRegr.i + 1
      tryCatch({
        LM.L <- lm(R_eco ~ 0 + fLloydTaylor(
          R_ref, E_0, Temp_degK, TRef = TRef)
          , data = data.frame(
            R_eco = NEEnight, R_ref = 1, E_0 = E0Sub
            , Temp_degK = TempKelvin))
        LMRes.F <- rbind(LMRes.F, cbind(
          Start = DayStart.i, End = DayEnd.i, Num = length(NEEnight)
          , MeanH = MeanHour.i
          , R_ref = coef(summary(LM.L))[1], R_ref_SD = coef(summary(LM.L))[2]))
        #! Note on PV-Wave code: trimmed linear regression not used in the end
        #, i.e. in online webtool
        #
        tmp.f <- function(x) {
          # Plot for testing, twutz: transformed debug code into function because
          # check was complaining of undefined x
          plot(NEEnight ~ fLloydTaylor(
            1, E0Sub, TempKelvin, TRef = TRef))
          curve(coef(LM.L)[1] * x, add = T, col = 'green')
        }
      }, error = function(e) {
        LMRes.F <- rbind(LMRes.F, cbind(
          Start = DayStart.i, End = DayEnd.i, Num = length(NEEnight)
          , MeanH = MeanHour.i, R_ref = NA, R_ref_SD = NA))
      }   )  #Spaces between brackets required to avoid replacement on docu gen
    }
  }
  #
  # Check for validity of regression results
  LMRes.F$R_ref_ok <- ifelse(LMRes.F$R_ref < 0 , NA, LMRes.F$R_ref)
  #! New code: Omit regressions with R_ref <0, in PV-Wave smaller values are
  #set to 0.000001, not mentioned in paper
  #TODO later: Flag for long distances between R_refs, especially if long
  #distance in the beginning
  #TODO later: Provide some kind of uncertainty estimate from R_ref_SD
  #
  # Interpolate R_ref periods linearly between MeanHour.i and keep constant
  # at beginning / end
  Rref <- rep(NA, length(NightFlux))
  Rref[LMRes.F$MeanH] <- LMRes.F$R_ref_ok
  Rref <- fInterpolateGaps(Rref)
  attr(Rref, 'varnames') <- 'R_ref'
  attr(Rref, 'units') <- attr(sDT[[NightFluxVar]], 'units')
  #
  message(
    'Regression of reference temperature R_ref for '
    , sum(!is.na(LMRes.F$R_ref_ok)), ' periods.')
  Rref
  ##value<<
  ## Data vector (length number of windows) with reference
  ## respiration (R_ref, flux units)
}
sEddyProc$methods(sRegrRref = sEddyProc_sRegrRref)


