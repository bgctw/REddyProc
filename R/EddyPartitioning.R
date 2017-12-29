#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc methods for flux partitioning +++
#+++ Flux partitionig algorithm, adapted after the PV-Wave code and paper by Markus Reichstein +++
#+++ Dependencies: Eddy.R, DataFunctions.R
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#TEST: FluxVar.s <- 'NEE_f'; QFFluxVar.s <- 'NEE_fqc'; QFFluxValue.n <- 0; TempVar.s <- 'Tair_f'; QFTempVar.s <- 'Tair_fqc'; QFTempValue.n <- 0
#TEST: RadVar.s <- 'Rg'; Lat_deg.n <- 51.0; Long_deg.n <- 13.6; TimeZone_h.n <- 1.0; CallFunction.s = 'test'
#TEST: NightFlux.s = 'FP_VARnight';  TempVar.s = 'FP_Temp_NEW'; WinDays.i = 7; DayStep.i = 5; TempRange.n = 5; NumE_0.n = 3; Trim.n = 5
#TEST: NightFlux.s = 'FP_VARnight';  TempVar.s = 'FP_Temp_NEW'; E_0.s = 'E_0_NEW'; WinDays.i = 4; DayStep.i = 4;
#TEST: sDATA <- EddyProc.C$sDATA; sTEMP <- EddyProc.C$sTEMP

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' @export
sEddyProc_sGLFluxPartition <- function(
	##title<<
	## sGLFluxPartition: Flux partitioning after Lasslop et al. (2010)
	##description<<
	## Daytime-based partitioning of measured net ecosystem fluxes into gross primary production (GPP)
	## and ecosystem respiration (Reco)
	...		##<< arguments to \code{\link{partitionNEEGL}} additional to the dataset \code{ds}
		## such as \code{Suffix.s}
	, debug.l = list(		     ##<< List with debugging control.
			##describe<<
			useLocaltime.b = FALSE	##<< if TRUE use local time zone instead of geo-solar time to compute potential radiation
			##end<<
		)
	, isWarnReplaceColumns = TRUE		##<< set to FALSE to avoid the warning on replacing output columns
) {
	##author<<
	## MM, TW
	##references<<
	## Lasslop G, Reichstein M, Papale D, et al. (2010) Separation of net ecosystem exchange into assimilation and respiration using
	## a light response curve approach: critical issues and global evaluation. Global Change Biology, Volume 16, Issue 1, Pages 187-208
	.self$sCalcPotRadiation(useSolartime.b = !isTRUE(debug.l$useLocaltime.b) )
	dsAns <- partitionNEEGL(cbind(.self$sDATA, .self$sTEMP), ...
			, nRecInDay = sINFO$DTS
					)
	iExisting <- na.omit(match(colnames(dsAns), colnames(.self$sTEMP)  ))
	if (length(iExisting) ) {
		if (isWarnReplaceColumns) warning("replacing existing output columns", paste(colnames(.self$sTEMP)[iExisting], collapse = ", "))
		sTEMP <<- .self$sTEMP[, -iExisting]
	}
	sTEMP <<- cbind(.self$sTEMP, dsAns)
	return(invisible(NULL))
	##value<<
	## Flux partitioning results are in sTEMP data frame of the class.
}
sEddyProc$methods(sGLFluxPartition = sEddyProc_sGLFluxPartition)

#' @export
sEddyProc_sMRFluxPartition <- function(
				##title<<
				## sEddyProc$sMRFluxPartition - Flux partitioning after Reichstein et al. (2005)
				##description<<
				## Nighttime-based partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco)
				FluxVar.s = 'NEE_f'      ##<< Variable name of column with original and filled net ecosystem fluxes (NEE)
				, QFFluxVar.s = 'NEE_fqc' ##<< Quality flag of NEE variable
				, QFFluxValue.n = 0       ##<< Value of quality flag for _good_ (original) data
				, TempVar.s = 'Tair_f'    ##<< Filled air- or soil temperature variable (degC)
				, QFTempVar.s = 'Tair_fqc'##<< Quality flag of filled temperature variable
				, QFTempValue.n = 0       ##<< Value of temperature quality flag for _good_ (original) data
				, RadVar.s = 'Rg'         ##<< Unfilled (original) radiation variable
				, T_ref.n = 273.15 + 15     ##<< Reference temperature in Kelvin (degK) used in \code{fLloydTaylor} for regressing Flux and Temperature
				, Suffix.s = ''		     ##<< String suffix needed for different processing setups on the same dataset (for explanations see below)
				, debug.l = list(		     ##<< List with debugging control (passed also to \code{sEddyProc_sRegrE0fromShortTerm}).
						##describe<<
						useLocaltime.b = FALSE	##<< see details on solar vs local time
				##end<<
				)
				, parsE0Regression = list() ##<< list with further parameters passed down to \code{sEddyProc_sRegrE0fromShortTerm} and \code{fRegrE0fromShortTerm}, such as \code{TempRange.n}
) {
		##author<<
		## AMM, TW
		##references<<
		## Reichstein M, Falge E, Baldocchi D et al. (2005) On the separation of net ecosystem exchange
		## into assimilation and ecosystem respiration: review and improved algorithm. Global Change Biology, 11, 1424-1439.
			'Partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco)'
			##details<< \describe{\item{Description of newly generated variables with partitioning results:}{
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

			##details<< \describe{\item{Background}{
			## This partitioning is based on the regression of nighttime respiration with temperature
			## using the Lloyd-Taylor-Function \code{\link{fLloydTaylor}}.
			## First the temperature sensitivity E_0 is estimated from short term data, see \code{sEddyProc_sRegrE0fromShortTerm}.
			## Next the reference temperature R_ref is estimated for successive periods throughout the whole dataset (see \code{sEddyProc_sRegrRref}).
			## These estimates are then used to calculate the respiration during daytime and nighttime and with this GPP.
			## Attention: Gap filling of the net ecosystem fluxes (NEE) and temperature measurements (Tair or Tsoil) is required
			## prior to the partitioning!
			## }}

			# If the default variable names are used and a Suffix.s is provided, then the suffix is added to the variable name (see also comment below)
			if (FluxVar.s == 'NEE_f' && QFFluxVar.s == 'NEE_fqc' && fCheckValString(Suffix.s) ) {
				FluxVar.s <- paste('NEE_', Suffix.s, '_f', sep = '')
				QFFluxVar.s <- paste('NEE_', Suffix.s, '_fqc', sep = '')
			}

			# Check if specified columns exist in sDATA or sTEMP and if numeric and plausible. Then apply quality flag
			# TODO: avoid repeated cbind
			# TODO: checking column names this does not need a full combined data.frame
			fCheckColNames(cbind(.self$sDATA, .self$sTEMP), c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sMRFluxPartition')
			fCheckColNum(cbind(.self$sDATA, .self$sTEMP), c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sMRFluxPartition')
			fCheckColPlausibility(cbind(.self$sDATA, .self$sTEMP), c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sMRFluxPartition')
			Var.V.n <- fSetQF(cbind(.self$sDATA, .self$sTEMP), FluxVar.s, QFFluxVar.s, QFFluxValue.n, 'sMRFluxPartition')

			message('Start flux partitioning for variable ', FluxVar.s, ' with temperature ', TempVar.s, '.')

			##details<< \describe{\item{Selection of daytime data based on solar time}{
			## The respiration-temperature regression is very
			## sensitive to the selection of night- and daytime data.
			## Nighttime is selected by a combined threshold of current solar radiation and potential radiation.
			## The current implementation calculates potential radiation based on exact solar time, based on latitude and longitude.
			## (see \code{\link{fCalcPotRadiation}})
			## Therefore it might differ from implementations that use local winter clock time instead.
			## }}
			# Calculate potential radiation
			#! New code: Local time and equation of time accounted for in potential radiation calculation
			.self$sCalcPotRadiation(useSolartime.b =!isTRUE(debug.l$useLocaltime.b) )

			# Filter night time values only
			#! Note: Rg <= 10 congruent with MR PV-Wave, in paper Rg <= 20
			# Should be unfilled (original) radiation variable, therefore dataframe set to sDATA only
			sTEMP$FP_VARnight <<- ifelse(sDATA[, RadVar.s] > 10 | .self$sTEMP$PotRad_NEW > 0, NA,  Var.V.n)
			attr(sTEMP$FP_VARnight, 'varnames') <<- paste(attr(Var.V.n, 'varnames'), '_night', sep = '')
			attr(sTEMP$FP_VARnight, 'units') <<- attr(Var.V.n, 'units')
			#! New code: Slightly different subset than PV-Wave due to time zone correction (avoids timezone offset between Rg and PotRad)

			# Apply quality flag for temperature
			sTEMP$FP_Temp_NEW <<- fSetQF(cbind(.self$sDATA, .self$sTEMP), TempVar.s, QFTempVar.s, QFTempValue.n, 'sMRFluxPartition')

			# Estimate E_0 and R_ref (results are saved in sTEMP)
			# twutz1508: changed to do.call in order to allow passing further parameters when calling sMRFluxPartition
			sTEMP$E_0_NEW <<- do.call(.self$sRegrE0fromShortTerm, c(
							list('FP_VARnight', 'FP_Temp_NEW', T_ref.n = T_ref.n, CallFunction.s = 'sMRFluxPartition', debug.l = debug.l)
							, parsE0Regression))
			if (sum(.self$sTEMP$E_0_NEW == -111) != 0)
				return(invisible(-111)) # Abort flux partitioning if regression of E_0 failed

			# Reanalyse R_ref with E_0 fixed
			sTEMP$R_ref_NEW <<- .self$sRegrRref('FP_VARnight', 'FP_Temp_NEW', 'E_0_NEW', T_ref.n = T_ref.n, CallFunction.s = 'sMRFluxPartition')

			# Calculate the ecosystem respiration Reco
			sTEMP$Reco_NEW <<- fLloydTaylor(.self$sTEMP$R_ref_NEW, .self$sTEMP$E_0_NEW, fConvertCtoK(cbind(.self$sDATA, .self$sTEMP)[, TempVar.s]), T_ref.n = T_ref.n)
			attr(sTEMP$Reco_NEW, 'varnames') <<- 'Reco'
			attr(sTEMP$Reco_NEW, 'units') <<- attr(Var.V.n, 'units')

			# Calculate the gross primary production GPP_f
			sTEMP$GPP_NEW_f <<- -cbind(.self$sDATA, .self$sTEMP)[, FluxVar.s] + .self$sTEMP$Reco_NEW
			sTEMP$GPP_NEW_fqc <<- cbind(.self$sDATA, .self$sTEMP)[, QFFluxVar.s]
			#! New code: MDS gap filling information are not copied from NEE_fmet and NEE_fwin to GPP_fmet and GPP_fwin
			#           (since not known within this pure partitioning function)
			attr(sTEMP$GPP_NEW_f, 'varnames') <<- 'GPP_f'
			attr(sTEMP$GPP_NEW_f, 'units') <<- attr(Var.V.n, 'units')

			##details<< \describe{\item{Different processing setups on the same dataset}{
			## Attention: When processing the same site data set with different setups for the gap filling or flux partitioning
			## (e.g. due to different ustar filters),
			## a string suffix is needed! This suffix is added to the result column names to distinguish the results of the different setups.
			## If a Suffix.s is provided and if the defaults for FluxVar.s and QFFluxVar.s are used, the Suffix.s will be added to their variable names
			## (e.g. 'NEE_f' will be renamed to 'NEE_WithUstar_f' and 'NEE_fqc' to 'NEE_WithUstar_fqc' for the Suffix.s = 'WithUstar').
			## Currently, this works only with defaults of FluxVar.s = 'NEE_f' and QFFluxVar.s = 'NEE_fqc'.
			## }}
			# Rename new columns generated during flux partitioning:
			# For nighttime NEE (FP_NEEnight or FP_NEEnight_Suffix)
			colnames(sTEMP) <<- gsub('_VARnight', paste('_NEEnight', (if (fCheckValString(Suffix.s)) '_' else ''), Suffix.s, sep = ''), colnames(.self$sTEMP))
			# For the results columns, the _NEW is dropped and the suffix added
			colnames(sTEMP) <<- gsub('_NEW', paste((if (fCheckValString(Suffix.s)) '_' else ''), Suffix.s, sep = ''), colnames(.self$sTEMP))
			# Check for duplicate columns (to detect if different processing setups were executed without different suffixes)
			if (length(names(iDupl <- which(table(colnames(.self$sTEMP)) > 1))) )  {
				warning(paste0('sMRFluxPartition::: Duplicated columns found! (',
								paste(names(iDupl), collapse = ", ")
								, ')  Deleting each first of duplicate columns.'
								, ' Please use different Suffix.s when processing different setups on the same dataset!'))
				for (cname in names(iDupl) ) sTEMP[cname] <<- NULL	# need to remove columns else some tests fail
			}

			return(invisible(NULL))
			##value<<
			## Flux partitioning results (see variables in details) in sTEMP data frame (with renamed columns).
			## On success, return value is NULL. On failure an integer scalar error code is returned:
			## -111 if regression of E_0 failed due to insufficient relationship in the data.
}
sEddyProc$methods(sMRFluxPartition = sEddyProc_sMRFluxPartition)


#' @export
sEddyProc_sCalcPotRadiation <- function(
	### compute potential radiation from position and time
	useSolartime.b = TRUE	##<< by default corrects hour (given in local winter time) for latitude to solar time
		##<< (where noon is exactly at 12:00). Set this to FALSE to directly use local winter time
) {
	# queriing $hour is timezone agnostic, Also works if sDateTime as GMT while actual time being in another time zone
	DoY.V.n <- as.POSIXlt(sDATA$sDateTime)$yday + 1L
	Hour.V.n <- as.POSIXlt(sDATA$sDateTime)$hour + as.POSIXlt(sDATA$sDateTime)$min / 60
	# Check that location info has been set
	if (!(is.finite(sLOCATION$Lat_deg.n) & is.finite(sLOCATION$Long_deg.n) & is.finite(sLOCATION$TimeZone_h.n)))
		stop("Need to set valid location information (sSetLocationInfo) before calling sCalcPotRadiation.")
	##value<< column PotRad_NEW in sTEMP
	sTEMP$PotRad_NEW <<- fCalcPotRadiation(DoY.V.n, Hour.V.n, sLOCATION$Lat_deg.n, sLOCATION$Long_deg.n, sLOCATION$TimeZone_h.n
			, useSolartime.b = useSolartime.b)
}
sEddyProc$methods(sCalcPotRadiation = sEddyProc_sCalcPotRadiation)


fOptimSingleE0 <- function(
  ##title<<
  ## Estimate temperature sensitivity E_0 using a Newton type optimization
  ##description<<
  ## Estimate temperature sensitivity E_0 of \code{\link{fLloydTaylor}} for a single time series
  ## using a Newton type optimization.
  NEEnight.V.n	  ##<< (Original) nighttime ecosystem carbon flux, i.e. respiration vector
  , Temp_degK.V.n	##<< (Original) air or soil temperature vector (degC)
  , T_ref.n #= 273.15 + 15  ##<< Reference temperature in Kelvin (degK) used in \code{fLloydTaylor} for regressing Flux and Temperature
  , Trim.n = 5       ##<< Percentile to trim residual (%)
  , recoverOnError = FALSE	##<< Set to TRUE to debug errors instead of catching them
  , algorithm = "default"  ##<< optimization algorithm used (see \code{\link{nls}})
)
  ##author<<
  ## AMM, TW
{
  # Original implementation by AMM
  res <- tryCatch({
    # Non-linear regression
    NLS.L <- nls(formula = R_eco ~ fLloydTaylor(R_ref, E_0, Temp, T_ref.n = T_ref.n), trace = FALSE,
                 data = as.data.frame(cbind(R_eco = NEEnight.V.n, Temp = Temp_degK.V.n)), start = list(R_ref = 2, E_0 = 200)
		 		, algorithm = algorithm
		 )
    # Remove points with residuals outside Trim.n quantiles
    Residuals.V.n <- resid(NLS.L)
    #Residuals.V.n <- fLloydTaylor(R_ref = coef(summary(NLS.L))['R_ref', 1], E_0 = coef(summary(NLS.L))['E_0', 1],
    #		Temp_degK.V.n, T_ref.n = T_ref.n) - NEEnight.V.n
    t.b <- Residuals.V.n >= quantile(Residuals.V.n, probs = c(Trim.n / 100)) & Residuals.V.n <= quantile(Residuals.V.n, probs = c(1-Trim.n / 100))
    # Trimmed non-linear regression
    NLS_trim.L <- nls(formula = R_eco ~ fLloydTaylor(R_ref, E_0, Temp, T_ref.n = T_ref.n), algorithm = 'default', trace = FALSE,
                      data = as.data.frame(cbind(R_eco = NEEnight.V.n[t.b], Temp = Temp_degK.V.n[t.b])), start = list(R_ref = 2, E_0 = 200))
    ##value<< Numeric vector with following components:
    res <- c(
      R_ref = coef(summary(NLS.L))['R_ref', 1]		##<< Estimate of respiration rate at reference temperature
      , R_ref_SD = coef(summary(NLS.L))['R_ref', 2] 	##<< Standard deviation of R_ref
      , E_0 = coef(summary(NLS.L))['E_0', 1]			##<< Estimate of temperature sensitivity ("activation energy") in Kelvin (degK) for untrimmed dataset
      , E_0_SD = coef(summary(NLS.L))['E_0', 2]		##<< Standard deviation of E_0
      , E_0_trim = coef(summary(NLS_trim.L))['E_0', 1]		##<< Estimate of temperature sensitivity ("activation energy") in Kelvin (degK) for trimmed dataset
      , E_0_trim_SD = coef(summary(NLS_trim.L))['E_0', 2]	##<< Standard deviation of E_0_trim
      ##end<<
    )
    # Note on other tested algorithms:
    # Standard require(stats) nls with PORT algorithm and lower and upper bounds
    # require(FME) for modFit and modCost, has PORT algorithm included (and other algorithms like MCMC)
    # require(robustbase) for ltsReg but only linear regression
    # require(nlme) for heteroscedastic and mixed NLR but no port algo with upper and lower bounds
    # require(nlstools) for bootstrapping with nlsBoot(nls...)
  }, error = function(e) {
    if (isTRUE(recoverOnError) ) recover()
    res <- c(R_ref = NA, R_ref_SD = NA, E_0 = NA, E_0_SD = NA, E_0_trim = NA, E_0_trim_SD = NA)
  }   ) #Spaces between brackets required to avoid replacement on documentation generation
  res
}

fOptimSingleE0_Lev <- function(
  ##title<<
  ## Estimate temperature sensitivity E_0 using Levenberg-Marquard optimization
  ##description<<
  ## Estimate temperature sensitivity E_0 of \code{\link{fLloydTaylor}} for a single time series
  ## using Levenberg-Marquard optimization.
  NEEnight.V.n	 	##<< (Original) nighttime ecosystem carbon flux, i.e. respiration vector
  , Temp_degK.V.n	##<< (Original) air or soil temperature vector (degC)
  , T_ref.n #= 273.15 + 15   ##<< Reference temperature in Kelvin (degK) used in \code{fLloydTaylor} for regressing Flux and Temperature
  , Trim.n = 5      	##<< Percentile to trim residual (%)
  , recoverOnError = FALSE	##<< Set to TRUE to debug errors instead of catching them
  , algorithm = 'LM' 	##<< optimization algorithm used (see nlsLM from package minpack.lm)
)
  ##author<<
  ## TW
{
  if (!requireNamespace('minpack.lm') ) stop("Need to install package minpack.lm before using LM optimization.")
  res <- tryCatch({
    # Non-linear regression
    NLS.L <- minpack.lm::nlsLM(formula = R_eco ~ fLloydTaylor(R_ref, E_0, Temp, T_ref.n = T_ref.n), trace = FALSE,
                   data = as.data.frame(cbind(R_eco = NEEnight.V.n, Temp = Temp_degK.V.n)), start = list(R_ref = 2, E_0 = 200)
                   , control = minpack.lm::nls.lm.control(maxiter = 20)
				   , algorithm = algorithm
    )
    # Remove points with residuals outside Trim.n quantiles
    Residuals.V.n <- resid(NLS.L)
    #plot(Residuals.V.n ~ Temp_degK.V.n)
    t.b <- Residuals.V.n >= quantile(Residuals.V.n, probs = c(Trim.n / 100)) & Residuals.V.n <= quantile(Residuals.V.n, probs = c(1-Trim.n / 100))
    #points(Residuals.V.n[!t.b] ~ Temp_degK.V.n[!t.b], col = "red")
    # Trimmed non-linear regression
    NLS_trim.L <- minpack.lm::nlsLM(formula = R_eco ~ fLloydTaylor(R_ref, E_0, Temp, T_ref.n = T_ref.n), algorithm = 'default', trace = FALSE,
                        data = as.data.frame(cbind(R_eco = NEEnight.V.n[t.b], Temp = Temp_degK.V.n[t.b]))
                        , start = coef(NLS.L)
                        #, start = list(R_ref = 2, E_0 = 200)
    )
    ##value<< Numeric vector with following components:
    res <- c(
      R_ref = coef(summary(NLS.L))['R_ref', 1]		##<< Estimate of espiration rate at reference temperature
      , R_ref_SD = coef(summary(NLS.L))['R_ref', 2] 	##<< Standard deviation of R_ref
      , E_0 = coef(summary(NLS.L))['E_0', 1]			##<< Estimate of temperature sensitivity ("activation energy") in Kelvin (degK) for untrimmed dataset
      , E_0_SD = coef(summary(NLS.L))['E_0', 2]		##<< Standard deviation of E_0
      , E_0_trim = coef(summary(NLS_trim.L))['E_0', 1]		##<< Estimate of temperature sensitivity ("activation energy") in Kelvin (degK) for trimmed dataset
      , E_0_trim_SD = coef(summary(NLS_trim.L))['E_0', 2]	##<< Standard deviation of E_0_trim
      ##end<<
    )
  }, error = function(e) {
    if (isTRUE(recoverOnError) ) recover()
    res <- c(R_ref = NA, R_ref_SD = NA, E_0 = NA, E_0_SD = NA, E_0_trim = NA, E_0_trim_SD = NA)
  }   ) #Spaces between brackets required to avoid replacement on documentation generation
  res
}

fRegrE0fromShortTerm = function(
  ##title<<
  ## Estimation of the temperature sensitivity E_0
  ##description<<
  ## Estimation of the temperature sensitivity E_0 from regression of \code{\link{fLloydTaylor}} for short periods
  NightFlux.V.n		    ##<< (Original) nighttime ecosystem carbon flux, i.e. respiration vector
  , TempVar.V.n        ##<< (Original) air or soil temperature vector (degC)
  , DayCounter.V.i 	  ##<< Integer specifying the day of each record
  , WinDays.i = 7        ##<< Window size for \code{\link{fLloydTaylor}} regression in days
  , DayStep.i = 5        ##<< Window step for \code{\link{fLloydTaylor}} regression in days
  , TempRange.n = 5      ##<< Threshold temperature range to start regression (#! Could be larger for Tair)
  , Trim.n = 5           ##<< Percentile to trim residual (%)
  , NumE_0.n = 3         ##<< Number of best E_0's to average over
  , MinE_0.n = 30  		  ##<< Minimum E0 for validity check
  , MaxE_0.n = 450	  	  ##<< Maximum E0 for validity check
  , T_ref.n	#= 273.15 + 15   ##<< Reference temperature in Kelvin (degK) used in \code{fLloydTaylor} for regressing Flux and Temperature
  , CallFunction.s = ''  ##<< Name of function called from
  , optimAlgorithm = 'default'   ##<< optimization algorithm used (see \code{\link{nls}}) or 'LM' for Levenberg-Marquard (see nlsLM from package minpack.lm)
)
##author<<
## AMM
{
  ##details<<
  ##The coefficient E0 is estimated for windows with a length of
  ##\code{WinDays.i} days, for successive periods in steps of \code{DayStep.i}
  ##days. Only those windows with a sufficient number or records and with a
  ##sufficient temperature range \code{TempRange.n} are used for the
  ##\code{\link{fLloydTaylor}} regression of E0 using the optimization
  ##\code{\link{fOptimSingleE0}}. Unreasonable estimates are discarded (95%
  ##confidence interval inside \code{MinE_0.n} and \code{MaxE_0.n}) and the
  ##others are ordered by their standard deviations. The mean across the best (=
  ##lowest standard deviation) E0 estimates is reported with \code{NumE_0.n}
  ##defining the number of best estimates to use.

  # Regression settings
  #NLSRes.F <- data.frame(NULL) #Results of non-linear regression
  #NLSRes_trim.F <- data.frame(NULL) #Results of non-linear regression
  MinData.n <- 6 # Minimum number of data points

  fOptim <- fOptimSingleE0
  if (optimAlgorithm == 'LM') fOptim <- fOptimSingleE0_Lev
  #tw: better use rbind with a list instead of costly repeated extending a data.frame
  NLSRes.F <- as.data.frame(do.call(rbind, NLSRes.l <- lapply(seq(WinDays.i + 1, max(DayCounter.V.i), DayStep.i) , function(DayMiddle.i) {
    #TEST: DayMiddle.i <- 8
    DayStart.i <- DayMiddle.i - WinDays.i
    DayEnd.i <- DayMiddle.i + WinDays.i
    #! Window size of 7 days corresponds to full window length of 15 days as in paper, non-congruent with PV-Wave code of 14 days
    #! New code: Last window has minimum width of WinDays.i

    Subset.b <- DayCounter.V.i >= DayStart.i & DayCounter.V.i <= DayEnd.i & !is.na(NightFlux.V.n) & !is.na(TempVar.V.n)
    NEEnight.V.n <- subset(NightFlux.V.n, Subset.b)
    Temp.V.n <- subset(TempVar.V.n, Subset.b)
    Temp_degK.V.n <- fConvertCtoK(Temp.V.n)

    if (length(NEEnight.V.n) > MinData.n && diff(range(Temp_degK.V.n)) >= TempRange.n) {
      #CountRegr.i <- CountRegr.i + 1
      resOptim <- fOptim(NEEnight.V.n, Temp_degK.V.n, algorithm = optimAlgorithm, T_ref.n = T_ref.n)
      NLSRes.F <- c(Start = DayStart.i, End = DayEnd.i, Num = length(NEEnight.V.n), TRange = diff(range(Temp_degK.V.n)),
                    resOptim)
    } else NULL
  }) ))
  Limits.b <- (NLSRes.F$E_0_trim - NLSRes.F$E_0_trim_SD > MinE_0.n & NLSRes.F$E_0_trim + NLSRes.F$E_0_trim_SD < MaxE_0.n)
  #! New code: Check validity with SD (standard deviation) limits, in PV-Wave without SD, in paper if E_0_SD < (E_0 * 50%)
  NLSRes.F$E_0_trim_ok <- ifelse(Limits.b, NLSRes.F$E_0_trim, NA)
  NLSRes.F$E_0_trim_SD_ok <- ifelse(Limits.b, NLSRes.F$E_0_trim_SD, NA)
  #
  # Sort data frame for smallest standard deviation
  NLSsort.F <- NLSRes.F[order(NLSRes.F$E_0_trim_SD_ok), ] # ordered data.frame
  ##details<<
  ## Take average of the three E_0 with lowest standard deviation
  E_0_trim.n <- round(mean(NLSsort.F$E_0_trim_ok[1:NumE_0.n]), digits = 2)
  #
  # Abort flux partitioning if regression of E_0 failed
  if (is.na(E_0_trim.n) ) {
	  # twutz 150226: just warning and returning negative value gives problems later on, maybe better stop and catch exception
	  warning(CallFunction.s, ':::fRegrE0fromShortTerm::: Less than ', NumE_0.n, ' valid values for E_0 after regressing ',
			  nrow(NLSRes.F), ' periods! Aborting partitioning.')
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
    ## Estimation of the temperature sensitivity E_0 from regression of \code{\link{fLloydTaylor}}
    ## for short periods by calling \code{fRegrE0fromShortTerm}
    NightFlux.s           ##<< Variable with (original) nighttime ecosystem carbon flux, i.e. respiration
    , TempVar.s            ##<< Variable with (original) air or soil temperature (degC)
    , ...				  ##<< Parameters passed to \code{fRegrE0fromShortTerm}
    , CallFunction.s = ''    ##<< Name of function called from
    , debug.l = list(fixedE0 = NA) ##<< List with controls for debugging, see details
  ) {
    ##author<<
    ## AMM, TW
    'Estimation of the temperature sensitivity E_0 from regression of fLloydTaylor() for short periods by calling fRegrE0fromShortTerm()'
    ##details<< For further details see \code{fRegrE0fromShortTerm}.
    #
    # Check if specified columns are numeric
    SubCallFunc.s <- paste(CallFunction.s, 'sRegrE0fromShortTerm', sep = ':::')
    fCheckColNames(cbind(.self$sDATA, .self$sTEMP), c(NightFlux.s, TempVar.s), SubCallFunc.s)
    fCheckColNum(cbind(.self$sDATA, .self$sTEMP), c(NightFlux.s, TempVar.s), SubCallFunc.s)
    #
    ##details<< \describe{ \item{Debugging control}{
    ## When supplying a finite scalar value \code{debug.l$fixedE0}, then this value
    ## is used instead of the temperature sensitivity E_0 from short term data.
    ## }}
    if ( (length(debug.l$fixedE0) != 0) && is.finite(debug.l$fixedE0) ) {
      E_0_trim.n <- debug.l$fixedE0[1]
      message('Using prescribed temperature sensitivity E_0 of: ', format(E_0_trim.n, digits = 5), '.')
    }else{
      # Write into vectors since cbind of data frames is so slow (factor of ~20 (!))
      NightFlux.V.n <- cbind(.self$sDATA, .self$sTEMP)[, NightFlux.s]
      TempVar.V.n <- cbind(.self$sDATA, .self$sTEMP)[, TempVar.s]
      DayCounter.V.i <- c(1:sINFO$DIMS) %/% sINFO$DTS
      #
      # Check for validity of E_0 regression results
      if (grepl('Tair', TempVar.s) ) {
        #Limits in PV-Wave code for Tair
        MinE_0.n <- 30; MaxE_0.n <- 350
      } else if (grepl('Tsoil', TempVar.s) ) {
        #Limits in PV-Wave code for Tsoil
        MinE_0.n <- 30; MaxE_0.n <- 550 # Higher values due to potentially high Q10 values
      } else {
        #Default limits taken from paper
        MinE_0.n <- 30; MaxE_0.n <- 450
      }
      #
      E_0_trim.n <- fRegrE0fromShortTerm(NightFlux.V.n, TempVar.V.n, DayCounter.V.i, ...,
                                          MinE_0.n = MinE_0.n, MaxE_0.n = MaxE_0.n, CallFunction.s = SubCallFunc.s)
      message('Estimate of the temperature sensitivity E_0 from short term data: ', format(E_0_trim.n, digits = 5), '.')
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
    ##title<<
    ## sEddyProc$sRegrRref - Estimation of the time series of reference respiration Rref
    ##description<<
    ## Estimation of the reference respiration Rref of \code{\link{fLloydTaylor}}
    ## for successive periods
    NightFlux.s           ##<< Variable with (original) nighttime ecosystem carbon flux, i.e. respiration
    , TempVar.s            ##<< Variable with (original) air or soil temperature (degC)
	, E_0.s                ##<< Temperature sensitivity E_0 estimated with \code{sEddyProc_sRegrE0fromShortTerm}
	, T_ref.n #= 273.15 + 15   ##<< Reference temperature in Kelvin (degK) used in \code{fLloydTaylor} for regressing Flux and Temperature
	, WinDays.i = 3          ##<< Window size for \code{\link{fLloydTaylor}} regression in days
    , DayStep.i = 4          ##<< Window step for \code{\link{fLloydTaylor}} regression in days
    , CallFunction.s = ''    ##<< Name of function called from
) {
  ##author<<
  ## AMM
  ##details<<
  ## While parameter E0 in the Temperature-Respiration relationship (\code{\link{fLloydTaylor}}) is kept konstant,
  ## parameter Rref is allowed to change with time.
  ## This method estimates Rref for a series of time windows of length 2 *\code{WinDays.i} + 1 days
  ## shifted by \code{DayStep.i} days.
  ##
  ## For some of the windows, it maybe not be possible to estimate Rref. These missing values are filled by linear
  ## interpolation by function \code{\link{fInterpolateGaps}}.
    'Estimation of the reference respiration Rref of fLloydTaylor() for successive periods'

    # Check if specified columns are numeric
    SubCallFunc.s <- paste(CallFunction.s, 'sRegrRref', sep = ':::')
    fCheckColNames(cbind(.self$sDATA, .self$sTEMP), c(NightFlux.s, TempVar.s, E_0.s), SubCallFunc.s)
    fCheckColNum(cbind(.self$sDATA, .self$sTEMP), c(NightFlux.s, TempVar.s, E_0.s), SubCallFunc.s)

    # Regression settings
    LMRes.F <- data.frame(NULL) #Results of linear regression
    MinData.n <- 2 # Minimum number of data points for regression

    # Write into vectors since cbind of data frames is so slow (factor of ~20 (!))
    NightFlux.V.n <- cbind(.self$sDATA, .self$sTEMP)[, NightFlux.s]
    TempVar.V.n <- cbind(.self$sDATA, .self$sTEMP)[, TempVar.s]

    # Loop regression periods
    DayCounter.V.i <- c(1:sINFO$DIMS) %/% sINFO$DTS
    CountRegr.i <- 0
    for (DayMiddle.i in seq(WinDays.i + 1, max(DayCounter.V.i), DayStep.i)) {
      #TEST: DayMiddle.i <- 8
      DayStart.i <- DayMiddle.i - WinDays.i
      DayEnd.i <- DayMiddle.i + WinDays.i
      #! Window size of 4 days corresponds to a full window length of 9 days, non-congruent with PV-Wave code of 8 days, in paper not mentioned
      #! New code: Last window has minimum of window size

      Subset.b <- DayCounter.V.i >= DayStart.i & DayCounter.V.i <= DayEnd.i & !is.na(NightFlux.V.n) & !is.na(TempVar.V.n)
      MeanHour.i <- round(mean(which(Subset.b))) # Weighted middle of the time period
      NEEnight.V.n <- subset(NightFlux.V.n, Subset.b)
      Temp.V.n <- subset(TempVar.V.n, Subset.b)
      Temp_degK.V.n <- fConvertCtoK(Temp.V.n)
      E_0.V.n <- subset(.self$sTEMP[, E_0.s], Subset.b) # (Constant value)

      if (length(NEEnight.V.n) > MinData.n) {
        CountRegr.i <- CountRegr.i + 1
        tryCatch({
          LM.L <- lm(R_eco ~ 0 + fLloydTaylor(R_ref, E_0, Temp_degK, T_ref.n = T_ref.n), data = as.data.frame(cbind(R_eco = NEEnight.V.n, R_ref = 1, E_0 = E_0.V.n, Temp_degK = Temp_degK.V.n)))
          LMRes.F <- rbind(LMRes.F, cbind(Start = DayStart.i, End = DayEnd.i, Num = length(NEEnight.V.n), MeanH = MeanHour.i,
                                          R_ref = coef(summary(LM.L))[1], R_ref_SD = coef(summary(LM.L))[2]))
          #! Note on PV-Wave code: trimmed linear regression not used in the end
          #, i.e. in online webtool

          tmp.f <- function(x) { # Plot for testing, twutz: transformed debug code into function because check was complaining of undefined x
            plot(NEEnight.V.n ~ fLloydTaylor(1, E_0.V.n, Temp_degK.V.n, T_ref.n = T_ref.n))
            curve(coef(LM.L)[1] * x, add = T, col = 'green')
          }
        }, error = function(e) {
          LMRes.F <- rbind(LMRes.F, cbind(Start = DayStart.i, End = DayEnd.i, Num = length(NEEnight.V.n), MeanH = MeanHour.i,
                                          R_ref = NA, R_ref_SD = NA))
        }   )  #Spaces between brackets required to avoid replacement on documentation generation
      }
    }

    # Check for validity of regression results
    LMRes.F$R_ref_ok <- ifelse(LMRes.F$R_ref < 0 , NA, LMRes.F$R_ref)
    #! New code: Omit regressions with R_ref <0, in PV-Wave smaller values are set to 0.000001, not mentioned in paper
    #TODO later: Flag for long distances between R_refs, especially if long distance in the beginning
    #TODO later: Provide some kind of uncertainty estimate from R_ref_SD

    # Interpolate R_ref periods linearly between MeanHour.i and keep constant at beginning / end
    Rref.V.n <- rep(NA, length(NightFlux.V.n))
    Rref.V.n[LMRes.F$MeanH] <- LMRes.F$R_ref_ok
    Rref.V.n <- fInterpolateGaps(Rref.V.n)
    attr(Rref.V.n, 'varnames') <- 'R_ref'
    attr(Rref.V.n, 'units') <- attr(.self$sTEMP[, NightFlux.s], 'units')

    message('Regression of reference temperature R_ref for ', sum(!is.na(LMRes.F$R_ref_ok)), ' periods.')

    Rref.V.n
    ##value<<
    ## Data vector (length number of windows) with reference respiration (R_ref, flux units)
}
sEddyProc$methods(sRegrRref = sEddyProc_sRegrRref)


