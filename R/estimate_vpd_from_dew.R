#' Estimate VPD from daily minimum temperature
#'
#' of the data in the class function using \code{\link{estimate_vpd_from_dew}}.
#'
#' @param ... further arguments to \code{\link{estimate_vpd_from_dew}}
#'
#' @return side effect of updated column VPDfromDew in class
#' @export
sEddyProc_sFillVPDFromDew <- function(...){
  VPDfromDew <- cbind(.self$sDATA, select(.self$sTEMP, !"sDateTime")) %>%
     mutate(DateTime = .data$sDateTime) %>%
     estimate_vpd_from_dew(...)
  .self$sTEMP$VPDfromDew <- VPDfromDew
  if (!("VPD_f" %in% names(.self$sTEMP))) {
    warning(
    "Expected column VPD_f to be present from previous MDS based gap-filling. ",
    "Now creating this column and replacing also short gaps by dew-based estimate.")
    .self$sTEMP$VPD_f <- .self$sTEMP$VPD
  }
  iNA <- which(is.na(.self$sTEMP$VPD_f))
  .self$sTEMP$VPD_f[iNA] <- VPDfromDew[iNA]
  invisible(.self)
}
sEddyProc$methods(sFillVPDFromDew =
                    sEddyProc_sFillVPDFromDew)


#' Estimate VPD from assuming dewpoint at daily minimum temperature
#'
#' VPD is required for daytime NEE flux partitioning. Hence, it is necessary to
#' estimate VPD also for long gaps in data.
#' With two assumptions, VPD can be estimated from temperature
#' 1). The change of water mass in air is negligible during the day.
#' VPD is the difference of actual vapour pressure to  saturation vapour pressure.
#' 2.) At morning minimum temperature, vapour pressure is at minimum
#' in many cases at saturation.
#' Hence  \deqn{VPD = Esat(Tair) - E \approx Esat(Tair) - Esat_{daymin} \approx
#' Esat(Tair) - Esat(Tair_{min})}
#'
#' Since sometimes Esat_daymin is lower than Esat(Tair_min)
#' the estimated VPDfromDew is underestimated. This function applies a
#' linear model of the existing VPD and estimated VPD to correct for this bias:
#' VPD ~ 0 + VPDfromDew * Tair_f * hourOfDay *  TminOftheDay * TRangeDay
#'
#' @param df data.frame with columns DateTime, VPD, Tair, and Tair_f
#' @param pNonMissing numeric scalar of the necessary fraction of finite
#'   VPD and Tair. If fraction is lower then a warning is thrown.
#'
#' @return numeric vector of length(nrow(data)) of estimated VPD
#' @export
estimate_vpd_from_dew <- function(df, pNonMissing = 0.1){
  required_cols <- c("DateTime","VPD","Tair","Tair_f")
  iMissing <- which(!(required_cols %in% names(df)))
  if (length(iMissing)) stop(
    "Expected columns ", paste(required_cols, collapse = ","), " to be present.",
    " But columns ", paste(required_cols[iMissing], collapse = ","), " were missing.")
  nFinite <- sum(is.finite(df$VPD) & is.finite(df$Tair))
  if (nFinite < 3) stop(
    "Need finite VPD and Tair to estimate correction, but got only ",
    nFinite," finite records.")
  if (nFinite/nrow(df) < pNonMissing) warning(
    "Correction of VPD estimate needs sufficient number of finite VPD and Tair, ",
    "but only a fraction of ", signif(nFinite/nrow(df),2), " was finite. ",
    "VPD estimates might be inaccurate."
  )
  df_f <- df %>%
    mutate(cumday = getCumDay(.data$DateTime)) %>%
    group_by(.data$cumday) %>%
    mutate(
      hourOfDay = getHalfHourOfDay(.data$DateTime)/2,
      TminOftheDay = suppressWarnings(min(.data$Tair_f[.data$hourOfDay < 10], na.rm = TRUE)),
      TmaxOftheDay = suppressWarnings(max(.data$Tair_f, na.rm = TRUE)),
      TRangeDay = .data$TmaxOftheDay - .data$TminOftheDay,
      EminDew = Esat.slope(.data$TminOftheDay)$Esat*10,
      VPDfromDew = Esat.slope(.data$Tair_f)$Esat*10 - .data$EminDew
    ) %>%
    ungroup()
  # VPDfromDew is computed using Tair_f, but for the correction only use
  # those cases where original Tair is finite to avoid confounding issues
  lm1 <- lm(VPD ~ 0 + VPDfromDew * Tair_f * hourOfDay *  TminOftheDay * TRangeDay,
            filter(df_f, is.finite(.data$Tair)))
  # create column in original data.frame (do not return the intermediate vars)
  # the order in df and df_f should not have changed with grouping/ungrouping
  VPDfromDew = pmax(0,predict(lm1, df_f))
}

getCumDay <- function(
  ### get the cumulative day from a single equidistant time series
  dateHalfHour  ##<< POSIXct vector, indicating the end a
  ## half-hour (00:30, 01:00, 01:30, ...)
){
  ##value<< cumulative day starting
  cumDay <- cumsum(c(FALSE,diff(as.POSIXlt(dateHalfHour - 15L*60L)$yday) != 0))
}
getHalfHourOfDay <- function(
  ### get the half-Hour within one day
  dateHalfHour  ##<< POSIXct vector, indicating the end
  ## a half-hour (00:30, 01:00, 01:30, ...)
){
  hour <- as.POSIXlt(dateHalfHour - 15L*60L)$hour
  min <- as.POSIXlt(dateHalfHour - 30L*60L)$min
  ##value<< 00:30 -> 1, 01:00 -> 2, ... , 23:30 -> 47, 00:00 -> 48
  hour*2L + min/30L + 1L
}
