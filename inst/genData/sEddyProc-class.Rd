\name{sEddyProc-class}
\Rdversion{1.1}
\docType{class}
\alias{sEddyProc-class}
%\alias{sEddyProc}

\title{Class \code{"sEddyProc"}}
\description{
R5 reference class for processing of site-level half-hourly eddy data
}
\section{Extends}{
All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.
}

\author{ AM, TW }

\note{
  for examples see \href{../doc/useCase.html}{useCase vignette}
}
\keyword{classes}
\section{Fields}{
  private, not to be accessed directly:
  \describe{
    \item{\code{sID}:}{Object of class \code{character} with Site ID }
    \item{\code{sDATA}:}{Object of class \code{data.frame}  with (fixed) site data }
    \item{\code{sINFO}:}{Object of class \code{list}  with site information }
    \item{\code{sLOCATION}:}{Object of class \code{list}  with site location information }
    \item{\code{sTEMP}:}{Object of class \code{data.frame} of (temporary) result data }
    \item{\code{sUSTAR}:}{Object of class \code{list} with results form uStar Threshold estimation }
  }
}
\section{Methods}{
  Setup, import and export
  \describe{
    \item{\code{\link{sEddyProc_initialize}(ID.s, Data.F, ColNames.V.s, ColPOSIXTime.s, DTS.n, ColNamesNonNumeric.V.s, Lat_deg.n, Long_deg.n, TimeZone_h.n, ...)}}{}
    \item{\code{\link{sEddyProc_sSetLocationInfo}(Lat_deg.n, Long_deg.n, TimeZone_h.n)}}{}
    \item{\code{\link{sEddyProc_sExportResults}(isListColumnsExported)}}{}
    \item{\code{\link{sEddyProc_sExportData}()}}{}
    \item{\code{\link{sEddyProc_sGetData}()}}{}
  }

  uStar threshold estimation
  \describe{
    \item{\code{\link{sEddyProc_sEstUstarThresholdDistribution}(ctrlUstarEst.l, ctrlUstarSub.l, UstarColName, NEEColName, TempColName, RgColName, ..., seasonFactor.v, seasonFactorsYear, nSample, probs, verbose.b)}}{}
    \item{\code{\link{sEddyProc_sEstUstarThreshold}(UstarColName, NEEColName, TempColName, RgColName, ...)}}{}
    \item{\code{\link{sEddyProc_sPlotNEEVersusUStarForSeason}(season.s, Format.s, Dir.s, UstarColName, NEEColName, TempColName, WInch, HInchSingle, ...)}}{}
  }

  Gapfilling
  \describe{
    \item{\code{\link{sEddyProc_sCalcPotRadiation}(useSolartime.b)}}{}
    \item{\code{\link{sEddyProc_sMDSGapFill}(Var.s, QFVar.s, QFValue.n, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n, FillAll.b, Verbose.b, Suffix.s)}}{}
    \item{\code{\link{sEddyProc_sMDSGapFillAfterUStarDistr}(..., UstarThres.df, UstarSuffix.V.s)}}{}
    \item{\code{\link{sEddyProc_sMDSGapFillAfterUstar}(FluxVar.s, UstarVar.s, UstarThres.df, UstarSuffix.s, FlagEntryAfterLowTurbulence.b, isFilterDayTime, swThr, RgColName, ...)}}{}
    \item{\code{\link{sEddyProc_sFillMDC}(WinDays.i, Verbose.b)}}{}
    \item{\code{\link{sEddyProc_sFillLUT}(WinDays.i, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n, V4.s, T4.n, V5.s, T5.n, Verbose.b)}}{}
    \item{\code{\link{sEddyProc_sFillInit}(Var.s, QFVar.s, QFValue.n, FillAll.b)}}{}
  }

  Flux partitioning
  \describe{
    \item{\code{\link{sEddyProc_sMRFluxPartition}(FluxVar.s, QFFluxVar.s, QFFluxValue.n, TempVar.s, QFTempVar.s, QFTempValue.n, RadVar.s, Lat_deg.n, Long_deg.n, TimeZone_h.n, T_ref.n, Suffix.s, debug.l, parsE0Regression)}}{}
    \item{\code{\link{sEddyProc_sGLFluxPartition}(..., debug.l, isWarnReplaceColumns)}}{}
  }

  Plotting
  \describe{
    \item{\code{\link{sEddyProc_sPlotDailySums}(Var.s, VarUnc.s, Format.s, Dir.s, unit.s, ...)}}{}
    \item{\code{\link{sEddyProc_sPlotDailySumsY}(Var.s, VarUnc.s, Year.i, timeFactor.n, massFactor.n, unit.s)}}{}
    \item{\code{\link{sEddyProc_sPlotHHFluxes}(Var.s, QFVar.s, QFValue.n, Format.s, Dir.s)}}{}
    \item{\code{\link{sEddyProc_sPlotHHFluxesY}(Var.s, QFVar.s, QFValue.n, Year.i)}}{}
    \item{\code{\link{sEddyProc_sPlotDiurnalCycle}(Var.s, QFVar.s, QFValue.n, Format.s, Dir.s)}}{}
    \item{\code{\link{sEddyProc_sPlotFingerprint}(Var.s, QFVar.s, QFValue.n, Format.s, Dir.s, ...)}}{}
    \item{\code{\link{sEddyProc_sPlotFingerprintY}(Var.s, QFVar.s, QFValue.n, Year.i, Legend.b, Col.V, valueLimits)}}{}
  }
}
