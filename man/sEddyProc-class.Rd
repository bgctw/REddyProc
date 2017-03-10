\name{sEddyProc-class}
\Rdversion{1.1}
\docType{class}
\alias{sEddyProc-class}
\alias{sEddyProc}

\title{Class \code{"sEddyProc"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("sEddyProc")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{sID}:}{Object of class \code{character} ~~ }
    \item{\code{sDATA}:}{Object of class \code{data.frame} ~~ }
    \item{\code{sINFO}:}{Object of class \code{list} ~~ }
    \item{\code{sLOCATION}:}{Object of class \code{list} ~~ }
    \item{\code{sTEMP}:}{Object of class \code{data.frame} ~~ }
    \item{\code{sUSTAR}:}{Object of class \code{list} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{sEstUstarThresholdDistribution(ctrlUstarEst.l, ctrlUstarSub.l, UstarColName, NEEColName, TempColName, RgColName, ..., seasonFactor.v, seasonFactorsYear, nSample, probs, verbose.b)}:}{ ~~ }
    \item{\code{sEstUstarThreshold(UstarColName, NEEColName, TempColName, RgColName, ...)}:}{ ~~ }
    \item{\code{sPlotNEEVersusUStarForSeason(season.s, Format.s, Dir.s, UstarColName, NEEColName, TempColName)}:}{ ~~ }
    \item{\code{sPlotDailySums(Var.s, VarUnc.s, Format.s, Dir.s, unit.s, ...)}:}{ ~~ }
    \item{\code{sPlotDailySumsY(Var.s, VarUnc.s, Year.i, timeFactor.n, massFactor.n, unit.s)}:}{ ~~ }
    \item{\code{sPlotHHFluxes(Var.s, QFVar.s, QFValue.n, Format.s, Dir.s)}:}{ ~~ }
    \item{\code{sPlotHHFluxesY(Var.s, QFVar.s, QFValue.n, Year.i)}:}{ ~~ }
    \item{\code{sPlotDiurnalCycle(Var.s, QFVar.s, QFValue.n, Format.s, Dir.s)}:}{ ~~ }
    \item{\code{sPlotFingerprint(Var.s, QFVar.s, QFValue.n, Format.s, Dir.s, ...)}:}{ ~~ }
    \item{\code{sPlotFingerprintY(Var.s, QFVar.s, QFValue.n, Year.i, Legend.b, Col.V, valueLimits)}:}{ ~~ }
    \item{\code{sRegrRref(NightFlux.s, TempVar.s, E_0.s, T_ref.n, WinDays.i, DayStep.i, CallFunction.s)}:}{ ~~ }
    \item{\code{sRegrE0fromShortTerm(NightFlux.s, TempVar.s, ..., CallFunction.s, debug.l)}:}{ ~~ }
    \item{\code{sCalcPotRadiation(useSolartime.b)}:}{ ~~ }
    \item{\code{sMRFluxPartition(FluxVar.s, QFFluxVar.s, QFFluxValue.n, TempVar.s, QFTempVar.s, QFTempValue.n, RadVar.s, Lat_deg.n, Long_deg.n, TimeZone_h.n, T_ref.n, Suffix.s, debug.l, parsE0Regression)}:}{ ~~ }
    \item{\code{sGLFluxPartition(..., debug.l, isWarnReplaceColumns)}:}{ ~~ }
    \item{\code{sMDSGapFillAfterUStarDistr(..., UstarThres.df, UstarSuffix.V.s)}:}{ ~~ }
    \item{\code{sMDSGapFillAfterUstar(FluxVar.s, UstarVar.s, UstarThres.df, UstarSuffix.s, FlagEntryAfterLowTurbulence.b, isFilterDayTime, swThr, RgColName, ...)}:}{ ~~ }
    \item{\code{sMDSGapFill(Var.s, QFVar.s, QFValue.n, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n, FillAll.b, Verbose.b, Suffix.s)}:}{ ~~ }
    \item{\code{sFillMDC(WinDays.i, Verbose.b)}:}{ ~~ }
    \item{\code{sFillLUT(WinDays.i, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n, V4.s, T4.n, V5.s, T5.n, Verbose.b)}:}{ ~~ }
    \item{\code{sFillInit(Var.s, QFVar.s, QFValue.n, FillAll.b)}:}{ ~~ }
    \item{\code{sPrintFrames(NumRows.i)}:}{ ~~ }
    \item{\code{sExportResults()}:}{ ~~ }
    \item{\code{sExportData()}:}{ ~~ }
    \item{\code{sGetData()}:}{ ~~ }
    \item{\code{sSetLocationInfo(Lat_deg.n, Long_deg.n, TimeZone_h.n)}:}{ ~~ }
    \item{\code{initialize(ID.s, Data.F, ColNames.V.s, ColPOSIXTime.s, DTS.n, ColNamesNonNumeric.V.s, Lat_deg.n, Long_deg.n, TimeZone_h.n, ...)}:}{ ~~ }
  }
}
