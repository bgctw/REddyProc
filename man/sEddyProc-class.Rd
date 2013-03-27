\name{sEddyProc-class}
\Rdversion{1.1}
\docType{class}
\alias{sEddyProc-class}

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
    \item{\code{sTEMP}:}{Object of class \code{data.frame} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{sPlotDailySums(Var.s, VarUnc.s, Format.s, Dir.s)}:}{ ~~ }
    \item{\code{sPlotDailySumsY(Var.s, VarUnc.s, Year.i)}:}{ ~~ }
    \item{\code{sPlotHHFluxes(Var.s, QFvar.s, QFvalue.n, Format.s, Dir.s)}:}{ ~~ }
    \item{\code{sPlotHHFluxesY(Var.s, QFvar.s, QFvalue.n, Year.i)}:}{ ~~ }
    \item{\code{sPlotHHMeans(Var.s, QFvar.s, QFvalue.n, Format.s, Dir.s)}:}{ ~~ }
    \item{\code{sPlotHHMeansM(Var.s, QFvar.s, QFvalue.n, Month.i)}:}{ ~~ }
    \item{\code{sPlotFingerprint(Var.s, QFvar.s, QFvalue.n, Format.s, Dir.s)}:}{ ~~ }
    \item{\code{sPlotFingerprintY(Var.s, QFvar.s, QFvalue.n, Year.i)}:}{ ~~ }
    \item{\code{sxClosePlot(PlotFile.s)}:}{ ~~ }
    \item{\code{sxOpenPlot(Var.s, QFvar.s, QFvalue.n, PlotType.s, WInch.n, HInch.n, Format.s, Dir.s, CallFunction.s)}:}{ ~~ }
    \item{\code{sxSetTitle(Var.s, QFvar.s, QFvalue.n, Name.s)}:}{ ~~ }
    \item{\code{sMDSGapFill(Var.s, QFvar.s, QFvalue.n, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n, V4.s, T4.n, V5.s, T5.n, Verbose.b)}:}{ ~~ }
    \item{\code{sFillMDC(WinDays.i, Verbose.b)}:}{ ~~ }
    \item{\code{sFillLUT(WinDays.i, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n, V4.s, T4.n, V5.s, T5.n, Verbose.b)}:}{ ~~ }
    \item{\code{sPrintData()}:}{ ~~ }
    \item{\code{sExportData()}:}{ ~~ }
    \item{\code{sGetData()}:}{ ~~ }
    \item{\code{initialize(ID.s, Data.F, ColNames.V.s, ColPOSIXTime.s, ...)}:}{ ~~ }
  }
}
