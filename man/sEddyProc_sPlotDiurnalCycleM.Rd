\name{sEddyProc_sPlotDiurnalCycleM}
\alias{sEddyProc_sPlotDiurnalCycleM}
\title{sEddyProc sPlotDiurnalCycleM}
\description{The diurnal cycles of a single month are plotted to the current device,
scaled to all data. Each year is plotted as a different (coloured) line.}
\usage{sEddyProc_sPlotDiurnalCycleM(Var = Var.s, 
    QFVar = if (!missing(QFVar.s)) QFVar.s else "none", 
    QFValue = if (!missing(QFValue.n)) QFValue.n else NA_real_, 
    Month = Month.i, Legend = if (!missing(Legend.b)) Legend.b else T, 
    data = cbind(sDATA, sTEMP), dts = sINFO$DTS, 
    Var.s, QFVar.s = "none", QFValue.n = NA_real_, 
    Month.i, Legend.b = T)}
\arguments{
  \item{Var}{Variable to plot}
  \item{QFVar}{Quality
flag of variable to be filled}
  \item{QFValue}{Value
of quality flag for data to plot}
  \item{Month}{Month to plot}
  \item{Legend}{Plot
with legend}
  \item{data}{data.frame with variables to plot}
  \item{dts}{numeric integer}
  \item{Var.s}{Variable to plot}
  \item{QFVar.s}{Quality flag of variable to be filled}
  \item{QFValue.n}{Value of quality flag for data to plot}
  \item{Month.i}{Month to plot}
  \item{Legend.b}{Plot with legend}
}



\author{AMM, KS
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}





