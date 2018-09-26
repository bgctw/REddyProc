\name{sEddyProc_sPlotDiurnalCycle}
\alias{sEddyProc_sPlotDiurnalCycle}
\title{sEddyProc sPlotDiurnalCycle}
\description{Generates image in specified format ('pdf' or 'png') with diurnal cycles.}
\usage{sEddyProc_sPlotDiurnalCycle(Var = Var.s, 
    QFVar = if (!missing(QFVar.s)) QFVar.s else "none", 
    QFValue = if (!missing(QFValue.n)) QFValue.n else NA_real_, 
    Format = if (!missing(Format.s)) Format.s else "pdf", 
    Dir = if (!missing(Dir.s)) Dir.s else "plots", 
    data = cbind(sDATA, sTEMP), dts = sINFO$DTS, 
    Var.s, QFVar.s, QFValue.n, Format.s, 
    Dir.s)}
\arguments{
  \item{Var}{Variable to plot}
  \item{QFVar}{Quality
flag of variable to be filled}
  \item{QFValue}{Value
of quality flag for data to plot}
  \item{Format}{Graphics
file format (e.g. 'pdf', 'png')}
  \item{Dir}{Directory for plotting}
  \item{data}{data.frame with variables to plot}
  \item{dts}{numeric integer}
  \item{Var.s}{deprecated}
  \item{QFVar.s}{deprecated}
  \item{QFValue.n}{deprecated}
  \item{Format.s}{deprecated}
  \item{Dir.s}{deprecated}
}



\author{KS, AMM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





