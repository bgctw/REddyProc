\name{sEddyProc_sPlotHHFluxesY}
\alias{sEddyProc_sPlotHHFluxesY}
\title{sEddyProc sPlotHHFluxesY}
\description{Plot half-hourly fluxes for a single year scaled to all data.}
\usage{sEddyProc_sPlotHHFluxesY(Var = Var.s, QFVar = if (!missing(QFVar.s)) QFVar.s else "none", 
    QFValue = if (!missing(QFValue.n)) QFValue.n else NA_real_, 
    Year = Year.i, data = cbind(sDATA, sTEMP), 
    dts = sINFO$DTS, Var.s, QFVar.s, QFValue.n, 
    Year.i)}
\arguments{
  \item{Var}{Variable to plot}
  \item{QFVar}{
}
  \item{QFValue}{
}
  \item{Year}{Year to plot}
  \item{data}{data.frame with variables to plot}
  \item{dts}{numeric integer}
  \item{Var.s}{deprecated}
  \item{QFVar.s}{deprecated}
  \item{QFValue.n}{deprecated}
  \item{Year.i}{deprecated}
}



\author{AMM, KS
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





