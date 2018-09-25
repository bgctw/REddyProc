\name{sEddyProc_sPlotFingerprintY}
\alias{sEddyProc_sPlotFingerprintY}
\title{sEddyProc sPlotFingerprintY}
\description{Plot fingerprint for a single year scaled to all data.}
\usage{sEddyProc_sPlotFingerprintY(Var = Var.s, 
    QFVar = "none", QFValue = if (!missing(QFValue.n)) QFValue.n else NA_real_, 
    Year = Year.i, onlyLegend = if (!missing(Legend.b)) Legend.b else F, 
    colors = if (!missing(Col.V)) Col.V else colorRampPalette(c("#00007F", 
        "blue", "#007FFF", "cyan", "#7FFF7F", 
        "yellow", "#FF7F00", "red", "#7F0000"))(50), 
    valueLimits = range(Plot.V.n, na.rm = TRUE), 
    data = cbind(sDATA, sTEMP), dts = sINFO$DTS, 
    Var.s, QFVar.s, QFValue.n, Year.i, Legend.b, 
    Col.V)}
\arguments{
  \item{Var}{Variable to plot}
  \item{QFVar}{
}
  \item{QFValue}{
}
  \item{Year}{Year to plot}
  \item{onlyLegend}{Plot
only legend}
  \item{colors}{Color palette
for fingerprint plot
(can be also defined by user), i.e. color scale argument to
\code{\link{image}}}
  \item{valueLimits}{values outside this range
will be set to the range borders to avoid distorting colour scale
e.g. \code{valueLimits = quantile(EddyProc.C$sDATA$NEE, prob = c(
0.05, 0.95), na.rm = TRUE)}}
  \item{data}{data.frame with variables to plot}
  \item{dts}{numeric integer of hours in day}
  \item{Var.s}{deprecated}
  \item{QFVar.s}{deprecated}
  \item{QFValue.n}{deprecated}
  \item{Year.i}{deprecated}
  \item{Legend.b}{deprecated}
  \item{Col.V}{deprecated}
}



\author{AMM, KS, TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





