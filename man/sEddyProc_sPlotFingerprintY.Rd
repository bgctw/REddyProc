\name{sEddyProc_sPlotFingerprintY}
\alias{sEddyProc_sPlotFingerprintY}
\title{sEddyProc$sPlotFingerprintY - Plot fingerprint of specified year}
\description{The fingerprint for a single year is plotted to the current device,
scaled to all data.}
\usage{sEddyProc_sPlotFingerprintY(Var.s, QFVar.s = "none", 
    QFValue.n = NA_real_, Year.i, Legend.b = F, 
    Col.V = colorRampPalette(c("#00007F", 
        "blue", "#007FFF", "cyan", "#7FFF7F", 
        "yellow", "#FF7F00", "red", "#7F0000"))(50), 
    valueLimits = range(Plot.V.n, na.rm = TRUE))}
\arguments{
  \item{Var.s}{Variable to plot}
  \item{QFVar.s}{Quality flag of variable to be filled}
  \item{QFValue.n}{Value of quality flag for data to plot}
  \item{Year.i}{Year to plot}
  \item{Legend.b}{Plot only legend}
  \item{Col.V}{Color palette for fingerprint plot
(can be also defined by user), i.e. color scale argument to \code{\link{image}}}
  \item{valueLimits}{values outside this range
will be set to the range borders to avoid distorting colour scale
e.g. valueLimits = quantile(EddyProc.C$sDATA$NEE, prob = c(0.05, 0.95), na.rm = TRUE)}
}



\author{AMM, KS, TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





