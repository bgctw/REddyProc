\name{sEddyProc_sPlotFingerprint}
\alias{sEddyProc_sPlotFingerprint}
\alias{sPlotFingerprint}
\title{sEddyProc sPlotFingerprint}
\description{Generates fingerprint in file}
\usage{sEddyProc_sPlotFingerprint(Var = Var.s, QFVar = "none", 
    QFValue = if (!missing(QFValue.n)) QFValue.n else NA_real_, 
    Format = if (!missing(Format.s)) Format.s else "pdf", 
    Dir = if (!missing(Dir.s)) Dir.s else "plots", 
    ..., Var.s, QFVar.s = "none", QFValue.n = NA_real_, 
    Format.s = "pdf", Dir.s = "plots")}
\arguments{
  \item{Var}{Variable to plot}
  \item{QFVar}{Quality flag of variable to be filled}
  \item{QFValue}{Value of
quality flag for data to plot}
  \item{Format}{
}
  \item{Dir}{Directory
for plotting}
  \item{\dots}{further arguments to \code{\link{sEddyProc_sPlotFingerprintY}}}
  \item{Var.s}{Variable to plot}
  \item{QFVar.s}{Quality flag of variable to be filled}
  \item{QFValue.n}{Value of quality flag for data to plot}
  \item{Format.s}{Graphics file format (e.g. 'pdf', 'png')}
  \item{Dir.s}{Directory for plotting}
}



\author{KS, AMM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





