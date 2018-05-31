\name{sEddyProc_sPlotDailySums}
\alias{sEddyProc_sPlotDailySums}
\title{sEddyProc$sPlotDailySums - Image with daily sums of each year}
\description{Generates image in specified format ('pdf' or 'png') with daily sums, see also \code{\link{sEddyProc_sPlotDailySumsY}}.}
\usage{sEddyProc_sPlotDailySums(Var.s, VarUnc.s = "none", 
    Format.s = "pdf", Dir.s = "plots", unit.s = "gC/m2/day", 
    ...)}
\arguments{
  \item{Var.s}{(Filled) variable to plot}
  \item{VarUnc.s}{Uncertainty estimates for variable}
  \item{Format.s}{Graphics file format ('pdf' or 'png')}
  \item{Dir.s}{Directory for plotting}
  \item{unit.s}{unit of the daily sums}
  \item{\dots}{further arguments to \code{\link{sEddyProc_sPlotDailySumsY}}, such as \code{timeFactor.n} and \code{massFactor.n}.}
}



\author{KS, AMM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





