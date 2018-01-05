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
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





