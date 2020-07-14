\name{sEddyProc_sPlotDailySums}
\alias{sEddyProc_sPlotDailySums}
\title{sEddyProc$sPlotDailySums - Image with daily sums of each year}
\description{Generates image in specified format ('pdf' or 'png') with daily sums,
see also \code{\link{sEddyProc_sPlotDailySumsY}}.}
\usage{sEddyProc_sPlotDailySums(Var = Var.s, VarUnc = "none", 
    Format = if (!missing(Format.s)) Format.s else "pdf", 
    Dir = if (!missing(Dir.s)) Dir.s else "plots", 
    unit = if (!missing(unit.s)) unit.s else "gC/m2/day", 
    ..., Var.s, VarUnc.s, Format.s, Dir.s, 
    unit.s)}
\arguments{
  \item{Var}{(Filled) variable to plot}
  \item{VarUnc}{Uncertainty
estimates for variable}
  \item{Format}{Graphics
file format ('pdf' or 'png')}
  \item{Dir}{Directory
for plotting}
  \item{unit}{unit
of the daily sums}
  \item{\dots}{further arguments to \code{\link{sEddyProc_sPlotDailySumsY}},
such as \code{timeFactor} and \code{massFactor}.}
  \item{Var.s}{deprecated}
  \item{VarUnc.s}{deprecated}
  \item{Format.s}{deprecated}
  \item{Dir.s}{deprecated}
  \item{unit.s}{deprecated}
}



\author{KS, AMM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}





