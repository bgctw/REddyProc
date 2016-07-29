\name{sPlotDailySums}
\alias{sPlotDailySums}
\title{sEddyProc$sPlotDailySums - Image with daily sums of each year}
\description{Generates image in specified format ('pdf' or 'png') with daily sums, see also \code{\link{sPlotDailySumsY}}.}
\usage{sPlotDailySums(Var.s, VarUnc.s = "none", Format.s = "pdf", 
    Dir.s = "plots", unit.s = "gC/m2/day", ...)}
\arguments{
  \item{Var.s}{(Filled) variable to plot}
  \item{VarUnc.s}{Uncertainty estimates for variable}
  \item{Format.s}{Graphics file format ('pdf' or 'png')}
  \item{Dir.s}{Directory for plotting}
  \item{unit.s}{unit of the daily sums}
  \item{\dots}{further arguments to \code{\link{sPlotDailySumsY}}, such as \code{timeFactor.n} and \code{massFactor.n}.}
}



\author{KS, AMM  
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





