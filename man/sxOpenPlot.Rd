\name{sxOpenPlot}
\alias{sxOpenPlot}
\title{sEddyProc - Internal function}
\description{Open graphics device.}
\usage{sxOpenPlot(Var.s, QFvar.s, QFvalue.n, PlotType.s, WInch.n, HInch.n, 
    Format.s, Dir.s, CallFunction.s = "")}
\arguments{
  \item{Var.s}{Variable to plot}
  \item{QFvar.s}{Quality flag of variable to be filled}
  \item{QFvalue.n}{Value of quality flag for data to plot}
  \item{PlotType.s}{Internal plot type}
  \item{WInch.n}{Width in inch}
  \item{HInch.n}{Height in inch}
  \item{Format.s}{Graphics format ('png' or 'pdf')}
  \item{Dir.s}{Directory for plotting}
  \item{CallFunction.s}{Name of function called from}
}

\value{Name of opened graphics device}

\author{KS, AMM
(Antje M. Moffat, Kerstin Sickel, Thomas Wutzler)}





