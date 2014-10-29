\name{sxOpenPlot}
\alias{sxOpenPlot}
\title{sEddyProc - Internal function}
\description{Open graphics device.}
\usage{sxOpenPlot(Var.s, QFVar.s, QFValue.n, PlotType.s, WInch.n, HInch.n, 
    Format.s, Dir.s, CallFunction.s = "", DotsPerInc.n = 72)}
\arguments{
  \item{Var.s}{Variable to plot}
  \item{QFVar.s}{Quality flag of variable to be filled}
  \item{QFValue.n}{Value of quality flag for data to plot}
  \item{PlotType.s}{Internal plot type}
  \item{WInch.n}{Width in inch}
  \item{HInch.n}{Height in inch}
  \item{Format.s}{Graphics format ('png' or 'pdf' or 'cairo-png')}
  \item{Dir.s}{Directory for plotting}
  \item{CallFunction.s}{Name of function called from}
  \item{DotsPerInc.n}{Number of dots per inch for converting width and height to pixels on png output}
}
\details{Not all formats are supported on all platforms. The \code{png} will not work on unix without X-system. However 
there might be cairo support be built into R, allowing to use the 'cairo-png' format.}
\value{Name of opened graphics device}

\author{AMM, KS, TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





