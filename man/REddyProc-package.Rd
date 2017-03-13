\name{REddyProc-package}
\alias{REddyProc-package}
\alias{REddyProc}
\title{Data processing and plotting utilities of (half-)hourly eddy-covariance measurements}
\description{
R eddy processing package with MDS gap filling algorithm, adopted after PV-Wave source code from Markus Reichstein.

The official R-Forge site of this package is here: url{http://r-forge.r-project.org/projects/reddyproc/}.

A general description and an online tool based on this package can be found here: \url{http://www.bgc-jena.mpg.de/bgi/index.php/Services/REddyProcWeb}.
}
\details{
A first overview of the REddyProc functions:

A \bold{detailed example} of the processing can be found under \code{\link{sEddyProc.example}}. 

These functions help with the preparation of your data for the analysis: 
\itemize{
\item{ Loading text files into dataframes: \code{\link{fLoadTXTIntoDataframe}} }
\item{ Preparing a proper time stamp: \code{\link{fConvertTimeToPosix}} }
\item{ Calculating latent variables, e.g. VPD: \code{\link{fCalcVPDfromRHandTair}} }
}%itemize

Then the data can be processed in an R5 reference class framework:
\itemize{
\item{ Initalizing the R5 reference class: \code{\link{sEddyProc.new}} }
\item{ Estimating the turbulence criterion, Ustar threshold, for omitting data from periods of low turbulence: 
Functions \code{\link{sEstUstarThreshold}} and \code{\link{sEstUstarThresholdDistribution}} }.
\item{ Gap filling: \code{\link{sMDSGapFill}} and  \code{\link{sMDSGapFillAfterUstar}}. 
}
\item{ Flux partitioning based on Night-Time: \code{\link{sMRFluxPartition}} }
\item{ Flux partitioning based on Day-Time: \code{\link{sGLFluxPartition}} }
}%itemize

Before or after processing, the data can be plotted:
\itemize{
\item{ Fingerprint: \code{\link{sPlotFingerprint}} }
\item{ Half-hourly fluxes and their daily means: \code{\link{sPlotHHFluxes}} }
\item{ Daily sums (and their uncertainties): \code{\link{sPlotDailySums}} }
\item{ Diurnal cycle: \code{\link{sPlotDiurnalCycle}} }
}%itemize

A \bold{complete list} of REddyProc functions be viewed by clicking on the \bold{Index} link at the bottom of this help page.

Also have a look at the package vignettes: get a list with \code{vignette(package = "REddyProc")}, view with \code{vignette("DEGebExample")}. 


}%details


\author{ Department for Biogeochemical Integration at MPI-BGC, Jena, Germany }

\references{ Reichstein M, Falge E, Baldocchi D et al. (2005) On the separation of net ecosystem exchange 
into assimilation and ecosystem respiration: review and improved algorithm. Global Change Biology, 11, 1424-1439. }

\examples{
#+++ Detailed example code can be found here:
help('sEddyProc.example')
}

\keyword{ package }
