\name{REddyProc-package}
\alias{REddyProc-package}
\alias{REddyProc}
\title{Data processing and plotting utilities of (half-)hourly eddy-covariance measurements}
\description{
R eddy processing package with MDS gap filling algorithm, adopted after PV-Wave source code from Markus Reichstein.

The online tool: \url{http://www.bgc-jena.mpg.de/bgi/index.php/Services/REddyProcWeb} is based on this package.
}
\details{

In the following is a first overview of the REddyProc functions.
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
\item{ Gap filling: \code{\link{sMDSGapFill}}  }
\item{ Flux partitioning: \emph{in preparation} }
\item{ Ustar filtering: \emph{in preparation} }
}%itemize

Before or after processing, the data can be plotted:
\itemize{
\item{ Fingerprints: \code{\link{sPlotFingerprint}} }
\item{ Half-hourly fluxes and their daily means: \code{\link{sPlotHHFluxes}} }
\item{ Daily sums (and their uncertainties): \code{\link{sPlotDailySums}} }
\item{ Diurnal cycle: \code{\link{sPlotDiurnalCycle}} }
}%itemize

A \bold{complete list} of REddyProc functions be viewed by clicking on the \bold{Index} link at the bottom of this help page.

}%details


\author{ Department for Biogeochemical Integration at MPI-BGC, Jena, Germany }

\references{ Reichstein M, Falge E, Baldocchi D et al. (2005) On the separation of net ecosystem exchange 
into assimilation and ecosystem respiration: review and improved algorithm. Global Change Biology, 11, 1424-1439. }

\examples{
See \code{\link{sEddyProc.example}}.
}

\keyword{ package }
