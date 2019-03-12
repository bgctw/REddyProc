\name{REddyProc-package}
\alias{REddyProc-package}
\alias{REddyProc}
\title{Post Processing of (Half-)Hourly Eddy-Covariance Measurements}
\description{
  Standard and extensible Eddy-Covariance data post-processing including
  uStar-filtering, gap-filling, and flux-partitioning
  (Wutzler et al. (2018) <doi:10.5194/bg-15-5015-2018>).

  The Eddy-Covariance (EC)  micrometeorological technique quantifies continuous
  exchange fluxes of gases, energy, and momentum between an ecosystem and the atmosphere.
  It is important for understanding ecosystem dynamics and upscaling exchange fluxes.
  (Aubinet et al. (2012) <doi:10.1007/978-94-007-2351-1>).

  This package inputs pre-processed (half-)hourly data and supports further processing.
  First, a quality-check and filtering is performed based on the relationship between
  measured flux and friction
  velocity (uStar) to discard biased data
  (Papale et al. (2006) <doi:10.5194/bg-3-571-2006>).

  Second, gaps in the data are filled based on information from environmental conditions
  (Reichstein et al. (2005) <doi:10.1111/j.1365-2486.2005.001002.x>).

  Third, the net flux of carbon dioxide is partitioned
  into its gross fluxes in and out of the ecosystem by night-time
  based and day-time based approaches
  (Lasslop et al. (2010) <doi:10.1111/j.1365-2486.2009.02041.x>).

A general description and an online tool based on this package can be found
here: \url{https://www.bgc-jena.mpg.de/bgi/index.php/Services/REddyProcWeb}.
}

\details{
A \bold{detailed example} of the processing can be found in the
\href{../doc/useCase.html}{useCase vignette}.

A first overview of the REddyProc functions:

These functions help with the preparation of your data for the analysis:
\itemize{
\item{ Loading text files into dataframes: \code{\link{fLoadTXTIntoDataframe}} }
\item{ Preparing a proper time stamp: \code{\link{fConvertTimeToPosix}} }
\item{ Calculating latent variables, e.g. VPD: \code{\link{fCalcVPDfromRHandTair}} }
}%itemize

Then the data can be processed with the \code{\link{sEddyProc-class}} R5 reference class:
\itemize{
\item{ Initializing the R5 reference class: \code{\link{sEddyProc_initialize}} }
\item{ Estimating the turbulence criterion, Ustar threshold, for omitting data
  from periods of low turbulence:
Functions \code{\link{sEddyProc_sEstUstarThreshold}} and
\code{\link{sEddyProc_sEstUstarThresholdDistribution}} }.
\item{ Gap filling: \code{\link{sEddyProc_sMDSGapFill}} and
\code{\link{sEddyProc_sMDSGapFillAfterUstar}}.
}
\item{ Flux partitioning based on Night-Time: \code{\link{sEddyProc_sMRFluxPartition}} }
\item{ Flux partitioning based on Day-Time: \code{\link{sEddyProc_sGLFluxPartition}} }
}%itemize

Processing across different scenarios of u* threshold estimate is supported by
\itemize{
\item{ Estimating the turbulence criterion, Ustar threshold, for omitting data
  from periods of low turbulence:
\code{\link{sEddyProc_sEstimateUstarScenarios}} and associated
\itemize{
\item query the thresholds to be used \code{\link{sEddyProc_sGetUstarScenarios}}
\item set the thresholds to be used \code{\link{sEddyProc_sSetUstarScenarios}}
\item query the estimated thresholds all different aggregation levels
\code{\link{sEddyProc_sGetEstimatedUstarThresholdDistribution}}
}} %itemize and %item
\item{ Gap-Filling:
\code{\link{sEddyProc_sMDSGapFillUStarScens}} }
\item{ Flux partitioning based on Night-Time (Reichstein 2005):
\code{\link{sEddyProc_sMRFluxPartitionUStarScens}} }
\item{ Flux partitioning based on Day-Time (Lasslop 2010):
\code{\link{sEddyProc_sGLFluxPartitionUStarScens}} }
\item{ Flux partitioning based on modified Day-Time (Keenan 2019):
\code{\link{sEddyProc_sTKFluxPartitionUStarScens}} }
}%itemize

Before or after processing, the data can be plotted:
\itemize{
\item{ Fingerprint: \code{\link{sEddyProc_sPlotFingerprint}} }
\item{ Half-hourly fluxes and their daily means: \code{\link{sEddyProc_sPlotHHFluxes}} }
\item{ Daily sums (and their uncertainties): \code{\link{sEddyProc_sPlotDailySums}} }
\item{ Diurnal cycle: \code{\link{sEddyProc_sPlotDiurnalCycle}} }
}%itemize

A \bold{complete list} of REddyProc functions be viewed by clicking on the
\bold{Index} link at the bottom of this help page.

Also have a look at the \href{../doc/index.html}{package vignettes}.
%get a list with \code{vignette(package = "REddyProc")},
%view with e.g. \href{../doc/DEGebExample.html}{\code{vignette("DEGebExample")}}.

}%details


\author{ Department for Biogeochemical Integration at MPI-BGC, Jena, Germany }

\references{ Reichstein M, Falge E, Baldocchi D et al. (2005)
On the separation of net ecosystem exchange
into assimilation and ecosystem respiration: review and improved algorithm.
Global Change Biology, 11, 1424-1439. }


\keyword{package}
