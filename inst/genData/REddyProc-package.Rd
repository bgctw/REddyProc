\name{REddyProc-package}
\alias{REddyProc-package}\alias{REddyProc}
\title{EddyCovariance data processing and plotting utilities}
\description{EddyCovariance data processing and plotting utilities}
\details{Work of the package can be best seen by an \link{sEddyProc.example}.First the package helps to prepare your data for analysis \itemize{\item{ Loading text files into dataframes: \code{\link{fLoadTXTIntoDataframe}}  }\item{ Preparing a proper time field: \code{\link{fConvertTimeToPosix}}  }\item{ Initalize R5 reference class for the dataset: \code{\link{sEddyProc.new}}  }}%itemizeThe function are are assembled in class EddyProc (see \code{\link{sEddyProc.new}})Plotting\itemize{\item{ Fingerprints: \code{\link{sPlotFingerprint}}  }\item{ half-hourly fluxes and their daily means: \code{\link{sPlotHHFluxes}}  }\item{ daily sums: \code{\link{sPlotDailySums}}  }\item{ half-hourly means: \code{\link{sPlotHHMeans}}  }}%itemizeProcessing\itemize{\item{ Gapfilling: \code{\link{sMDSGapFill}}  }}%itemize}%details\author{Antje Moffat, Thomas Wutzler, Kerstin Sickel}
\keyword{ package }


