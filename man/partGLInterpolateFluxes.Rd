\name{partGLInterpolateFluxes}
\alias{partGLInterpolateFluxes}
\title{partGLInterpolateFluxes}
\description{Interpolate ecoystem respiration (Reco) and Gross primary production (GPP) and associated uncertainty from two neighboring parameter sets of Light response curves }
\usage{partGLInterpolateFluxes(Rg, VPD, Temp, resParms, controlGLPart = partGLControl(), 
    lrcFitter)}
\arguments{
  \item{Rg}{photosynthetic flux density [umol/m2/s] or Global Radiation}
  \item{VPD}{Vapor Pressure Deficit [hPa]}
  \item{Temp}{Temperature [degC]}
  \item{resParms}{data frame with results of \code{\link{partGLFitLRCWindows}} of fitting the light-response-curve for several windows}
  \item{controlGLPart}{further default parameters, see \code{\link{partGLControl}}}
  \item{lrcFitter}{R5 class instance responsible for fitting the light response curve}
}
\details{\code{resLRC$iFirstRecInCentralDay} must denote the row for which the LRC parameters are representative, 
here, the first record of the center day

Parameter estimates are reported for the first record of the window, or
if \code{isTRUE(controlGLPart.l$isAssociateParmsToMeanOfValids)} for the mean time of all valid records within the window}
\value{data.frame with nrow() rows and columns  GPP, Reco, varGPP, varReco, and dRecNextEstimate}

\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{link{partitionNEEGL}}}

