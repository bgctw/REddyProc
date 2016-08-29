\name{partGLInterpolateFluxes}
\alias{partGLInterpolateFluxes}
\title{partGLInterpolateFluxes}
\description{Interpolate ecoystem respiraiton (Reco) and Gross primary production (GPP) and associated uncertainty from two neighboring parameter sets of Light respons curves }
\usage{partGLInterpolateFluxes(Rg, VPD, Temp, resLRC, controlGLPart.l = partGLControl())}
\arguments{
  \item{Rg}{photosynthetic flux density [umol/m2/s] or Global Radiation}
  \item{VPD}{Vapor Pressure Deficit [hPa]}
  \item{Temp}{Temperature [degC]}
  \item{resLRC}{data frame with results of \code{\link{partGLFitLRCWindows}} of fitting the light-response-curve for several windows}
  \item{controlGLPart.l}{further default parameters, see \code{\link{partGLControl}}}
}
\details{\code{resLRC$iFirstRecInCentralDay} must denote the row for which the LRC parameters are representative, 
here, the first record of the center day

Parameter estimates are reported for the first record of the window, or
if \code{isTRUE(controlGLPart.l$isAssociateParmsToMeanOfValids)} for the mean time of all valid records within the window}


\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{link{partitionNEEGL}}}

