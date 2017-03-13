\name{partitionNEEGL}
\alias{partitionNEEGL}
\title{partitionNEEGL}
\description{Partitioning NEE fluxes into GP and Reco after daytime method of Lasslop et al. (2010)}
\usage{partitionNEEGL(ds, NEEVar.s = paste0("NEE", SuffixDash.s, 
    "_f"), QFNEEVar.s = paste0("NEE", SuffixDash.s, 
    "_fqc"), QFNEEValue.n = 0, NEESdVar.s = paste0("NEE", 
    SuffixDash.s, "_fsd"), TempVar.s = paste0("Tair_f"), 
    QFTempVar.s = paste0("Tair_fqc"), QFTempValue.n = 0, 
    VPDVar.s = paste0("VPD_f"), QFVPDVar.s = paste0("VPD_fqc"), 
    QFVPDValue.n = 0, RadVar.s = "Rg", PotRadVar.s = "PotRad_NEW", 
    Suffix.s = "", controlGLPart = partGLControl(), 
    isVerbose = TRUE, nRecInDay = 48L, lrcFitter = RectangularLRCFitter())}
\arguments{
  \item{ds}{dataset with all the specified input columns and full days in equidistant times}
  \item{NEEVar.s}{Variable of Net Ecosystem Exchange flux}
  \item{QFNEEVar.s}{Quality flag of variable}
  \item{QFNEEValue.n}{Value of quality flag for _good_ (original) data}
  \item{NEESdVar.s}{Variable of standard deviation of net ecosystem fluxes}
  \item{TempVar.s}{Filled air or soil temperature variable (degC)}
  \item{QFTempVar.s}{Quality flag of filled temperature variable}
  \item{QFTempValue.n}{Value of temperature quality flag for _good_ (original) data}
  \item{VPDVar.s}{Filled Vapor Pressure Deficit - VPD - (hPa)}
  \item{QFVPDVar.s}{Quality flag of filled VPD variable}
  \item{QFVPDValue.n}{Value of VPD quality flag for _good_ (original) data}
  \item{RadVar.s}{Unfilled (original) radiation variable}
  \item{PotRadVar.s}{Variable name of potential radiation (W/m2)}
  \item{Suffix.s}{string inserted into column names before identifier for NEE column defaults (see \code{\link{sMDSGapFillAfterUstar}}).}
  \item{controlGLPart}{further default parameters, see \code{\link{partGLControl}}}
  \item{isVerbose}{set to FALSE to suppress output messages}
  \item{nRecInDay}{number of records within one day (for half-hourly data its 48)}
  \item{lrcFitter}{R5 class instance responsible for fitting the light response curve}
}
\details{Daytime-based partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco)

The fit to the light-response-curve is done by default using the Rectangular hyperbolic function, as in Lasslop et al. (2010)
Alternative fittings can be used by providing the correspodning subclass of \code{LightResponseCurveFitter-class} to \code{lrcFitter} argument.
(see \code{\link{LightResponseCurveFitter_predictGPP}})}
\value{data.frame with columns
\itemize{
\item{Reco_DT_<suffix>}{predicted ecosystem respiraiton: mumol CO2/m2/second}
\item{GPP_DT_<suffix>}{predicted gross primary production mumol CO2/m2/second}
}
\item{FP_VARnight}{NEE filtered for nighttime records (others NA)}
\item{FP_VARday}{NEE filtered for daytime recores (others NA)}
\item{NEW_FP_Temp}{temperature after filtering for quality flag degree Celsius}
\item{NEW_FP_VPD}{vapour pressure deficit after filtering for quality flag, hPa}
\item{FP_RRef_Night}{basal respiration estimated from nighttime (W/m2)}
\item{FP_qc}{quality flag: 0: good parameter fit, 1: some parameters out of range, required refit, 2: next parameter estimate is more than two weeks away}
\item{FP_dRecPar}{records until or after closest record that has a parameter estimate associated
\item{<LRC>}{Furhter light response curve (LRC) parameters and their standard deviation depend on the used LRC
(e.g. for the non-rectangular LRCC see \code{\link{NonrectangularLRCFitter_getParameterNames}}). 
They are estimated for windows and are reported with the first record of the window}}}
\references{Lasslop G, Reichstein M, Papale D, et al. (2010) Separation of net ecosystem exchange into assimilation and respiration using 
a light response curve approach: critical issues and global evaluation. Global Change Biology, Volume 16, Issue 1, Pages 187208}
\author{MM, TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partGLFitLRCWindows}}

\code{\link{partGLInterpolateFluxes}}}

