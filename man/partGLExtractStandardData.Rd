\name{partGLExtractStandardData}
\alias{partGLExtractStandardData}
\title{partGLExtractStandardData}
\description{extract the relevant columns from original input to generate dataset with defined names }
\usage{partGLExtractStandardData(ds, NEEVar.s = paste0("NEE", 
    SuffixDash.s, "_f"), QFNEEVar.s = paste0("NEE", 
    SuffixDash.s, "_fqc"), QFNEEValue.n = 0, 
    NEESdVar.s = paste0("NEE", SuffixDash.s, 
        "_fsd"), TempVar.s = paste0("Tair_f"), 
    QFTempVar.s = paste0("Tair_fqc"), QFTempValue.n = 0, 
    VPDVar.s = paste0("VPD_f"), QFVPDVar.s = paste0("VPD_fqc"), 
    QFVPDValue.n = 0, RadVar.s = "Rg", PotRadVar.s = "PotRad_NEW", 
    Suffix.s = "", controlGLPart = partGLControl())}
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
}
\details{The LRC fit usually weights NEE records by its uncertainty. In order to also use
records with missing \code{NEESdVar.s}, uncertainty of the missing values is by default set 
to a conservatively high value, parameterized by \code{controlGLPart$replaceMissingSdNEEParms)}.
Controlled by argument \code{replaceMissingSdNEEParms} in \code{\link{partGLControl}}, but overruled
by argument \code{neglectNEEUncertaintyOnMissing}.}
\value{a data.frame with columns
\item{sDateTime}{first column of ds, usually the time stamp
not used, but usually first column is a dateTime is kept for aiding debug}
\item{NEE}{NEE filtered for quality flay}
\item{sdNEE}{standard deviation of NEE with missing values replaced}
\item{Temp}{Temperature, quality filtered if isTRUE(controlGLPart$isFilterMeteoQualityFlag)}
\item{VPD}{Water pressure deficit, quality filtered if isTRUE(controlGLPart$isFilterMeteoQualityFlag)}
\item{Rg}{Incoming radiation}
\item{isDay}{Flag that is true for daytime records}
\item{isNight}{Flag that is true for nighttime records
}}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





