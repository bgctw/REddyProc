\name{partGLExtractStandardData}
\alias{partGLExtractStandardData}
\title{partGLExtractStandardData}
\description{Relevant columns from original input with defined names}
\usage{partGLExtractStandardData(ds, NEEVar = paste0("NEE", 
    suffixDash, "_f"), QFNEEVar = if (!missing(QFNEEVar.s)) QFNEEVar.s else paste0("NEE", 
    suffixDash, "_fqc"), QFNEEValue = if (!missing(QFNEEValue.n)) QFNEEValue.n else 0, 
    NEESdVar = if (!missing(NEESdVar.s)) NEESdVar.s else paste0("NEE", 
        suffixDash, "_fsd"), TempVar = paste0("Tair_f"), 
    QFTempVar = if (!missing(QFTempVar.s)) QFTempVar.s else paste0("Tair_fqc"), 
    QFTempValue = if (!missing(QFTempValue.n)) QFTempValue.n else 0, 
    VPDVar = if (!missing(VPDVar.s)) VPDVar.s else paste0("VPD_f"), 
    QFVPDVar = if (!missing(QFVPDVar.s)) QFVPDVar.s else paste0("VPD_fqc"), 
    QFVPDValue = if (!missing(QFVPDValue.n)) QFVPDValue.n else 0, 
    RadVar = if (!missing(RadVar.s)) RadVar.s else "Rg_f", 
    QFRadVar = if (!missing(QFRadVar.s)) QFRadVar.s else paste0("Rg_fqc"), 
    QFRadValue = if (!missing(QFRadValue.n)) QFRadValue.n else 0, 
    PotRadVar = if (!missing(PotRadVar.s)) PotRadVar.s else "PotRad_NEW", 
    suffix = if (!missing(Suffix.s)) Suffix.s else "", 
    NEEVar.s, QFNEEVar.s, QFNEEValue.n, NEESdVar.s, 
    TempVar.s, QFTempVar.s, QFTempValue.n, 
    VPDVar.s, QFVPDVar.s, QFVPDValue.n, RadVar.s, 
    QFRadVar.s, QFRadValue.n, PotRadVar.s, 
    Suffix.s, controlGLPart = partGLControl())}
\arguments{
  \item{ds}{dataset with all the specified input columns and
full days in equidistant times}
  \item{NEEVar}{Variable of NEE}
  \item{QFNEEVar}{Quality
flag of variable}
  \item{QFNEEValue}{Value of quality flag for
_good_ (original) data}
  \item{NEESdVar}{Variable of
standard deviation of net ecosystem fluxes}
  \item{TempVar}{Filled air or soil
temperature variable (degC)}
  \item{QFTempVar}{Quality flag of
filled temperature variable}
  \item{QFTempValue}{Value of temperature quality flag
for _good_ (original) data}
  \item{VPDVar}{Filled Vapor Pressure Deficit, VPD (hPa)}
  \item{QFVPDVar}{Quality flag of filled VPD variable}
  \item{QFVPDValue}{Value of VPD quality flag for
_good_ (original) data}
  \item{RadVar}{Filled radiation variable}
  \item{QFRadVar}{Quality flag of filled radiation variable}
  \item{QFRadValue}{Value of radiation quality flag for
_good_ (original) data}
  \item{PotRadVar}{Variable name of potential rad. (W / m2)}
  \item{suffix}{string inserted into column names before
identifier for NEE column defaults
(see \code{\link{sEddyProc_sMDSGapFillAfterUstar}}).}
  \item{NEEVar.s}{deprecated}
  \item{QFNEEVar.s}{deprecated}
  \item{QFNEEValue.n}{deprecated}
  \item{NEESdVar.s}{deprecated}
  \item{TempVar.s}{deprecated}
  \item{QFTempVar.s}{deprecated}
  \item{QFTempValue.n}{deprecated}
  \item{VPDVar.s}{deprecated}
  \item{QFVPDVar.s}{deprecated}
  \item{QFVPDValue.n}{deprecated}
  \item{RadVar.s}{deprecated}
  \item{QFRadVar.s}{deprecated}
  \item{QFRadValue.n}{deprecated}
  \item{PotRadVar.s}{deprecated}
  \item{Suffix.s}{deprecated}
  \item{controlGLPart}{further default parameters,
see \code{\link{partGLControl}}}
}
\details{The LRC fit usually weights NEE records by its uncertainty. In order
to also use
records with missing \code{NEESdVar}, uncertainty of the missing values
is by default set
to a conservatively high value, parameterized by
\code{controlGLPart$replaceMissingSdNEEParms)}.
Controlled by argument \code{replaceMissingSdNEEParms} in
\code{\link{partGLControl}}, but overruled
by argument \code{neglectNEEUncertaintyOnMissing}.}
\value{a data.frame with columns
\item{sDateTime}{first column of \code{ds},
usually the time stamp
not used, but usually first column is a dateTime is kept
for aiding debug}
\item{NEE}{NEE filtered for quality flay}
\item{sdNEE}{standard deviation of NEE
with missing values replaced}
\item{Temp}{Temperature, quality filtered
if isTRUE(controlGLPart$isFilterMeteoQualityFlag)}
\item{VPD}{Water pressure deficit, quality
filtered if isTRUE(controlGLPart$isFilterMeteoQualityFlag)}
\item{Rg}{Incoming radiation}
\item{isDay}{Flag that is true for daytime records}
\item{isNight}{Flag that is true for nighttime records
}}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





