\name{partGLControlLasslopCompatible}
\alias{partGLControlLasslopCompatible}
\title{partGLControlLasslopCompatible}
\description{Daytime flux partitioning parms compatible with with the pvWave}
\usage{partGLControlLasslopCompatible(nBootUncertainty = 0L, 
    minNRecInDayWindow = 10L, isAssociateParmsToMeanOfValids = FALSE, 
    isLasslopPriorsApplied = TRUE, isUsingLasslopQualityConstraints = TRUE, 
    isBoundLowerNEEUncertainty = FALSE, fixedTRefAtNightTime = 15, 
    isExtendTRefWindow = FALSE, smoothTempSensEstimateAcrossTime = FALSE, 
    isRefitMissingVPDWithNeglectVPDEffect = FALSE, 
    minPropSaturation = NA, isNeglectVPDEffect = FALSE, 
    replaceMissingSdNEEParms = c(NA, NA), 
    neglectNEEUncertaintyOnMissing = TRUE, 
    ...)}
\arguments{
  \item{nBootUncertainty}{0: Derive uncertainty from
curvature of a single fit, neglecting the uncertainty of previously
estimated temperature sensitivity, E0}
  \item{minNRecInDayWindow}{Minimum number of 10 valid records
for regression in a single window}
  \item{isAssociateParmsToMeanOfValids}{associate parameters to
the first record of the window for interpolation instead of mean across
valid records inside a window}
  \item{isLasslopPriorsApplied}{Apply fixed Lasslop priors
in LRC fitting.}
  \item{isUsingLasslopQualityConstraints}{avoid quality constraints
additional to the ones in Lasslop 2010}
  \item{isBoundLowerNEEUncertainty}{FALSE: avoid adjustment of very
low uncertainties before
day-Time fitting that avoids the high leverage those records with
unreasonable low uncertainty.}
  \item{fixedTRefAtNightTime}{use fixed (degree Centigrade)
temperature sensitivity
instead of median data temperature as reference temperature in
estimation of temperature sensitivity from night data}
  \item{isExtendTRefWindow}{avoid successively extending the
night-time window
in order to estimate a temperature sensitivity where previous
estimates failed}
  \item{smoothTempSensEstimateAcrossTime}{FALSE: use independent
estimates of temperature
sensitivity on each windows instead of a vector of E0 that is
smoothed over time}
  \item{isRefitMissingVPDWithNeglectVPDEffect}{FALSE: avoid
repeating estimation with \code{isNeglectVPDEffect = TRUE}}
  \item{minPropSaturation}{NA: avoid quality constraint of
sufficient saturation in data
This option is overruled, i.e. not considered, if option
isUsingLasslopQualityConstraints = TRUE.}
  \item{isNeglectVPDEffect}{FALSE: do not neglect VPD effect}
  \item{replaceMissingSdNEEParms}{do not replace missing NEE,
but see option}
  \item{neglectNEEUncertaintyOnMissing}{if there are records with
missing uncertainty of NEE inside one window, set all sdNEE to 1.
This overrules option replaceMissingSdNEEParms.}
  \item{\dots}{further arguments to \code{\link{partGLControl}}}
}



\author{TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{partGLControl}}}
\examples{
partGLControlLasslopCompatible()
}
