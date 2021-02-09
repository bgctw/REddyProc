\name{partGLControl}
\alias{partGLControl}
\title{partGLControl}
\description{Default list of parameters for Lasslop 2010 daytime flux partitioning
For highest compatibility to the pvWave code of G.Lasslop
(used by first BGC-online tool)
see function \code{\link{partGLControlLasslopCompatible}}.}
\usage{partGLControl(LRCFitConvergenceTolerance = 0.001, 
    nLRCFitConvergenceTolerance = 0.001, 
    nBootUncertainty = 30L, minNRecInDayWindow = 10L, 
    isAssociateParmsToMeanOfValids = TRUE, 
    isLasslopPriorsApplied = TRUE, isUsingLasslopQualityConstraints = FALSE, 
    isSdPredComputed = TRUE, isFilterMeteoQualityFlag = FALSE, 
    isBoundLowerNEEUncertainty = TRUE, fixedTRefAtNightTime = NA, 
    isExtendTRefWindow = TRUE, smoothTempSensEstimateAcrossTime = TRUE, 
    isNeglectPotRadForNight = FALSE, NRHRfunction = FALSE, 
    isNeglectVPDEffect = FALSE, isRefitMissingVPDWithNeglectVPDEffect = TRUE, 
    fixedTempSens = data.frame(E0 = NA_real_, 
        sdE0 = NA_real_, RRef = NA_real_), 
    replaceMissingSdNEEParms = c(perc = 0.2, 
        minSd = 0.7), neglectNEEUncertaintyOnMissing = FALSE, 
    minPropSaturation = NA, useNightimeBasalRespiration = FALSE)}
\arguments{
  \item{LRCFitConvergenceTolerance}{convergence criterion for rectangular
light response curve fit.
If relative improvement of reducing residual sum of squares between
predictions and
observations is less than this criterion, assume convergence.
Decrease to get more precise parameter estimates, Increase for speedup.}
  \item{nLRCFitConvergenceTolerance}{convergence criterion for
nonrectangular light response curve fit.
Here its a factor of machine tolerance.}
  \item{nBootUncertainty}{number of bootstrap samples for
estimating uncertainty.
Set to zero to derive uncertainty from curvature of a single fit}
  \item{minNRecInDayWindow}{Minimum number of data points
for regression}
  \item{isAssociateParmsToMeanOfValids}{set to FALSE to
associate parameters to
the first record of the window for interpolation
instead of mean across valid records inside a window}
  \item{isLasslopPriorsApplied}{set to TRUE to apply strong fixed
priors on LRC fitting.
Returned parameter estimates claimed valid for some case where not
enough data was available}
  \item{isUsingLasslopQualityConstraints}{set to TRUE to avoid
quality constraints additional to Lasslop 2010}
  \item{isSdPredComputed}{set to FALSE to avoid computing
standard errors
of Reco and GPP for small performance increase}
  \item{isFilterMeteoQualityFlag}{set to TRUE to use only records
where quality flag
of meteo drivers (radiation, temperature, VPD) is zero, i.e.
non-gapfilled for parameter estimation.
For prediction, the gap-filled value is used always, to produce
predictions also for gaps.}
  \item{isBoundLowerNEEUncertainty}{set to FALSE to avoid adjustment
of very low uncertainties before
day-Time fitting that avoids the high leverage those records with
unreasonable low uncertainty.}
  \item{fixedTRefAtNightTime}{if a finite value (degree Centigrade)
is given, it is used instead of median data temperature as reference
temperature in estimation of temperature sensitivity from night data}
  \item{isExtendTRefWindow}{set to FALSE to avoid successively
extending the night-time window
in order to estimate a temperature sensitivity where previous estimates
failed}
  \item{smoothTempSensEstimateAcrossTime}{set to FALSE to use
independent estimates of temperature
sensitivity on each windows instead of a vector of E0 that is
smoothed over time}
  \item{isNeglectPotRadForNight}{set to TRUE to not use potential
radiation in determining night-time data.}
  \item{NRHRfunction}{deprecated: Flag if TRUE use the NRHRF
for partitioning; Now use \code{lrcFitter = NonrectangularLRCFitter()}}
  \item{isNeglectVPDEffect}{set to TRUE to avoid using VPD in the
computations. This may help when VPD is rarely measured.}
  \item{isRefitMissingVPDWithNeglectVPDEffect}{set to FALSE to avoid
repeating estimation
with \code{isNeglectVPDEffect = TRUE} trying to predict when VPD
is missing}
  \item{fixedTempSens}{data.frame
of one row or nRow = nWindow
corresponding to return value of \code{partGLFitNightTimeTRespSens}
While column \code{RRef} is used only as a  prior and initial value for
the daytime-fitting and can be NA,
\code{E0} is used as given temperature sensitivity and varied according
to \code{sdE0} in the bootstrap.}
  \item{replaceMissingSdNEEParms}{parameters for
replacing missing standard deviation of NEE.
see \code{replaceMissingSdByPercentage}.
Default sets missing uncertainty to 20\% of NEE but at least 0.7
flux-units (usually mumol CO2 / m2 / s).
Specify c(NA, NA) to avoid replacing missings in standard deviation of
NEE and to omit those records from LRC fit.}
  \item{neglectNEEUncertaintyOnMissing}{If set to TRUE: if there are
records with missing uncertainty of NEE inside one window,
set all uncertainties to 1.
This overrules option replaceMissingSdNEEParms.}
  \item{minPropSaturation}{quality criterion for sufficient data
in window. If GPP prediction of highest PAR of window is less than
minPropSaturation * (GPP at light-saturation, i.e. beta)
this indicates that PAR is not sufficiently high to constrain the
shape of the LRC}
  \item{useNightimeBasalRespiration}{set to TRUE to estimate
nighttime respiration based on basal respiration estimated on
nighttime data instead of basal respiration estimated from daytime
data. This implements the modified daytime method from
Keenan 2019 (doi:10.1038/s41559-019-0809-2)}
}

\value{list with entries of given arguments.}

\author{TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{partitionNEEGL}}}
\examples{
partGLControl(nBootUncertainty = 40L)
}
