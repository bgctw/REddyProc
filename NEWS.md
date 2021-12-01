# REddyProc 1.3.1
- bug fix for day-time partitioning: Adapt default of argument uStarScenKeep
  to reflect changes in naming the uStar threshold scenarios 
  [#55](https://github.com/bgctw/REddyProc/issues/55#issue-1068184211).

# REddyProc 1.3.0
Accumulations of improvements warrant a new version and new CRAN release

- workaround bug in changepoint detection (package segmented) that
  reset random generator and caused resampling the same "random" bootstrap sample
  repeatedly.
- report the row and value of first non-numeric entry when checking for 
  numeric columns
- update vignette of aggregating uncertainty  
- clarifying several points in function documentations
- improving several error messages and warnings
- allow for custom color palette in fingerprint plots
- adapt C-code to work wit R4.x
- bugfix in fLoadFluxnet15 with argument additional_columns

# REddyProc 1.2.4

- Provided VPD estimate of longer gaps by assuming daily minimum temperature at
  dewpoint.
- Daytime partitioning: now uses reduced LRC model if there is no 
  large VPD in daily subset.

# REddyProc 1.2.3

- provide reading and exporting from Fluxnet 2015 formatted files and datasets.

# REddyProc 1.2.2

- improved error messages on uStar problems
- deal with missing night-time data during day-time partitioning
- provide option to select uStar case for which to keep detailed results when 
  computing several scenarios 
  and explain the new argument uStarScenKeep in uStarCases vignette.
- fix error in aggUncertainty vignette of requiring nEff of at least one 
  instead of zero.
- check assumption of non-missing VPD at entry of daytime partitioning.

# REddyProc 1.2.1

- fix bug in time conversion: explicitly check for leap-years instead of
  relying on strptime returning NA for implausible day-of-year. 
- new function sEddyProc_sGetUstarSuffixes to get current uStar suffixes
- refactoring: replace deprecated select_ method by select
- avoid warning on replacing columns during scenarios of daytime partitioning

# REddyProc 1.2

- move GeoFunctions functionality to packages solartime and bigleaf

- implemented experimental modified daytime partitioning after Keenan et al. 2019
    where nighttime ecosystem respiration is estimated based on nighttime
    estimate of respiration at reference temperature.

# REddyProc 1.1.6

## support consistent processing across u* threshold scenarios 

- also subsequent gapfilling and partitioning
- keep information about u* thresholds and scenarios in class
- adapted vignettes and overview

## simplify argument naming and defaults

- Changed argument names to exclude the type specifier suffix
  , e.g. 'FluxVar.s' to 'FluxVar'. 
  Provding the old argument names still works, but gives a warning. However,
  at some future version, the old argument names will be removed.
- Changed default column suffix in sEddyProc_sMDSGapFillAfterUstar from 
  'withUstar' to 'uStar' for consistency with 
  'sEddyProc_sMDSGapFillAfterUStarDistr'

## improve fingerprint plots

- fingerprint plots align month axis at the 1st of month instead of center
- fingerprint plots change Infinity to NA before plotting

## deprecated method 'sEddyProc_sEstUstarThreshold'

And replace by 'sEddyProc_sEstUstarThold' with a simpler return value of
only the component 'uStarTh' of the former complex return value.
Use cases relying on the other return value components can still get them
from class variable 'sUSTAR_DETAILS'.

Currently, the method gives only a warning, but will be removed 
in future in version 2.x of REddyProc.

## further changes

- set default number of uStar bootstrap samples 
  to 200 to be consistent with the paper.
- prevent bug when calling 'sEddyProc_sEstUstarThresholdDistribution'
  with a single element vector.
- on failing 'sEddyProc_sMRFluxPartition' tell user how to relax 
  temperature range constraint    


# REddyProc 1.1.5

Moving profiling code and associated dependencies out of the distributed code.

# REddyProc 1.1.4

Reworking dependencies

- implementing logit and invlogit from logitnorm directly in REddyProc
- moving NetCDF related functions and dependencies to separate package 
  REddyProcNCDF

# REddyProc 1.1.3

Code cleanup

- replacing deprecated dplyr underscore functions by rlang syntax
- extended tests

# REddyProc 1.1.2

Further CRAN issues, not affecting usage:

- avoid building computation-intensive vignette on CRAN
- fixed a small memory-leak in uStar-filtering routine whichValueGreaterEqualC
- deleted one example that still violated CRAN policy by writing to /tmp


# REddyProc 1.1

CRAN policy related adjustments, not affecting the usage:

- avoid writing files outside tempdir()
- Using latex syntax \donttest{} instead of R function donttest() in examples.
- Converted the realistic data example from function example section 
  to a series of vignettes.


# REddyProc 1.0.0.9000 

There have been major code restructurings for implementing alternative light 
response curves in the day-time flux partitioning. 
A few non-backward compatible changes have been introduced, e.g. by shortening 
function names. Hence this version is meant to be tested before changing version 
number to 1.1.0. 

Interface changes

- renamed usGetAnnualSeasonUStarMappingFromDistributionResult to 
  usGetAnnualSeasonUStarMap
- renamed usGetSeasonalSeasonUStarMappingFromDistributionResult to 
  usGetSeasonalSeasonUStarMap
- removed position arguments from sMRFluxPartition
  consistently use sSetLocationInfo before
  
Changes in Day-Time partitioning

- adopted Lasslop prior information on LRC-fit parameters as default
- more options to control daytime-fitting with partGLControl
- quickly maximise compatibility with Lasslop-pvWave processing by function
  partGLControlLasslopCompatible

# REddyProc 0.8.6.9000

This is a development version put to github so that all contributers can test 
and contribute for preparing version 1.0.

Gapfilling
- now marking half-hours for which uStar is missing as a gap
- not filtering day-time for low uStar (but kept option to do so)

Daytime-Flux partitioning
- estimating temperature E0 with reference temperature at median of the data
- smoothing E0 estimates across time 
- re-estimating prior and initial value of RRef for smoothed E0

## Further changes

### Migration to github

The hosting of the development moved (maybe temporarily) from mercurial to 
github. Releases will still be put to r-forge, because of its good 
package-checking setup for several platforms, and the help for submission to 
CRAN, but versioning and development of the code will be done on github. 

### Documentation

A README.Rmd and this NEWS.md file have been added.
 
