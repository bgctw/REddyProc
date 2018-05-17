# REddyProc 1.1.5

Moving profiling code and associated dependencies out of the distributed code.


# REddyProc 1.1.4

Reworking dependencies

- implementing logit and invlogit from logtinorm directly in REddyProc
- moving NetCDF related functions and dependencies to separate package REddyProcNCDF

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

There have been major code restructurings for implementing alternative light response curves in the day-time flux partitioning. 
A few non-backward compatible changes have been introduced, e.g. by shortening function names. Hence this version is meant to be tested before changing version number to 1.1.0. 

Interface changes

- renamed usGetAnnualSeasonUStarMappingFromDistributionResult to usGetAnnualSeasonUStarMap
- renamed usGetSeasonalSeasonUStarMappingFromDistributionResult to usGetSeasonalSeasonUStarMap
- removed position arguments from sMRFluxPartition
  consistently use sSetLocationInfo before
  
Changes in Day-Time partitioning

- adpted Lasslop prior information on LRC-fit parameters as default
- more options to control daytim-fitting with partGLControl
- quickly maximise compatibility with Lasslop-pvWave processing by function
  partGLControlLasslopCompatible

# REddyProc 0.8.6.9000

This is a development version put to github so that all contributers can test and contribute for preparing version 1.0.

Gapfilling
- now marking half-hours for which uStar is missing as a gap
- not filtering day-time for low uStar (but kept option to do so)

Daytime-Flux partitioning
- estimating temperature E0 with Reference Temperatue at median of the data
- smoothing E0 estimates across time 
- re-estimating prior and intial value of RRef for smoothed E0

## Further changes

### Migration to github

The hosting of the development moved (maybe temporarily) from mercurial to github. Releases will still be put to r-forge, because of its good package-checking setup for several platforms, and the help for submission to CRAN, but versioning and development of the code will be done on github. 

### Documentation

A README.Rmd and this NEWS.md file have been added.
 
