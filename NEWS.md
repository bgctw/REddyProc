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
 
