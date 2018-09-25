
<!-- 
README.md is generated from README.Rmd. Please edit that file
#knitr::knit("README.Rmd") 
rmarkdown::render("README.Rmd") 
maybe clear cache before
-->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/REddyProc)](http://cran.r-project.org/package=REddyProc) [![Travis-CI Build Status](https://travis-ci.org/bgctw/REddyProc.svg?branch=master)](https://travis-ci.org/bgctw/REddyProc)

Overview
--------

`REddyProc` package supports processing (half)hourly data from Eddy-Covariance sensors.

There is an online-formular to use the functionality of the package including description at <https://www.bgc-jena.mpg.de/bgi/index.php/Services/REddyProcWeb>.

Installation
------------

``` r
# Release stable version from CRAN
install.packages("REddyProc")

# The development version from GitHub using devtools:
# install.packages("devtools")
devtools::install_github("bgctw/REddyProc")
```

The REddyProc~package requires a quite recent versions of the tidyverse packages. On encountering problems on installations with older versions should run the following code before installing REddyProc.

``` r
install.packages("tidyverse")
update.packages(oldPkgs="dplyr")
```

Usage
-----

A simple example performs Lookuptable-based gapFilling of Net-Ecosystem-Exchange (NEE) and plotting a fingerprint plot of the filled values.

``` r
library(REddyProc)
#+++ Input data from csv (example needs to be downloaded)
examplePath <- getExamplePath('Example_DETha98.txt', isTryDownload = TRUE)
if (length(examplePath)) {
  EddyData.F <- fLoadTXTIntoDataframe(examplePath)
} else {
  warning(
      "Could not find example text data file."
      ," In order to execute this example code,"
      ," please, allow downloading it from github. " 
      ," Type '?getExamplePath' for more information.")
  # using RData version distributed with the package instead
  EddyData.F <- Example_DETha98
}
#+++ If not provided, calculate VPD from Tair and rH
EddyData.F$VPD <- fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair)
#+++ Add time stamp in POSIX time format
EddyDataWithPosix.F <- fConvertTimeToPosix(
  EddyData.F, 'YDH', Year.s = 'Year', Day.s = 'DoY', Hour.s = 'Hour')
#+++ Initalize R5 reference class sEddyProc for processing of eddy data
#+++ with all variables needed for processing later
EddyProc.C <- sEddyProc$new(
  'DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD', 'Ustar'))
#Location of DE-Tharandt
EddyProc.C$sSetLocationInfo(
  LatDeg = 51.0, LongDeg = 13.6, TimeZoneHour = 1)  
#
#++ Fill NEE gaps with MDS gap filling algorithm (without prior ustar filtering)
EddyProc.C$sMDSGapFill('NEE', FillAll.b = FALSE)
#
#++ Export gap filled and partitioned data to standard data frame
FilledEddyData.F <- EddyProc.C$sExportResults()
#
#++ Example plots of filled data to screen or to directory \plots
EddyProc.C$sPlotFingerprintY('NEE_f', Year.i = 1998)
```

![](README-example-1.png)

Further examples are in [vignette(useCase)](https://github.com/bgctw/REddyProc/blob/master/vignettes/useCase.md) and [vignette(DEGebExample)](https://github.com/bgctw/REddyProc/blob/master/vignettes/DEGebExample.md) and further md-files of the [vignettes directory](https://github.com/bgctw/REddyProc/blob/master/vignettes).

Docker images
-------------

Docker images are provided that comprise rstudio, rocker/tidyverse, and REddyProc. There are different version for the latest push to github, for the version on CRAN and for specific tags starting from 1.1.4.

-   bgctw/reddyproc:latest
-   bgctw/reddyproc\_cran
-   bgctw/reddyproc:`tag`

They are usually run with installed docker by typing at a shell:

    docker run --rm -p 8787:8787 <imagename>

Then the loading url `localhost:8787` in a browser window should bring up RStudio
(default username and password are both rstudio), where you can type the above usage example.

Reference
---------

The methodology and benchmark of `REddyProc` 1.1.3 is descibed in the following paper:

Wutzler, T., Lucas-Moffat, A., Migliavacca, M., Knauer, J., Sickel, K., Šigut, L., Menzer, O., and Reichstein, M. (2018): Basic and extensible post-processing of eddy covariance flux data with REddyProc, Biogeosciences, 15, 5015-5030, <https://doi.org/10.5194/bg-15-5015-2018>.
