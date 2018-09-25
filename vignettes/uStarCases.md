---
output: 
  rmarkdown::html_vignette:
    keep_md: true
vignette: >
  %\VignetteEngine{knitr::knitr}
  %\VignetteIndexEntry{different treatments of uStar threshold}
  %\usepackage[UTF-8]{inputenc}

---








# Different treatments of uStar threshold

The recommended way of dealing with the uncertain uStar threshold for filtering
the half-hourly data, is to repeat all the processing steps with several 
bootstrapped estimates of the threshold as in `vignette('useCase')`.

First, some setup.


```r
#+++ load libraries used in this vignette
library(REddyProc)
library(dplyr)
#+++ define directory for outputs
outDir <- tempdir()  # CRAN policy dictates to write only to this dir in examples
#outDir <- "out"     # to write to subdirectory of current users dir
#+++ Add time stamp in POSIX time format to example data
EddyDataWithPosix.F <- fConvertTimeToPosix(Example_DETha98, 'YDH',Year.s = 'Year' 
    ,Day.s = 'DoY',Hour.s = 'Hour')
```

## Not applying uStar filtering

Subsequent processing steps can be performed without further uStar filtering
using `sEddyProc_sMDSGapFill`. Corresponding result columns then have
no uStar specific suffix. 


```r
EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, 
	c('NEE','Rg','Tair','VPD', 'Ustar'))
EddyProc.C$sMDSGapFill('NEE')
grep("NEE.*_f$",names(EddyProc.C$sExportResults()), value = TRUE)
```

```
## [1] "NEE_f"
```

## User-specified uStar threshold

The user can provide value for uStar-filtering before gapfilling, using
`sEddyProc_sMDSGapFillAfterUstar`. Output columns for this uStar scenario use
the suffix as specified by argument `uStarSuffix` which defaults to "uStar". 

The friction velocity, uStar, needs to be in column named "Ustar" of the input 
dataset.


```r
EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, 
	c('NEE','Rg','Tair','VPD', 'Ustar'))
uStar <- 0.46
EddyProc.C$sMDSGapFillAfterUstar('NEE', uStarTh = uStar)
grep("NEE.*_f$",names(EddyProc.C$sExportResults()), value = TRUE)
```

```
## [1] "NEE_uStar_f"
```

## Sinlge uStar threshold estimate

The uStar threshold can be estimated from the uStar-NEE relationship 
from the data without estimating its uncertainty by a bootstrap.


```r
EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, 
	c('NEE','Rg','Tair','VPD', 'Ustar'))
# estimating the thresholds based on the data (without bootstrap)
(uStarTh <- EddyProc.C$sEstUstarThreshold()$uStarTh)
```

```
##   aggregationMode seasonYear  season     uStar
## 1          single         NA    <NA> 0.4162500
## 2            year       1998    <NA> 0.4162500
## 3          season       1998 1998001 0.4162500
## 4          season       1998 1998003 0.4162500
## 5          season       1998 1998006 0.3520000
## 6          season       1998 1998009 0.3369231
## 7          season       1998 1998012 0.1740000
```

```r
# may plot saturation of NEE with UStar for a specified season to pdf
EddyProc.C$sPlotNEEVersusUStarForSeason(levels(uStarTh$season)[3], dir = outDir )
```

Next, the annual estimate is used as the default in gap-filling.
Output columns use the suffix as specified by argument `uSstarSuffix` 
which defaults to "uStar".


```r
#usGetAnnualSeasonUStarMap(EddyProc.C$sUSTAR_DETAILS$uStarTh)
EddyProc.C$sMDSGapFillAfterUstar('NEE')
grep("NEE.*_f$",names(EddyProc.C$sExportResults()), value = TRUE)
```

```
## [1] "NEE_uStar_f"
```
## Scenarios across distribution of u* threshold estimate

Chossing a different u* threshold effects filtering and the subsequent processing
steps of gap-filling, and flux-partitioning. In order to quantify the uncertainty
due to not exactly knowing the u* threshold, these processing steps should be
repeated for different threshold scenarios, and the spread across the results should
be investigated.

First, the quantiles of the threshodl distribution are estimated by bootstrap.


```r
EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, 
	c('NEE','Rg','Tair','VPD', 'Ustar'))
```

```
## New sEddyProc class for site 'DE-Tha'
```

```r
EddyProc.C$sEstimateUstarScenarios(
    nSample = 100L, probs = c(0.05, 0.5, 0.95))
```

```
## 
```

```
## Estimated UStar distribution of:
##      uStar        5%       50%       95%
## 1 0.41625 0.3720107 0.4463021 0.6579375 
## by using  100 bootstrap samples and controls:
##                        taClasses                    UstarClasses 
##                               7                              20 
##                           swThr            minRecordsWithinTemp 
##                              10                             100 
##          minRecordsWithinSeason            minRecordsWithinYear 
##                             160                            3000 
## isUsingOneBigSeasonOnFewRecords 
##                               1
```

```r
# inspect the thresholds to be used
EddyProc.C$sGetUstarScenarios()
```

```
##    season   uStar       U05       U50       U95
## 1 1998001 0.41625 0.3720107 0.4463021 0.6579375
## 2 1998003 0.41625 0.3720107 0.4463021 0.6579375
## 3 1998006 0.41625 0.3720107 0.4463021 0.6579375
## 4 1998009 0.41625 0.3720107 0.4463021 0.6579375
## 5 1998012 0.41625 0.3720107 0.4463021 0.6579375
```

By default the annually aggregated threshold estimates are used for each season
whithin one year as in the original method publication.
Here, we tell to use the seasonal threshold estiamtes and to
omit the first threshold scenario based on the un-bootstrapped data in the second 
column.


```r
(uStarThAgg <- EddyProc.C$sGetEstimatedUstarThresholdDistribution())
```

```
##   aggregationMode seasonYear  season     uStar        5%       50%
## 1          single         NA    <NA> 0.4162500 0.3720107 0.4463021
## 2            year       1998    <NA> 0.4162500 0.3720107 0.4463021
## 3          season       1998 1998001 0.4162500 0.3720107 0.4463021
## 4          season       1998 1998003 0.4162500 0.3263455 0.4088182
## 5          season       1998 1998006 0.3520000 0.3211214 0.3858333
## 6          season       1998 1998009 0.3369231 0.2684043 0.3829583
## 7          season       1998 1998012 0.1740000 0.1883429 0.4150785
##         95%
## 1 0.6579375
## 2 0.6579375
## 3 0.6579375
## 4 0.5553000
## 5 0.4443083
## 6 0.5187952
## 7 0.5549714
```

```r
EddyProc.C$sSetUstarScenarios( usGetSeasonalSeasonUStarMap(uStarThAgg)[,-2])
# inspect the changed thresholds to be used
EddyProc.C$sGetUstarScenarios()
```

```
##    season       U05       U50       U95
## 3 1998001 0.3720107 0.4463021 0.6579375
## 4 1998003 0.3263455 0.4088182 0.5553000
## 5 1998006 0.3211214 0.3858333 0.4443083
## 6 1998009 0.2684043 0.3829583 0.5187952
## 7 1998012 0.1883429 0.4150785 0.5549714
```

Several function whose name ends with 'UstarScens'
perform the subsequent processing steps for all uStar scenarios.
They operate and create columns that differ between threshold scenarios by
a suffix.


```r
EddyProc.C$sMDSGapFillUStarScens("NEE")
grep("NEE_.*_f$",names(EddyProc.C$sExportResults()), value = TRUE)
```

```
## [1] "NEE_U05_f" "NEE_U50_f" "NEE_U95_f"
```

```r
EddyProc.C$sSetLocationInfo(LatDeg = 51.0, LongDeg = 13.6, TimeZoneHour = 1)
EddyProc.C$sMDSGapFill('Tair', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('VPD', FillAll.b = FALSE)
EddyProc.C$sMRFluxPartitionUStarScens()
grep("GPP_.*_f$",names(EddyProc.C$sExportResults()), value = TRUE)
```

```
## [1] "GPP_U05_f" "GPP_U50_f" "GPP_U95_f"
```

## See also
A more advanced case of user-specified seasons for
uStar threshold estimate is given in [`vignette('DEGebExample')`](DEGebExample.html).
