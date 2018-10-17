Different treatments of uStar threshold
=======================================

The recommended way of dealing with the uncertain uStar threshold for
filtering the half-hourly data, is to repeat all the processing steps
with several bootstrapped estimates of the threshold as in
`vignette('useCase')`.

First, some setup.

    #+++ load libraries used in this vignette
    library(REddyProc)
    library(dplyr)
    #+++ define directory for outputs
    outDir <- tempdir()  # CRAN policy dictates to write only to this dir in examples
    #outDir <- "out"     # to write to subdirectory of current users dir
    #+++ Add time stamp in POSIX time format to example data
    EddyDataWithPosix <- fConvertTimeToPosix(
      Example_DETha98, 'YDH', Year = 'Year', Day = 'DoY', Hour = 'Hour')

Not applying uStar filtering
----------------------------

Subsequent processing steps can be performed without further uStar
filtering using `sEddyProc_sMDSGapFill`. Corresponding result columns
then have no uStar specific suffix.

    EProc <- sEddyProc$new(
      'DE-Tha', EddyDataWithPosix, c('NEE','Rg','Tair','VPD', 'Ustar'))
    EProc$sMDSGapFill('NEE')

    ## Warning in EProc$sMDSGapFill("NEE"): Variable NEE contains long runs of
    ## numerically equal numbers. Longest of 10 repeats of value 0.695 starts at
    ## index 1397

    grep("NEE.*_f$",names(EProc$sExportResults()), value = TRUE)

    ## [1] "NEE_f"

User-specified uStar threshold
------------------------------

The user can provide value for uStar-filtering before gapfilling, using
`sEddyProc_sMDSGapFillAfterUstar`. Output columns for this uStar
scenario use the suffix as specified by argument `uStarSuffix` which
defaults to "uStar".

The friction velocity, uStar, needs to be in column named "Ustar" of the
input dataset.

    EProc <- sEddyProc$new(
      'DE-Tha', EddyDataWithPosix, c('NEE','Rg','Tair','VPD', 'Ustar'))
    uStar <- 0.46
    EProc$sMDSGapFillAfterUstar('NEE', uStarTh = uStar)
    grep("NEE.*_f$",names(EProc$sExportResults()), value = TRUE)

    ## [1] "NEE_uStar_f"

Sinlge uStar threshold estimate
-------------------------------

The uStar threshold can be estimated from the uStar-NEE relationship
from the data without estimating its uncertainty by a bootstrap.

    EProc <- sEddyProc$new(
      'DE-Tha', EddyDataWithPosix, c('NEE','Rg','Tair','VPD', 'Ustar'))
    # estimating the thresholds based on the data (without bootstrap)
    (uStarTh <- EProc$sEstUstarThold())

    ##   aggregationMode seasonYear  season     uStar
    ## 1          single         NA    <NA> 0.4162500
    ## 2            year       1998    <NA> 0.4162500
    ## 3          season       1998 1998001 0.4162500
    ## 4          season       1998 1998003 0.4162500
    ## 5          season       1998 1998006 0.3520000
    ## 6          season       1998 1998009 0.3369231
    ## 7          season       1998 1998012 0.1740000

    # may plot saturation of NEE with UStar for a specified season to pdf
    EProc$sPlotNEEVersusUStarForSeason(levels(uStarTh$season)[3], dir = outDir )

Next, the annual estimate is used as the default in gap-filling. Output
columns use the suffix as specified by argument `uSstarSuffix` which
defaults to "uStar".

    #usGetAnnualSeasonUStarMap(EProc$sUSTAR_DETAILS$uStarTh)
    EProc$sMDSGapFillAfterUstar('NEE')

    ## Warning in sMDSGapFill(fluxVar, QFVar = attr(qfUStar, "varnames"), QFValue
    ## = 0, : Variable NEE contains long runs of numerically equal numbers.
    ## Longest of 10 repeats of value 0.695 starts at index 1397

    grep("NEE.*_f$",names(EProc$sExportResults()), value = TRUE)

    ## [1] "NEE_uStar_f"

Scenarios across distribution of u\* threshold estimate
-------------------------------------------------------

Chossing a different u\* threshold effects filtering and the subsequent
processing steps of gap-filling, and flux-partitioning. In order to
quantify the uncertainty due to not exactly knowing the u\* threshold,
these processing steps should be repeated for different threshold
scenarios, and the spread across the results should be investigated.

First, the quantiles of the threshodl distribution are estimated by
bootstrap.

    EProc <- sEddyProc$new(
      'DE-Tha', EddyDataWithPosix, c('NEE','Rg','Tair','VPD', 'Ustar'))

    ## New sEddyProc class for site 'DE-Tha'

    EProc$sEstimateUstarScenarios(
        nSample = 100L, probs = c(0.05, 0.5, 0.95))

    ## 

    ## Estimated UStar distribution of:
    ##      uStar        5%       50%      95%
    ## 1 0.41625 0.3764458 0.4470139 0.640325 
    ## by using  100 bootstrap samples and controls:
    ##                        taClasses                    UstarClasses 
    ##                               7                              20 
    ##                           swThr            minRecordsWithinTemp 
    ##                              10                             100 
    ##          minRecordsWithinSeason            minRecordsWithinYear 
    ##                             160                            3000 
    ## isUsingOneBigSeasonOnFewRecords 
    ##                               1

    # inspect the thresholds to be used by default
    EProc$sGetUstarScenarios()

    ##    season   uStar       U05       U50      U95
    ## 1 1998001 0.41625 0.3764458 0.4470139 0.640325
    ## 2 1998003 0.41625 0.3764458 0.4470139 0.640325
    ## 3 1998006 0.41625 0.3764458 0.4470139 0.640325
    ## 4 1998009 0.41625 0.3764458 0.4470139 0.640325
    ## 5 1998012 0.41625 0.3764458 0.4470139 0.640325

By default the annually aggregated threshold estimates are used for each
season whithin one year as in the original method publication. Here, we
tell to use the seasonal threshold estimates and to omit the first
threshold scenario based on the un-bootstrapped data in the second
column.

    (uStarThAgg <- EProc$sGetEstimatedUstarThresholdDistribution())

    ##   aggregationMode seasonYear  season     uStar        5%       50%
    ## 1          single         NA    <NA> 0.4162500 0.3764458 0.4470139
    ## 2            year       1998    <NA> 0.4162500 0.3764458 0.4470139
    ## 3          season       1998 1998001 0.4162500 0.3764458 0.4470139
    ## 4          season       1998 1998003 0.4162500 0.3097727 0.4025000
    ## 5          season       1998 1998006 0.3520000 0.2964060 0.3853571
    ## 6          season       1998 1998009 0.3369231 0.2851547 0.4042140
    ## 7          season       1998 1998012 0.1740000 0.2484000 0.4318182
    ##         95%
    ## 1 0.6403250
    ## 2 0.6403250
    ## 3 0.6403250
    ## 4 0.5740354
    ## 5 0.4581750
    ## 6 0.5114247
    ## 7 0.6403250

    EProc$sSetUstarScenarios( usGetSeasonalSeasonUStarMap(uStarThAgg)[,-2])
    # inspect the changed thresholds to be used, default annual aggregate
    EProc$sGetUstarScenarios()

    ##    season       U05       U50       U95
    ## 3 1998001 0.3764458 0.4470139 0.6403250
    ## 4 1998003 0.3097727 0.4025000 0.5740354
    ## 5 1998006 0.2964060 0.3853571 0.4581750
    ## 6 1998009 0.2851547 0.4042140 0.5114247
    ## 7 1998012 0.2484000 0.4318182 0.6403250

Several function whose name ends with 'UstarScens' perform the
subsequent processing steps for all uStar scenarios. They operate and
create columns that differ between threshold scenarios by a suffix.

    EProc$sMDSGapFillUStarScens("NEE")

    ## Warning in sMDSGapFill(fluxVar, QFVar = attr(qfUStar, "varnames"), QFValue
    ## = 0, : Variable NEE contains long runs of numerically equal numbers.
    ## Longest of 10 repeats of value 0.695 starts at index 1397

    grep("NEE_.*_f$",names(EProc$sExportResults()), value = TRUE)

    ## [1] "NEE_U05_f" "NEE_U50_f" "NEE_U95_f"

    EProc$sSetLocationInfo(LatDeg = 51.0, LongDeg = 13.6, TimeZoneHour = 1)
    EProc$sMDSGapFill('Tair', FillAll = FALSE)

    ## Warning in EProc$sMDSGapFill("Tair", FillAll = FALSE): Variable Tair
    ## contains long runs of numerically equal numbers. Longest of 9 repeats of
    ## value -6.2 starts at index 16357

    EProc$sMDSGapFill('VPD', FillAll = FALSE)

    ## Warning in EProc$sMDSGapFill("VPD", FillAll = FALSE): Variable VPD contains
    ## long runs of numerically equal numbers. Longest of 84 repeats of value 0.3
    ## starts at index 16352

    EProc$sMRFluxPartitionUStarScens()
    grep("GPP_.*_f$",names(EProc$sExportResults()), value = TRUE)

    ## [1] "GPP_U05_f" "GPP_U50_f" "GPP_U95_f"

See also
--------

A more advanced case of user-specified seasons for uStar threshold
estimate is given in [`vignette('DEGebExample')`](DEGebExample.html).
