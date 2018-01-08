---
author: "Thomas Wutzler"
date: "2018-01-08"
output: 
  rmarkdown::html_vignette:
    keep_md: true
vignette: >
  %\VignetteEngine{knitr::knitr}
  %\VignetteIndexEntry{DEGeb example: UStar estimation}
  %\usepackage[UTF-8]{inputenc}
---



Crop example demonstrating multiple years and user defined uStar-Seasons
========================================================================


```r
#isDevelopMode <- TRUE
if (!exists("isDevelopMode")) library(REddyProc)
set.seed(0815)      # for reproducible results
```

First, the data is loaded. This example uses data that has been downloaded 
from http://www.europe-fluxdata.eu
and preprocessed by `fLoadEuroFlux16`, where the DateTime Column has been created, 
and the variables renamed to the BGC-convention (e.g. Tair instead of Ta).


```r
		data(DEGebExample)
		summary(DEGebExample)
```

```
##     DateTime                        NEE              Ustar       
##  Min.   :2004-01-01 00:30:00   Min.   :-49.919   Min.   :0.0000  
##  1st Qu.:2004-10-01 00:22:30   1st Qu.: -1.864   1st Qu.:0.0640  
##  Median :2005-07-02 00:15:00   Median :  0.635   Median :0.1490  
##  Mean   :2005-07-02 00:15:00   Mean   : -1.935   Mean   :0.1884  
##  3rd Qu.:2006-04-02 00:07:30   3rd Qu.:  1.834   3rd Qu.:0.2800  
##  Max.   :2007-01-01 00:00:00   Max.   : 19.008   Max.   :2.0450  
##                                NA's   :21849     NA's   :1149    
##       Tair               rH               Rg         
##  Min.   :-16.710   Min.   : 15.87   Min.   :   0.00  
##  1st Qu.:  3.360   1st Qu.: 66.61   1st Qu.:   0.00  
##  Median :  9.970   Median : 79.10   Median :   2.04  
##  Mean   :  9.664   Mean   : 75.24   Mean   : 124.71  
##  3rd Qu.: 15.520   3rd Qu.: 87.07   3rd Qu.: 176.03  
##  Max.   : 34.680   Max.   :100.00   Max.   :1046.03  
##                    NA's   :1
```


VPD was not given with the original dataset and is calculated from Tair and rH.

```r
		DEGebExample$VPD <- fCalcVPDfromRHandTair(DEGebExample$rH, DEGebExample$Tair)
		EddyProc.C <- sEddyProc$new('DE-Geb', DEGebExample, c('NEE','Rg','Tair','VPD', 'Ustar'))
		EddyProc.C$sSetLocationInfo(Lat_deg.n = 51.1, Long_deg.n = 10.9, TimeZone_h.n = 1)  #Location of Gebesee
```

Defining Seasons with different surface friction conditions 
-----------------------------------------------------------

The site is a crop site. The harvesting times are visible as sharp edges in the plots of NEE.

The micrometeorological conditions differ between the different cropping periods,
because the friction at the surface differs.
Also not the cropping periods do not correspond very well to seasons.
Hence, for the estimation of uStar-Thresholds, we apply a user-defined splitting 
of uStar-seasons. With function `usCreateSeasonFactorYdayYear` we provide the starting
points of the seasons.  

Note that, here, the seasons are not constrained within one calendaryear. 
There are other variants of a user-specified season that do respect calendaryear boundaries, 
or that let seasons start at the same day each year. 


```r
	seasonStarts <- as.data.frame( do.call( rbind, list(
		  c(70,2004)
            ,c(210,2004)
            ,c(320,2004)
            ,c(70,2005)
            ,c(180,2005)
            ,c(320,2005)
            ,c(120,2006)
            ,c(305,2006) 		
	)))
	seasonFactor <- usCreateSeasonFactorYdayYear(
	  DEGebExample$DateTime + 15*60, starts = seasonStarts)
	plot( NEE ~ DateTime, DEGebExample )
	seasonStartsDate <- fConvertTimeToPosix( data.frame(Year = seasonStarts[,2]
		, DoY = seasonStarts[,1], Hour = 0.25),'YDH'
		, Year.s = "Year", Day.s = "DoY",Hour.s = "Hour")
	abline( v = seasonStartsDate$DateTime)
```

<img src="DEGebExample_files/figure-html/DEGeb_estUStar1a-1.png" style = "display:block; margin: auto" />

The user-specific seasoning is provided to the gap-filling by the argument `seasonFactor.v`. 

```r
	(uStarTh <- EddyProc.C$sEstUstarThreshold(seasonFactor.v = seasonFactor)$uStarTh)
```

```
##    aggregationMode seasonYear  season      uStar
## 1           single         NA    <NA> 0.15788889
## 2             year       2004    <NA> 0.13766667
## 3             year       2005    <NA> 0.15788889
## 4             year       2006    <NA> 0.25094444
## 5           season       2004 2004001 0.13766667
## 6           season       2004 2004070 0.12287500
## 7           season       2004 2004210 0.08925000
## 8           season       2005 2004320 0.15788889
## 9           season       2005 2005070 0.12533333
## 10          season       2005 2005180 0.12900000
## 11          season       2006 2005320 0.04842361
## 12          season       2006 2006120 0.07390909
## 13          season       2006 2006305 0.25094444
```

```r
	# estimation can be inspected by plotting the saturation of NEE with UStar 
	# for the temperatures of one season
	#EddyProc.C$sPlotNEEVersusUStarForSeason( levels(uStarTh$season)[2] )
```

Note that there is an estimate for each season. Further, an annual estimate is obtained
by taking the maximum across the seasons, and the overall estimate is the mean across the years.

By default the gap-filling uses annually aggregated estimates of uStar-Threshold.
This usually works for sites with continuous vegetation cover.
For the crop-site of this example, we will use a different threshold for each of the defined seasons.
This is achieved by providing the seasonal estimates with argument `UstarThres.df`.
The season factor has already been stored with the class when calling `EddyProc.C$sEstUstarThreshold`. 


```r
	(UstarThres.df <- usGetSeasonalSeasonUStarMap(uStarTh))
```

```
##     season      uStar
## 5  2004001 0.13766667
## 6  2004070 0.12287500
## 7  2004210 0.08925000
## 8  2004320 0.15788889
## 9  2005070 0.12533333
## 10 2005180 0.12900000
## 11 2005320 0.04842361
## 12 2006120 0.07390909
## 13 2006305 0.25094444
```

```r
	EddyProc.C$sMDSGapFillAfterUstar(
	  'NEE', FillAll.b = FALSE, UstarThres.df = UstarThres.df, Verbose.b = FALSE)
```


Uncertainty introduced by the uStar Threshold estimate: bootstrap  
-----------------------------------------------------------------

With a lower estimate of uStar threshold, more records with lower NEE are kept in 
the dataset instead of marked as gaps. Therefore annual estimate of NEE will decrease
with lower uStar Threshold. Also the partitioning of the net-flux to GPP and Reco is 
sensitive to inclusion of data at dawn period with conditions of low uStar.

In order to quantify this uncertainty, a lower, median and upper estimates of uStar 
are obtained from a bootstrapped sample of half-hourly NEE measurements.
The Gap-Filling and computation of derived quantities such as GPP are then repeated for
different estimates of the uStar Threshold.



```r
	# here, because of speed only 30 samples instead of 100, and 10% and 90% 
	# percentile instead of 5%,50%, and 95%
	uStarRes <- EddyProc.C$sEstUstarThresholdDistribution( 
	  seasonFactor.v = seasonFactor, nSample = 30L, probs = c(0.1,0.9))
	(UstarThres.df <- usGetSeasonalSeasonUStarMap(uStarRes))
```

```
##     season      uStar        U10        U90
## 5  2004001 0.13766667 0.09421545 0.18925333
## 6  2004070 0.12287500 0.09852917 0.12300000
## 7  2004210 0.08925000 0.08879167 0.18845972
## 8  2004320 0.15788889 0.11723849 0.17302044
## 9  2005070 0.12533333 0.11859643 0.17075952
## 10 2005180 0.12900000 0.08351500 0.14246250
## 11 2005320 0.04842361 0.04836622 0.08997022
## 12 2006120 0.07390909 0.05912727 0.10045385
## 13 2006305 0.25094444 0.14389375 0.25849444
```

```r
	EddyProc.C$sMDSGapFillAfterUStarDistr(
	  'NEE', FillAll.b = FALSE, UstarThres.df = UstarThres.df)
```

Additional output columns are produced for each uStar quantile. 


```r
	grep("^NEE.*_f$", colnames( EddyProc.C$sExportResults()), value = TRUE )
```

```
## [1] "NEE_WithUstar_f" "NEE_uStar_f"     "NEE_U10_f"       "NEE_U90_f"
```
		
In order to provide results of different uStar Threshold estimates to the NEE 
Flux-partitioning, the 
argument suffix.s is used. The output columns of the Gap-Filling carry the same suffix.


```r
	EddyProc.C$sMDSGapFill('Tair', FillAll.b = FALSE)
	for (suffix in c('U10', 'U90')) {
		EddyProc.C$sMRFluxPartition(Suffix.s = suffix)
	}
	grep("U10", colnames(EddyProc.C$sExportResults()), value = TRUE) 	
```

```
##  [1] "Ustar_U10_Thres" "Ustar_U10_fqc"   "NEE_U10_orig"   
##  [4] "NEE_U10_f"       "NEE_U10_fqc"     "NEE_U10_fall"   
##  [7] "NEE_U10_fall_qc" "NEE_U10_fnum"    "NEE_U10_fsd"    
## [10] "NEE_U10_fmeth"   "NEE_U10_fwin"    "PotRad_U10"     
## [13] "FP_NEEnight_U10" "FP_Temp_U10"     "E_0_U10"        
## [16] "R_ref_U10"       "Reco_U10"        "GPP_U10_f"      
## [19] "GPP_U10_fqc"
```

Using change point detection instead of moving point method for UStar Threshold estimation
------------------------------------------------------------------------------------------

The package also provides another method of estimating the point where NEE saturates with increasing uStar.
With the ChangePointDetection (CPT) method, the data is not binned by classes of uStar but the changepoint
is estimated based on the entire subset within one seasons and one temperature class.
The user invokes this method by specifying argument `ctrlUstarEst.l = usControlUstarEst(isUsingCPTSeveralT = TRUE)`
to `EstUstarThreshold` or `sEstUstarThresholdDistribution`.

The CPT method is usually yields higher thresholds and marks more data as Gap. 
  

```r
	EddySetups.C <- sEddyProc$new(
	  'DE-Geb', DEGebExample, c('NEE','Rg','Tair','VPD', 'Ustar'))
	resUStar <- EddySetups.C$sEstUstarThreshold(
						ctrlUstarEst.l = usControlUstarEst(isUsingCPTSeveralT = TRUE)
						,seasonFactor.v = seasonFactor
				)$uStarTh
	(UstarThresCP.df <- usGetSeasonalSeasonUStarMap(resUStar))
```

```
##     season     uStar
## 5  2004001 0.5452155
## 6  2004070 0.1206278
## 7  2004210 0.2082677
## 8  2004320 0.1387257
## 9  2005070 0.1387257
## 10 2005180 0.1387257
## 11 2005320 0.3419706
## 12 2006120 0.3419706
## 13 2006305 0.3419706
```

```r
	#UstarThres.df
```


		




