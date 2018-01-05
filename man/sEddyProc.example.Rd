\name{sEddyProc.example}
\alias{sEddyProc.example}
\title{sEddyProc - Example code}
\description{Dummy function to show code example for sEddyProc provided below}
\usage{sEddyProc.example()}



\author{AMM
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}




\examples{
#+++ Simple example code for using the sEddyProc reference class +++
donttest({#Do not always execute example code (e.g. on package installation)
# library(REddyProc) # user should load the package before executing any example

#+++ Load data with one header and one unit row from (tab-delimited) text file
examplePath <- getExamplePath('Example_DETha98.txt')
if (length(examplePath)) {
  EddyData.F <- fLoadTXTIntoDataframe(examplePath)
} else {
  warning(
      "Could not find example text data file."
      , " In order to execute this example code, "
      , " please, allow downloading it from github. "
      , " Type '?getExamplePath' for more information."
      , " For now the RData version provided with the package is used.")
  #data(Example_DETha98)
  EddyData.F <- Example_DETha98
}

#+++ If not provided, calculate VPD from Tair and rH
EddyData.F$VPD  <- fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair)

#+++ Add time stamp in POSIX time format
EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s = 'Year'
  , Day.s = 'DoY', Hour.s = 'Hour')

#+++ Initalize R5 reference class sEddyProc for processing of eddy data
#+++ with all variables needed for processing later
EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F
        , c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
#set Location of DE-Tharandt
EddyProc.C$sSetLocationInfo(Lat_deg.n = 51.0, Long_deg.n = 13.6, TimeZone_h.n = 1)

#+++ Generate plots of all data in directory \plots (of current R working dir)
EddyProc.C$sPlotHHFluxes('NEE')
EddyProc.C$sPlotFingerprint('Rg')
EddyProc.C$sPlotDiurnalCycle('Tair')
#+++ Plot individual months / years to screen (of current R graphics device)
EddyProc.C$sPlotHHFluxesY('NEE', Year.i = 1998)
EddyProc.C$sPlotFingerprintY('NEE', Year.i = 1998)

#+++ Fill gaps with MDS gap filling algorithm (without prior ustar filtering)
#Fill all values to estimate flux uncertainties
EddyProc.C$sMDSGapFill('NEE', FillAll.b = TRUE)
#Fill only the gaps for the meteo condition, e.g. 'Rg'
EddyProc.C$sMDSGapFill('Rg', FillAll.b = FALSE)

#+++ Example plots of filled data to screen or to directory \plots
EddyProc.C$sPlotFingerprintY('NEE_f', Year.i = 1998)
#Plot of sums with uncertainties
EddyProc.C$sPlotDailySumsY('NEE_f', 'NEE_fsd', Year.i = 1998)
EddyProc.C$sPlotDailySums('NEE_f', 'NEE_fsd')

#+++ Partition NEE into GPP and respiration
# Gap-filled Tair (and NEE) needed for partitioning
EddyProc.C$sMDSGapFill('Tair', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('VPD', FillAll.b = FALSE)
EddyProc.C$sMRFluxPartition()  # night time partitioning -> Reco, GPP
EddyProc.C$sGLFluxPartition()  # day time partitioning -> Reco_DT, GPP_DT
#plot(EddyProc.C$sTEMP$GPP_DT ~ EddyProc.C$sTEMP$GPP_f); abline(0, 1)
#plot(-EddyProc.C$sTEMP$GPP_DT + EddyProc.C$sTEMP$Reco_DT ~ EddyProc.C$sTEMP$NEE_f)
# abline(0, 1)
#names(EddyProc.C$sTEMP)
# there are some constraints, that might be too strict for some datasets
# e.g. in the tropics the required temperature range might be too large.
# Its possible to change these constraints
#EddyProc.C$sMRFluxPartition(parsE0Regression = list(TempRange.n = 2.0, optimAlgorithm = "LM"))

#+++ Example plots of calculated GPP and respiration
EddyProc.C$sPlotFingerprintY('GPP_f', Year.i = 1998)
EddyProc.C$sPlotFingerprint('GPP_f')
EddyProc.C$sPlotHHFluxesY('Reco', Year.i = 1998)
EddyProc.C$sPlotHHFluxes('Reco')

#+++ Processing with ustar filtering before
EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F
        , c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
EddyProc.C$sSetLocationInfo(Lat_deg.n = 51.0, Long_deg.n = 13.6, TimeZone_h.n = 1)

# estimating the thresholds based on the data
(uStarTh <- EddyProc.C$sEstUstarThreshold()$uStarTh)
# plot saturation of NEE with UStar for one season
EddyProc.C$sPlotNEEVersusUStarForSeason(levels(uStarTh$season)[3] )
# Gapfilling by default it takes the annually aggregated estimate is used to
# mark periods with low uStar. For other options see vignette DEGebExample
EddyProc.C$sMDSGapFillAfterUstar('NEE')
colnames(EddyProc.C$sExportResults()) # Note the collumns with suffix _WithUstar
EddyProc.C$sMDSGapFill('Tair', FillAll.b = FALSE)
EddyProc.C$sMRFluxPartition(Suffix.s = 'WithUstar')  # Note suffix

#+++ Export gap filled and partitioned data to standard data frame
FilledEddyData.F <- EddyProc.C$sExportResults()
#tmp <- EddyProc.C$sExportResults(isListColumnsExported = TRUE); str(head(tmp))
#+++ Save results into (tab-delimited) text file in directory \out
CombinedData.F <- cbind(EddyData.F, FilledEddyData.F)
#+++ May rename variables to correspond to Ameriflux
colnames(CombinedDataAmeriflux.F <- renameVariablesInDataframe(
        CombinedData.F, getBGC05ToAmerifluxVariableNameMapping() ))
CombinedDataAmeriflux.F$TIMESTAMP_END <- POSIXctToBerkeleyJulianDate(
    EddyProc.C$sExportData()[[1]] )
head(tmp <- BerkeleyJulianDateToPOSIXct(CombinedDataAmeriflux.F$TIMESTAMP_END))
fWriteDataframeToFile(CombinedData.F, 'DE-Tha-Results.txt', 'out')

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Example 1 for advanced users: Processing different setups on the same site data
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++ Initialize new sEddyProc processing class
EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F,
    c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
EddyProc.C$sSetLocationInfo(Lat_deg.n = 51.0, Long_deg.n = 13.6, TimeZone_h.n = 1)

#+++ When running several processing setup, a string suffix declaration is needed
#+++ Here: Gap filling with and without ustar threshold
EddyProc.C$sMDSGapFill('NEE', FillAll.b = FALSE, Suffix.s = 'NoUstar')
EddyProc.C$sMDSGapFillAfterUstar('NEE', FillAll.b = FALSE
  , UstarThres.df = 0.3, UstarSuffix.s = 'Thres1')
EddyProc.C$sMDSGapFillAfterUstar('NEE', FillAll.b = FALSE
  , UstarThres.df = 0.4, UstarSuffix.s = 'Thres2')
# Gap-filled Tair, VPD, and Rg needed for partitioning
EddyProc.C$sMDSGapFill('Tair', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('VPD', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('Rg', FillAll.b = FALSE)
colnames(EddyProc.C$sExportResults()) # Note the suffix in output columns

#+++ Flux partitioning of the different gap filling setups
EddyProc.C$sMRFluxPartition(Suffix.s = 'NoUstar')
EddyProc.C$sMRFluxPartition(Suffix.s = 'Thres1')
EddyProc.C$sMRFluxPartition(Suffix.s = 'Thres2')
#EddyProc.C$sGLFluxPartition(Suffix.s = 'NoUstar')
grep("GPP. * $|Reco", names(EddyProc.C$sExportResults()), value = TRUE)
# Note the suffix in output columns also of GPP, Reco, GPP_DT, and Reco_DT

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Example 1b: Processing with different quantiles of uStar threshold distribution
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++ Initialize new sEddyProc processing class
EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F
        , c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
EddyProc.C$sSetLocationInfo(Lat_deg.n = 51.0, Long_deg.n = 13.6, TimeZone_h.n = 1)
EddyProc.C$sMDSGapFill('Tair', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('VPD', FillAll.b = FALSE)
EddyProc.C$sMDSGapFill('Rg', FillAll.b = FALSE)

#+++ Estimate the distribution of uStar by bootstrapping the data
uStarTh <- EddyProc.C$sEstUstarThresholdDistribution(nSample = 100L
  , probs = c(0.05, 0.5, 0.95))
print(uStarTh)
# for using seasonal threshold, see vignette DEGebExample
uStarThAnnual <- usGetAnnualSeasonUStarMap(uStarTh)[-2]
# as in example 1, we use suffixes to distinguish the setups of different quantiles
uStarSuffixes <- colnames(uStarThAnnual)[-1]

#+++ Run the gapfilling for each of the quantiles (specified by arguments probs above)
EddyProc.C$sMDSGapFillAfterUStarDistr('NEE',
    UstarThres.df = uStarThAnnual,
    UstarSuffix.V.s = uStarSuffixes,
    FillAll = TRUE
)
# a look at the diffrent output column names:
grep("NEE_.*_f$", names(EddyProc.C$sExportResults()), value = TRUE)

#+++ Run the partitioning on each of the gap-filled NEE columns
for (suffix in uStarSuffixes) {
  EddyProc.C$sMRFluxPartition(Suffix.s = suffix)
}
# a look at the diffrent output column names:
grep("GPP.*_f$|Reco", names(EddyProc.C$sExportResults()), value = TRUE)

#+++ Aggregate to annual values with neglecting covariances and missing values
dsRes <- EddyProc.C$sExportResults()
GPPAgg <- sapply(uStarSuffixes, function(suffix) {
  GPPHalfHour <- dsRes[, paste0("GPP_", suffix, "_f")]
  mean(GPPHalfHour, na.rm = TRUE)
})
print(GPPAgg)
# The difference is a first estimate of uncertainty in GPP due
# to uncertaint uStar threshold
(max(GPPAgg) - min(GPPAgg)) / median(GPPAgg)
# here a relative error of about 2 to 4 percent (differs by runs with different
# uStar sample)
# for a better but time consuming uncertainty estimate, specify a larger sample
# in uStar estimation above:
# sEstUstarThresholdDistribution(nSample = 200, probs = seq(0.025, 0.975, length.out = 39) )
# and run statistics across the larger sample of computed GPP-scenarios

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Example 2 for advanced users: Extended usage of the gap filling algorithm
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++ Add some (non-sense) example vectors:
#+++ Quality flag vector (e.g. from applying ustar filter)
QF.V.n <- rep(c(1, 0, 1, 0, 1, 0, 0, 0, 0, 0), nrow(EddyData.F) / 10)
#+++ Dummy step function vector to simulate e.g. high / low water table
Step.V.n <- ifelse(EddyData.F$DoY < 200 | EddyData.F$DoY > 250, 0, 1)

#+++ Initialize new sEddyProc processing class with more columns
EddyTest.C <- sEddyProc$new('DE-Tha', cbind(EddyDataWithPosix.F, Step = Step.V.n
  , QF = QF.V.n), c('NEE', 'LE', 'H', 'Rg', 'Tair', 'Tsoil', 'rH', 'VPD', 'QF', 'Step'))

#+++ Gap fill variable with (non-default) variables and limits
# including preselection of data with quality flag QF == 0
EddyTest.C$sMDSGapFill('LE', QFVar.s = 'QF', QFValue.n = 0, V1.s = 'Rg'
  , T1.n = 30, V2.s = 'Tsoil', T2.n = 2, 'Step', 0.1)

#+++ Use individual gap filling subroutines with different window sizes
# and up to five variables and limits
EddyTest.C$sFillInit('NEE') #Initialize 'NEE' as variable to fill
Result_Step1.F <- EddyTest.C$sFillLUT(3, 'Rg', 50, 'rH', 30, 'Tair', 2.5
  , 'Tsoil', 2, 'Step', 0.5)
Result_Step2.F <- EddyTest.C$sFillLUT(6, 'Tair', 2.5, 'VPD', 3, 'Step', 0.5)
Result_Step3.F <- EddyTest.C$sFillMDC(3)
#Individual fill result columns are called 'VAR_...'
EddyTest.C$sPlotHHFluxesY('VAR_fall', Year.i = 1998)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Example 3 advanced use: Explicit demonstration of MDS algorithm for NEE gap filling
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++ Initialize new sEddyProc processing class
EddyTestMDS.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F
        , c('NEE', 'Rg', 'Tair', 'VPD'))
#Initialize 'NEE' as variable to fill
EddyTestMDS.C$sFillInit('NEE')
# Set variables and tolerance intervals
# twutz: \u00B1 is the unicode for ' + over-'
V1.s = 'Rg'; T1.n = 50 # Global radiation 'Rg' within \u00B150 W m-2
V2.s = 'VPD'; T2.n = 5 # Vapour pressure deficit 'VPD' within 5 hPa
V3.s = 'Tair'; T3.n = 2.5 # Air temperature 'Tair' within \u00B12.5 degC
# Step 1: Look-up table with window size \u00B17 days
Result_Step1.F <- EddyTestMDS.C$sFillLUT(7, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n)
# Step 2: Look-up table with window size \u00B114 days
Result_Step2.F <- EddyTestMDS.C$sFillLUT(14, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n)
# Step 3: Look-up table with window size \u00B17 days, Rg only
Result_Step3.F <- EddyTestMDS.C$sFillLUT(7, V1.s, T1.n)
# Step 4: Mean diurnal course with window size 0 (same day)
Result_Step4.F <- EddyTestMDS.C$sFillMDC(0)
# Step 5: Mean diurnal course with window size \u00B11, \u00B12 days
Result_Step5a.F <- EddyTestMDS.C$sFillMDC(1)
Result_Step5b.F <- EddyTestMDS.C$sFillMDC(2)
# Step 6: Look-up table with window size \u00B121, \u00B128, ..., \u00B170
for (WinDays.i in seq(21, 70, 7) ) Result_Step6.F <-
      EddyTestMDS.C$sFillLUT(WinDays.i, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n)
# Step 7: Look-up table with window size \u00B114, \u00B121, ..., \u00B170, Rg only
for (WinDays.i in seq(14, 70, 7) ) Result_Step7.F <-
      EddyTestMDS.C$sFillLUT(WinDays.i, V1.s, T1.n)
# Step 8: Mean diurnal course with window size \u00B17, \u00B114, ..., \u00B1210 days
for (WinDays.i in seq(7, 210, 7) ) Result_Step8.F <- EddyTestMDS.C$sFillMDC(WinDays.i)
# Export results, columns are named 'VAR_'
FilledEddyData.F <- EddyTestMDS.C$sExportResults()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Example 4 for advanced users: Processing with user-defined uStar threshold
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++ Provide a single user-defined uStarThreshold
EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F
        , c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
Ustar.V.n <- 0.46
EddyProc.C$sMDSGapFillAfterUstar('NEE', UstarThres.df = Ustar.V.n)
grep("NEE_.*_f$", names(EddyProc.C$sExportResults()), value = TRUE)

# See vignette DEGebExample for
#  - using tailored seasons of differing uStar dynamics with vegetation changes (crop)
#  - using seasonal instead of annual uStar threshold estimates in gapfilling
#  - Bootstrapping uncertainty associated with uStar Threshold estimation
#  - Using change point detection instead of moving point method
}) # donttest()
}
