#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Developers' R script to test eddy processing for various datasets +++
#+++ --> run from package directory
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM

Develop.b <- F #True if in development mode: Load individual scripts, if false test REddyProc as package
LongTest.b <- F #True if in intensive test mode including NC files, all plots and ...

if (Develop.b) {
  # Source settings for R environment and standard functions
  source('inst/setREnvir.R')
  
  # Source file and data handling scripts
  source("R/DataFunctions.R")
  source("R/FileHandling.R")
  # Initialization of sEddyProc class
  source("R/Eddy.R")
  # sEddyProc methods
  source("R/EddyGapfilling.R")
  source("R/EddyPlotting.R")
  # Source geo functions
  source("R/GeoFunctions.R")
} else {
  # Source settings for R environment and standard functions
  source('inst/setREnvir.R')
  # If needed, generate package
  # system('R CMD INSTALL --build --html --library=/Library/Frameworks/R.framework/Versions/current/Resources/library ../REddyProc')
  # system('R CMD INSTALL --build --html ../REddyProc')
  # Load REddyProc package
  if( sum(grepl("REddyProc", (.packages()))) == 1 ) detach("package:REddyProc")
  require('REddyProc')
  # Test to source data
  #   data('Example_DETha98')
  #   str(Example_DETha98) # --> problems because of unit row in header...
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Packages to source for unit tests
require(testthat)
if (Develop.b) {
  test_dir('inst/tests') # works only if R scripts are sourced
} else {
  test_package('REddyProc') # works only if loaded as package
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load standard example data from file
EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt','data')
# Add dummy quality flag for tests
EddyData.F <- cbind(EddyData.F, QF=structure(rep(c(1,0,1,0,1,0,0,0,0,0),nrow(EddyData.F)/10), units="dummy_flag"))
# Add POSIX time stamp
EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s = 'Year', Day.s = 'DoY', Hour.s = 'Hour')

# Write data to files
fWriteDataframeToFile(EddyDataWithPosix.F, 'DE-Tha-Data.txt', 'out')
fWriteDataframeToFile(EddyDataWithPosix.F, 'DE-Tha-Data.nc', 'out', 'nc')

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load other datasets

if (LongTest.b) {
  # Load MDS output data
  MDSData.F <- fLoadTXTIntoDataframe('Example_DETha98_MDSOutput_DataSetafterGapfill.txt','inst/MDSdata')
  MDSData.F <- fConvertTimeToPosix(MDSData.F, 'YMDHM', Year.s = 'Year', Month.s= 'Month', Day.s = 'DoY', Hour.s = 'Hour', Min.s = 'Minute')
  
  # Load NC file with multiple years (upload includes time conversion)
  lVar.V.s <- c('NEE', 'Rg', 'Tair', 'VPD', 'NEE_f', 'NEE_fmet', 'NEE_fwin', 'NEE_fn', 'NEE_fs', 'NEE_fqc', 'NEE_fqcOK')
  EddyNCData.F <- fLoadFluxNCIntoDataframe(lVar.V.s, 'Example_DE-Tha.1996.1998.hourly.nc','inst/MDSdata')
}

# Run loop over all (site) files in BGI Fluxnet data directory
if (T==F) { 
  SiteFile.V.s <- fInitFilesDir(DirFluxnet.s, 'hourly.nc')
  SiteName.V.s <- fStripFileExtension(SiteFile.V.s)
  for (Site.i in 1:length(SiteName.V.s)) {
    message(paste('Handling site file ', Site.i, ': \"', SiteName.V.s[Site.i],'\"', sep='')) 
    #...  
  }
  lVar.V.s <- c('NEE', 'Rg', 'Tair', 'VPD', 'NEE_f', 'NEE_fmet', 'NEE_fwin', 'NEE_fn', 'NEE_fs', 'NEE_fqc', 'NEE_fqcOK') #!!! Attention NEE_fqcOK or NEE_fqcok, both exists
  EddyBGINCData.F <- fLoadFluxNCIntoDataframe(lVar.V.s, 'DE-Tha.1996.2006.hourly.nc', DirFluxnet.s)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load data into REddyProc classes
# Standard dataset
EPTha.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE', 'QF', 'Rg', 'Tair', 'VPD'))
# Dataset with pseudo hourly data
EPThaH.C <- sEddyProc$new('DE-Tha-H', EddyDataWithPosix.F[c(F,T),], c('NEE', 'QF', 'Rg', 'Tair', 'VPD'), DTS.n=24)
# Subset of dataset
EPThaS.C <- sEddyProc$new('DE-Tha-S', EddyDataWithPosix.F[(4321:8640),], c('NEE', 'QF', 'Rg', 'Tair', 'VPD'))

# Work with (subsets of) multiple years
if (LongTest.b) {
  # All three years of data
  EPThaNC.C <- sEddyProc$new('DE-Tha', EddyNCData.F, c('NEE','Rg', 'Tair', 'VPD', 'NEE_f', 'NEE_fqc', 'NEE_fmet', 'NEE_fwin', 'NEE_fs', 'NEE_fn'))
  # Single year 1996
  EPThaNC96.C <- sEddyProc$new('DE-Tha', EddyNCData.F[1:17568,], c('NEE','Rg', 'Tair', 'VPD', 'NEE_f', 'NEE_fqc', 'NEE_fmet', 'NEE_fwin', 'NEE_fs', 'NEE_fn'))
  # Subset of half 1996 to half of 1997
  EPThaNCsub.C <- sEddyProc$new('DE-Tha', EddyNCData.F[8737:(17568+9600),], c('NEE','Rg', 'Tair', 'VPD', 'NEE_f', 'NEE_fqc', 'NEE_fmet', 'NEE_fwin', 'NEE_fs', 'NEE_fn'))
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Fill gaps with MDS algorithm

EPTha.C$sMDSGapFill('NEE','QF','0', Verbose.b=T) #system.time(...)
EPThaH.C$sMDSGapFill('NEE','QF','0', Verbose.b=T) #system.time(...)
EPThaS.C$sMDSGapFill('NEE','QF','0', Verbose.b=T) #system.time(...)

# Fill also other variables
if( LongTest.b ) {
  EPTha.C$sMDSGapFill('Rg', Verbose.b=T)
  EPTha.C$sMDSGapFill('VPD', Verbose.b=T)
  EPTha.C$sMDSGapFill('Tair', Verbose.b=T)
}

# Fill multiple years
if (LongTest.b) {
  EPThaNC.C$sMDSGapFill('NEE_f', 'NEE_fqc', 0, Verbose.b=T)
  EPThaNC96.C$sMDSGapFill('NEE_f', 'NEE_fqc', 0, Verbose.b=T)
  EPThaNCsub.C$sMDSGapFill('NEE_f', 'NEE_fqc', 0, Verbose.b=T)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Export processing results
ThaFilled.F <- EPTha.C$sExportResults()
fWriteDataframeToFile(cbind(EddyDataWithPosix.F,ThaFilled.F), 'DE-Tha-Results.txt', 'out')

if (LongTest.b) {
  ThaHFilled.F <- EPThaH.C$sExportResults()
  ThaSFilled.F <- EPThaS.C$sExportResults()
  ThaNCFilled.F <- EPThaNC.C$sExportResults()
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Test documention, e.g. information on reference class methods
# sEddyProc$help(sMDSGapFill)

#Show function code
# fix('sEddyProc.example')
# fix('fSetQF')
# fix('EPTha.C')
# fix('sEddyProc$sMDSGapFill') or fix('EPTha.C$sMDSGapFill') --> not possible to access S4 functions... :-(

# Show data in reference class
# EPTha.C$sPrintData()
# rstudio::viewData(EPTha.C$sDATA)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++]
# Plotting of single years/month

if (LongTest.b) {
  #Before filling
  EPTha.C$sPlotFingerprintY('NEE',Year.i=1998)
  EPTha.C$sPlotDailySumsY('NEE', Year.i=1998)
  EPTha.C$sPlotDiurnalCycleM('NEE', Month.i=10)
  EPTha.C$sPlotHHFluxesY('NEE',Year.i=1998)
  
  EPThaH.C$sPlotFingerprintY('NEE',Year.i=1998)
  EPThaH.C$sPlotDailySumsY('NEE', Year.i=1998)
  EPThaH.C$sPlotDiurnalCycleM('NEE', Month.i=10)
  EPThaH.C$sPlotHHFluxesY('NEE',Year.i=1998)
  
  EPThaS.C$sPlotFingerprintY('NEE',Year.i=1998)
  EPThaS.C$sPlotDailySumsY('NEE', Year.i=1998)
  EPThaS.C$sPlotDiurnalCycleM('NEE', Month.i=6)
  EPThaS.C$sPlotHHFluxesY('NEE',Year.i=1998)
  
  EPThaNCsub.C$sPlotFingerprintY('NEE',Year.i=1996)
  EPThaNCsub.C$sPlotHHFluxesY('NEE',Year.i=1996)
  EPThaNCsub.C$sPlotDiurnalCycleM('NEE', Month.i=6)
  EPThaNCsub.C$sPlotDailySumsY('NEE',Year.i=1996)
  
  EPThaNCsub.C$sPlotFingerprintY('NEE',Year.i=1997)
  EPThaNCsub.C$sPlotHHFluxesY('NEE',Year.i=1997)
  EPThaNCsub.C$sPlotDailySumsY('NEE',Year.i=1997)
  EPThaNCsub.C$sPlotDailySumsY('NEE',Year.i=1998)
  
  #After filling
  EPTha.C$sPlotFingerprintY('NEE_f','NEE_fqc', 1, 1998)
  EPTha.C$sPlotDailySumsY('NEE_f', 'NEE_fsd', 1998)
  EPTha.C$sPlotDiurnalCycleM('NEE','none', NA, 10)
  EPTha.C$sPlotHHFluxesY('NEE_f','NEE_fqc', 1, 1998)
  
  EPThaS.C$sPlotFingerprintY('NEE_f','NEE_fqc', 1, 1998)
  EPThaS.C$sPlotDailySumsY('NEE_f', 'NEE_fsd', 1998)
  EPThaS.C$sPlotDiurnalCycleM('NEE','none', NA, 10)
  EPThaS.C$sPlotHHFluxesY('NEE_f','NEE_fqc', 1, 1998)
  
  EPThaH.C$sPlotFingerprintY('NEE_f','NEE_fqc', 1, 1998)
  EPThaH.C$sPlotDailySumsY('NEE_f', 'NEE_fsd', 1998)
  EPThaH.C$sPlotDiurnalCycleM('NEE','none', NA, 10)
  EPThaH.C$sPlotHHFluxesY('NEE_f','NEE_fqc', 1, 1998)
  
  EPThaH.C$sPlotFingerprintY('NEE_f','NEE_fqc', 1, 1998)
  EPThaH.C$sPlotDailySumsY('NEE_f', 'NEE_fsd', 1998)
  EPThaH.C$sPlotDiurnalCycleM('NEE','none', NA, 10)
  EPThaH.C$sPlotHHFluxesY('NEE_f','NEE_fqc', 1, 1998)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Plotting of standard example, unfilled and filled columns
EPTha.C$sPlotFingerprint('NEE')
EPTha.C$sPlotFingerprint('NEE_f')
EPTha.C$sPlotFingerprint('NEE_f','NEE_fqc',1)

EPTha.C$sPlotDiurnalCycle('NEE')
EPTha.C$sPlotDiurnalCycle('NEE_f')
EPTha.C$sPlotDiurnalCycle('NEE_f','NEE_fqc',1)

EPTha.C$sPlotHHFluxes('NEE')
EPTha.C$sPlotHHFluxes('NEE_f')
EPTha.C$sPlotHHFluxes('NEE_f','NEE_fqc',1)

EPTha.C$sPlotDailySums('NEE')
EPTha.C$sPlotDailySums('NEE_f')
EPTha.C$sPlotDailySums('NEE_f','NEE_fsd')

# Images for multiple year data
if (LongTest.b) {
  # Multiple year plots with leap year
  EPThaNC.C$sPlotFingerprint('NEE')
  EPThaNC96.C$sPlotFingerprint('NEE')
  EPThaNCsub.C$sPlotFingerprint('NEE')
  EPThaNC.C$sPlotFingerprint('NEE_f')
  EPThaNC.C$sPlotFingerprint('NEE_f','NEE_fqc',1)
  
  EPThaNC.C$sPlotDiurnalCycle('NEE')
  EPThaNC96.C$sPlotDiurnalCycle('NEE')
  EPThaNCsub.C$sPlotDiurnalCycle('NEE')
  EPThaNC.C$sPlotDiurnalCycle('NEE_f')
  EPThaNC.C$sPlotDiurnalCycle('NEE_f','NEE_fqc',1)
  
  EPThaNC.C$sPlotHHFluxes('NEE')
  EPThaNC96.C$sPlotHHFluxes('NEE')
  EPThaNCsub.C$sPlotHHFluxes('NEE')
  EPThaNC.C$sPlotHHFluxes('NEE_f')
  EPThaNC.C$sPlotHHFluxes('NEE_f','NEE_fqc',1)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Re-set class on previously filled data for plotting, e.g. to avoid rerunning MDS each time...
if (LongTest.b) { 
  EPTha.C <- sEddyProc$new('DE-Tha', ThaFilled.F, c('NEE','Rg', 'Tair', 'VPD', 'QF', 'NEE_f', 'NEE_fqc', 'NEE_fmeth', 'NEE_fwin', 'NEE_fsd', 'NEE_fnum'))
  EPThaC.C$sPlotDailySums('NEE_f_f','NEE_f_fsd')
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Compare data of old MDS filling with new filling
if (LongTest.b) { 
  # Rename 'NEE' to have different column names for old and new processing (though in different data frames: original data in sDATA and results in sTEMP)
  EddyNCData.F <- cbind(EddyNCData.F, NEEnew = EddyNCData.F$NEE)
  EPThaNC.C <- sEddyProc$new('DE-Tha', EddyNCData.F, c('NEEnew', 'Rg', 'Tair', 'VPD', 'NEE_f', 'NEE_fqc', 'NEE_fmet', 'NEE_fwin', 'NEE_fs', 'NEE_fn'))
  # Fill gaps
  EPThaNC.C$sMDSGapFill('NEEnew', 'NEE_fqc', 0, Verbose.b=T)
  
  # Plot difference between old and new MDS
  plot(EPThaNC.C$sTEMP$NEEnew_f ~ EPThaNC.C$sDATA$NEE_f)

  # Usual plots...
  EPThaNC.C$sPlotHHFluxes('NEEnew_f')
  EPThaNC.C$sPlotHHFluxes('NEE_f')
  EPThaNC.C$sPlotDiurnalCycle('NEEnew_f')
  EPThaNC.C$sPlotDiurnalCycle('NEE_f')
  EPThaNC.C$sPlotDailySums('NEEnew_f', 'NEEnew_fsd')
  EPThaNC.C$sPlotDailySums('NEE_f', 'NEE_fs')
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Other tests, here unit attributes
if (T==F) {
  
  t1 <- data.frame(a=c(1,2,3), b=c(4,5,6))
  attr(t1$b, "units") <- 'b_u'
  attr(t1$b, "units") 
  attr(t1[[2]], "units")
  attr(t1[[1,2]], "units") ##does not work...
  attributes(t1[,'b'])
  attributes(t1[['b']])
  attributes(t1$b)
  
  t2 <- list(t1)
  attr(t2[[1]]$b, "units")
  attr(t2[[1]][['b']], "units")
  attributes(t2[[1]][['b']])
  
  t3 <- data.frame(t2)
  attr(t3$b, "units") #works!
  
  t22 <- c(t1)
  attr(t22$b, "units")
  
  t33 <- data.frame(t22)
  attr(t33$b, "units") #works!
}