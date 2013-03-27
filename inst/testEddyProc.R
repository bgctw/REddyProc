#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script to test a Eddy Processing run +++
#+++ Run from package directory
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM

Develop.b <- F #True if in development mode: Load individual scripts, if false test REddyProc as package
LongTest.b <- F #True if in intensive test mode including NC files, all plots and ...

if (Develop.b) {
  # Source settings for R environment and standard functions
  source('inst/setREnvir.R')
  #Source file and data handling scripts
  source("R/DataFunctions.R")
  source("R/FileHandling.R")
  # Initialization of sEddyProc class
  source("R/Eddy.R")
  # sEddyProc methods
  source("R/EddyGapfilling.R")
  source("R/EddyPlotting.R")
} else {
  # Source settings for R environment and standard functions
  source('inst/setREnvir.R')
  # Load REddyProc package
  require('REddyProc')
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

# Load data from files
EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt','data') #File upload
EddyData.F <- cbind(EddyData.F, QF=rep(c(1,0,1,0,1,0,0,0,0,0),nrow(EddyData.F)/10))
EddyData.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s = 'Year', Day.s = 'Day', Hour.s = 'Hour')
if (LongTest.b) { #Example code, do not always execute (e.g. on package installation)
  MDSData.F <- fLoadTXTIntoDataframe('Example_DETha98_MDSOutput_DataSetafterGapfill.txt','inst/MDSdata')
  MDSData.F <- fConvertTimeToPosix(MDSData.F, 'YMDHM', Year.s = 'Year', Month.s= 'Month', Day.s = 'Day', Hour.s = 'Hour', Min.s = 'Minute')
  # NC file upload includes time conversion 
  lVar.V.s <- c('NEE', 'Rg', 'Tair', 'VPD', 'NEE_f', 'NEE_fmet', 'NEE_fwin', 'NEE_fn', 'NEE_fs', 'NEE_fqc', 'NEE_fqcOK') #!!! Attention NEE_fqcOK or NEE_fqcok, both exists
  EddyNCData.F <- fLoadFluxNCIntoDataframe(lVar.V.s, 'Example_DE-Tha.1996.1998.hourly.nc','inst/MDSdata')
  #EddyNCData.F <- fLoadFluxNCIntoDataframe(lVar.V.s, 'DE-Tha.1996.2006.hourly.nc', DirFluxnet.s)
}

# Write data to files
if (LongTest.b) { #Example code, do not always execute (e.g. on package installation)
  fWriteDataframeToFile(EddyData.F, 'OutputTest', 'out')
  fWriteDataframeToFile(EddyData.F, 'OutputTest', 'out', 'nc')
}

# Run loop over all (site) files in BGI Fluxnet data directory
if (LongTest.b) { #Example code, do not always execute (e.g. on package installation)
  SiteFile.V.s <- fInitFilesDir(DirFluxnet.s, 'hourly.nc')
  SiteName.V.s <- fStripFileExtension(SiteFile.V.s)
  for (Site.i in 1:length(SiteName.V.s)) {
    message(paste('Handling site file ', Site.i, ': \"', SiteName.V.s[Site.i],'\"', sep=''))
  }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Show information on reference class methods
sEddyProc$help(sMDSGapFill)

# Load data into reference class
EPTha.C <- sEddyProc$new('DE-Tha', EddyData.F, c('NEE','Rg', 'Tair', 'VPD','QF'))

# Fill gaps with MDS algorithm
EPTha.C$sMDSGapFill('NEE','QF','0', Verbose.b=T) #system.time(...)
if( LongTest.b ) {
  EPTha.C$sMDSGapFill('Rg', Verbose.b=T)
  EPTha.C$sMDSGapFill('VPD', Verbose.b=T)
  EPTha.C$sMDSGapFill('Tair', Verbose.b=T)
}
ThaFilled.F <- EPTha.C$sExportData()

# Test other methods of reference class
if (LongTest.b)
  EPTha.C$sPrintData()
# rstudio::viewData(EPTha.C$sDATA)

# Work with multiple years
if (LongTest.b) {
  EPThaNC.C <- sEddyProc$new('DE-Tha', EddyNCData.F, c('NEE','Rg', 'Tair', 'VPD', 'NEE_f', 'NEE_fqc', 'NEE_fmet', 'NEE_fwin', 'NEE_fs', 'NEE_fn'))
  EPThaNC.C$sMDSGapFill('NEE_f', 'NEE_fqc', 0, Verbose.b=T)
  ThaNCFilled.F <- EPThaNC.C$sExportData()
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Re-set class on previously filled data to avoid rerunning MDS each time...
if (FALSE) { 
  EPTha.C <- sEddyProc$new('DE-Tha', ThaFilled.F, c('NEE','Rg', 'Tair', 'VPD', 'QF', 'NEE_f', 'NEE_fqc', 'NEE_fmeth', 'NEE_fwin', 'NEE_fsd', 'NEE_fnum'))
  EPThaNC.C <- sEddyProc$new('DE-Tha', ThaNCFilled.F, c('NEE','Rg', 'Tair', 'VPD', 'NEE_f', 'NEE_fqc', 'NEE_fmet', 'NEE_fwin', 'NEE_fs', 'NEE_fn',
                                                        'NEE_f_f', 'NEE_f_fqc', 'NEE_f_fmeth', 'NEE_f_fwin', 'NEE_f_fsd', 'NEE_f_fnum'))
}

# Plotting of single years/month
if (LongTest.b) {
  EPTha.C$sPlotDailySumsY('NEE_f', 'NEE_fsd', 1998)
  EPThaNC.C$sPlotDailySumsY('NEE_f', 'NEE_fs', 1998)
  EPTha.C$sPlotHHMeansM('NEE','none', NA, 10)
  EPTha.C$sPlotHHFluxesY('NEE_f','NEE_fqc',1, 1998)
  EPTha.C$sPlotDailySumsY('NEE_f','NEE_fsd', 1998)
  EPThaNC.C$sPlotDailySumsY('NEE_f','NEE_fs', 1998)
}

# Images for single year data
EPTha.C$sPlotFingerprint('NEE')
EPTha.C$sPlotFingerprint('NEE_f')
EPTha.C$sPlotFingerprint('NEE_f','NEE_fqc',1)
EPTha.C$sPlotHHMeans('NEE')
EPTha.C$sPlotHHMeans('NEE_f')
EPTha.C$sPlotHHMeans('NEE_f','NEE_fqc',1)
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
  EPThaNC.C$sPlotFingerprint('NEE_f')
  EPThaNC.C$sPlotFingerprint('NEE_f','NEE_fqc',1)
#  EPThaNC.C$sPlotFingerprint('NEE_f_f')
#  EPThaNC.C$sPlotFingerprint('NEE_f_f','NEE_fqc',1)
  
  
  EPThaNC.C$sPlotHHMeans('NEE')
  EPThaNC.C$sPlotHHMeans('NEE_f')
  EPThaNC.C$sPlotHHMeans('NEE_f','NEE_fqc',1)
#  EPThaNC.C$sPlotHHMeans('NEE_f_f')
#  EPThaNC.C$sPlotHHMeans('NEE_f_f','NEE_fqc',1)
  
  EPThaNC.C$sPlotHHFluxes('NEE')
  EPThaNC.C$sPlotHHFluxes('NEE_f')
  EPThaNC.C$sPlotHHFluxes('NEE_f','NEE_fqc',1)
#  EPThaNC.C$sPlotHHFluxes('NEE_f_f')
#  EPThaNC.C$sPlotHHFluxes('NEE_f_f','NEE_fqc_f',1)
  
  EPThaNC.C$sPlotDailySums('NEE')
  EPThaNC.C$sPlotDailySums('NEE_f')
  EPThaNC.C$sPlotDailySums('NEE_f','NEE_fs')
#  EPThaNC.C$sPlotDailySums('NEE_f_f')
#  EPThaNC.C$sPlotDailySums('NEE_f_f','NEE_f_fsd')
}
