#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Developers' R script to test eddy processing for various datasets +++
#+++ --> run from package directory
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM

Develop.b <- T #True if in development mode: Load individual scripts, if false test REddyProc as package
ShortTest.b <- T #Short test only
LongTest.b <- F #True if in intensive test mode including NC files, all plots and ...

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load package or source files (development mode)

if (Develop.b) {
  # Source settings for R environment and standard functions
  source('inst/develop/setREnvir.R')
  
  # Source file and data handling scripts
  source('R/DataFunctions.R')
  source('R/FileHandling.R')
  # Initialization of sEddyProc class
  source('R/Eddy.R')
  # sEddyProc methods
  source('R/EddyGapfilling.R')
  source('R/EddyPlotting.R')
  # Source geo functions
  source('R/GeoFunctions.R')
  
} else {
  # Source settings for R environment and standard functions
  source('inst/develop/setREnvir.R')
  # Generate package from local path
  ###   system('R CMD INSTALL --build --html --library=/Library/Frameworks/R.framework/Versions/current/Resources/library ../REddyProc')
  ###   system('R CMD INSTALL --build --html ../REddyProc')
  # Test installation of package from various sources
  # Requires restart of R console afterwards!
  ### R-Forge:  install.packages("REddyProc", repos="http://R-Forge.R-project.org", type="source")
  ### R-Forge after manual download:  install.packages('../_FromRForge/REddyProc_0.5.tar.gz',  repos = NULL)
  ### Self generated package:  install.packages('REddyProc_0.5.tgz',  repos = NULL)
  require('REddyProc')
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Load example data directly from package or (if not available) from txt file

data('Example_DETha98', package='REddyProc')
if( sum(grepl('EddyData.F',ls())) == 0 ) {
  if( file.exists('../examples/Example_DETha98.txt') ) {
    EddyData.F <- suppressMessages(fLoadTXTIntoDataframe('Example_DETha98.txt','../examples'))
  } else {
    message('Unit test directory: ', getwd())
    message('Workspace: ', ls())
    stop('test_sEddyProc.R::: Example data could not be loaded.')
  }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Run short test

if( ShortTest.b ) {
  EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s = 'Year', Day.s = 'DoY', Hour.s = 'Hour')
  EPTha.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE', 'Rg', 'Tair', 'VPD'))
  EPTha.C$sMDSGapFill('NEE')
  View(EPTha.C$sTEMP)
  stop('No error but short test only.')
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Run unit tests

require(testthat) #Packages to source for unit tests
if (Develop.b) {
  test_dir('inst/tests') # works only if R scripts are sourced
} else {
  test_package('REddyProc') # works only if package is installed (not necessarily loaded)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data handling of standard example data

# Add dummy quality flag for tests
EddyData.F <- cbind(EddyData.F, QF=structure(rep(c(1,0,1,0,1,0,0,0,0,0),nrow(EddyData.F)/10), units='dummy_unit'))
# Test calculation of VPD
EddyData.F$VPDnew <- fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair)
# Add POSIX time stamp
EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s = 'Year', Day.s = 'DoY', Hour.s = 'Hour')

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load netcdf datasets in BGI format

if (LongTest.b) {
  # Load NC file with multiple years (upload includes time conversion)
  lVar.V.s <- c('NEE', 'Rg', 'Tair', 'VPD', 'NEE_f', 'NEE_fmet', 'NEE_fwin', 'NEE_fn', 'NEE_fs', 'NEE_fqc', 'NEE_fqcOK') #!!! Attention NEE_fqcOK or NEE_fqcok, both exists
  EddyNCData.F <- fLoadFluxNCIntoDataframe(lVar.V.s, 'Example_DE-Tha.1996.1998.hourly.nc','inst/examples')
}

# Run loop over all (site) files in BGI Fluxnet data directory
if (T==F) { 
  SiteFile.V.s <- fInitFilesDir(DirFluxnet.s, 'hourly.nc')
  SiteName.V.s <- fStripFileExtension(SiteFile.V.s)
  for (Site.i in 1:length(SiteName.V.s)) {
    message(paste('Handling site file ', Site.i, ': \'', SiteName.V.s[Site.i],'\'', sep='')) 
    #...  
  }
  EddyBGINCData.F <- fLoadFluxNCIntoDataframe(lVar.V.s, 'DE-Tha.1996.2006.hourly.nc', DirFluxnet.s)
  # EddyBGINCData.F <- fLoadFluxNCIntoDataframe(lVar.V.s, 'DE-Tha.1996.2006.hourly.nc', DirFluxnet.s,'RNetCDF')
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Write data to files
if (LongTest.b) {
  fWriteDataframeToFile(EddyDataWithPosix.F, 'DE-Tha-Data.txt', 'out')
}

#Produce new ascii test files from BGI netcdf fluxnet files
if( FALSE ) {
  lVar2.V.s <- c('NEE', 'LE', 'H', 'Rg', 'VPD', 'rH', 'Tair', 'Tsoil_f', 'julday')
  Example.F <- fLoadFluxNCIntoDataframe(lVar2.V.s, 'Example_DE-Tha.1996.1998.hourly.nc','inst/examples')
  #Example.F <- fLoadFluxNCIntoDataframe(lVar2.V.s, 'Example_DE-Tha.1996.1998.hourly.nc','inst/examples', 'RNetCDF')
  Example.F$Year  <- as.numeric(format(Example.F$DateTime, '%Y'))
  Example.F$Month <- as.numeric(format(Example.F$DateTime, '%m'))
  Example.F$DoY   <- as.numeric(format(Example.F$DateTime, '%j'))
  Example.F$Hour  <- as.numeric(format(Example.F$DateTime, '%H')) + as.numeric(format(Example.F$DateTime, '%M'))/60
  colnames(Example.F)[colnames(Example.F)=='Tsoil_f']  <- 'Tsoil'
  fWriteDataframeToFile(Example.F, 'DE-Tha.1996.1998.txt','out')
  
  # Try to reload data file
  Eddy3Years.F <- fLoadTXTIntoDataframe('DE-Tha.1996.1998.txt','out')
  Eddy3Years.F <- fConvertTimeToPosix(Eddy3Years.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
  #fCheckHHTimeSeries(Eddy3Years.F$DateTime, DTS.n=48)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data into REddyProc class instances

# Standard dataset
EPTha.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE', 'QF', 'Rg', 'Tair', 'VPD'))
# Dataset with pseudo hourly data
EPThaH.C <- sEddyProc$new('DE-Tha-H', EddyDataWithPosix.F[c(F,T),], c('NEE', 'QF', 'Rg', 'Tair', 'VPD'), DTS.n=24)
# Subset of dataset
EPThaS.C <- sEddyProc$new('DE-Tha-S', EddyDataWithPosix.F[(4321:8640),], c('NEE', 'QF', 'Rg', 'Tair', 'VPD'))
# Limited dataset with NEE only
EPThaL1.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE'))
EPThaL2.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE', 'Rg'))
EPThaL3.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE', 'Rg', 'Tair'))

# Work with (subsets of) multiple years
if (LongTest.b) {
  # All three years of data
  EPThaNC.C <- sEddyProc$new('DE-Tha', EddyNCData.F, lVar.V.s)
  # Single year 1996
  EPThaNC96.C <- sEddyProc$new('DE-Tha', EddyNCData.F[1:17568,], lVar.V.s)
  # Subset of half 1996 to half of 1997
  EPThaNCsub.C <- sEddyProc$new('DE-Tha', EddyNCData.F[8737:(17568+9600),], lVar.V.s)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Fill gaps with MDS algorithm

#system.time(...)
EPTha.C$sMDSGapFill('NEE','QF', 0, FillAll.b=T, Verbose.b=T) 
EPTha.C$sMDSGapFill('NEE', V1.s='none', FillAll.b=T, Verbose.b=T)
EPThaH.C$sMDSGapFill('NEE','QF', 0, FillAll.b=T, Verbose.b=T)
EPThaS.C$sMDSGapFill('NEE','QF', 0, FillAll.b=T, Verbose.b=T)
EPThaL1.C$sMDSGapFill('NEE', V1.s='none', T1.n=NA_real_, V2.s='none', T2.n=NA_real_, V3.s='none', T3.n=NA_real_, FillAll.b=T, Verbose.b=T)
EPThaL1.C$sMDSGapFill('NEE', V1.s='none', FillAll.b=T, Verbose.b=T)
EPThaL1.C$sMDSGapFill('NEE', FillAll.b=T, Verbose.b=T)
EPThaL2.C$sMDSGapFill('NEE', FillAll.b=T, Verbose.b=T)
EPThaL3.C$sMDSGapFill('NEE', FillAll.b=T, Verbose.b=T)

# Fill also other variables
EPTha.C$sMDSGapFill('Rg', FillAll.b=T, Verbose.b=T)
EPTha.C$sMDSGapFill('VPD', FillAll.b=T, Verbose.b=T)
EPTha.C$sMDSGapFill('Tair', FillAll.b=T, Verbose.b=T)

# Fill multiple years
if (LongTest.b) {
  EPThaNC.C$sMDSGapFill('NEE_f', 'NEE_fqc', 0, FillAll.b=T, Verbose.b=T)
  EPThaNC96.C$sMDSGapFill('NEE_f', 'NEE_fqc', 0, FillAll.b=T, Verbose.b=T)
  EPThaNCsub.C$sMDSGapFill('NEE_f', 'NEE_fqc', 0, FillAll.b=T, Verbose.b=T)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Export processing results
ThaFilled.F <- EPTha.C$sExportResults()
fWriteDataframeToFile(cbind(EddyDataWithPosix.F,ThaFilled.F), 'DE-Tha-ResultsTest.txt', 'out')

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
# View(EPTha.C$sDATA)
# View(EPTha.C$sTEMP)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++]
# Plotting of single years/month
fPlots <- function(ClassName.s, Var.s, QFVar.s='none', QFValue.n=NA, VarUnc.s='none', Month.i=NA, Year.i=NA) {
  if( is.na(Year.i)) { #All years/months and save to file
    eval(parse(text=paste(ClassName.s, '$sPlotFingerprint(\'', Var.s, '\', QFVar.s=\'', QFVar.s, '\', QFValue.n=', QFValue.n, ')', sep='')))
    eval(parse(text=paste(ClassName.s, '$sPlotHHFluxes(\'', Var.s, '\', QFVar.s=\'', QFVar.s, '\', QFValue.n=', QFValue.n, ')', sep='')))
    eval(parse(text=paste(ClassName.s, '$sPlotDailySums(\'', Var.s, '\', VarUnc.s=\'', VarUnc.s, '\')', sep='')))
    eval(parse(text=paste(ClassName.s, '$sPlotDiurnalCycle(\'', Var.s, '\', QFVar.s=\'', QFVar.s, '\', QFValue.n=', QFValue.n, ')', sep='')))  
  } else {  #Individual years/months
    eval(parse(text=paste(ClassName.s, '$sPlotFingerprintY(\'', Var.s, '\', QFVar.s=\'', 
                          QFVar.s, '\', QFValue.n=', QFValue.n, ', Year.i=', Year.i, ')', sep='')))
    eval(parse(text=paste(ClassName.s, '$sPlotHHFluxesY(\'', Var.s, '\', QFVar.s=\'', 
                          QFVar.s, '\', QFValue.n=', QFValue.n, ', Year.i=', Year.i, ')', sep='')))
    eval(parse(text=paste(ClassName.s, '$sPlotDailySumsY(\'', Var.s, '\', VarUnc.s=\'', 
                          VarUnc.s, '\', Year.i=', Year.i, ')', sep='')))
    eval(parse(text=paste(ClassName.s, '$sPlotDiurnalCycleM(\'', Var.s, '\', QFVar.s=\'', 
                          QFVar.s, '\', QFValue.n=', QFValue.n, ', Month.i=', Month.i, ')', sep='')))
  }
}

if (LongTest.b) {
  #Before filling
  fPlots('EPTha.C', 'NEE', Month.i=10, Year.i=1998)
  fPlots('EPThaH.C', 'NEE', Month.i=10, Year.i=1998)
  fPlots('EPThaS.C', 'NEE', Month.i=10, Year.i=1998)
  fPlots('EPThaNCsub.C', 'NEE', Month.i=6, Year.i=1996)
  fPlots('EPThaNCsub.C', 'NEE', Month.i=6, Year.i=1997)
  #After filling
  fPlots('EPTha.C', 'NEE_f','NEE_fqc', 1, 'NEE_fsd', Month.i=10, Year.i=1998)
  fPlots('EPThaH.C', 'NEE_f','NEE_fqc', 1, 'NEE_fsd', Month.i=10, Year.i=1998)
  fPlots('EPThaS.C', 'NEE_f','NEE_fqc', 1, 'NEE_fsd', Month.i=10, Year.i=1998)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Plotting of standard example, unfilled and filled columns
fPlots('EPTha.C', 'NEE')
fPlots('EPTha.C', 'NEE_f')
fPlots('EPTha.C', 'NEE_f', 'NEE_fqc', 1)

# Images for multiple year data
if (LongTest.b) {
  # Multiple year plots with leap year
  fPlots('EPThaNC.C', 'NEE')
  fPlots('EPThaNC.C', 'NEE_f')
  fPlots('EPThaNC.C', 'NEE_f', 'NEE_fqc', 1)
  fPlots('EPThaNC96.C', 'NEE')
  fPlots('EPThaNCsub.C', 'NEE')
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
  
  # Standard filling - without ustar filtering
  EPTha98.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE', 'QF', 'Rg', 'Tair', 'VPD'))
  EPTha98.C$sMDSGapFill('NEE', FillAll.b=T, Verbose.b=T)
  
  # Load MDS output data from old PV-Wave online tool - without ustar filtering
  MDSData.F <- fLoadTXTIntoDataframe('Example_DETha98_PVWave_DataSetafterGapfill.txt','inst/examples')
  MDSData.F <- fConvertTimeToPosix(MDSData.F, 'YMDHM', Year.s = 'Year', Month.s= 'Month', Day.s = 'Day', Hour.s = 'Hour', Min.s = 'Minute')

  # Plot difference between old and new MDS
  plot(EPTha98.C$sTEMP$NEE_f ~ MDSData.F$NEE_f, col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3)

  
  # Load MDS output data from LaThuile BGI fluxnet files with ustar filtering
  # Rename 'NEE' to have different column names for old and new processing 
  # (though in different data frames: original data in sDATA and results in sTEMP)
  EddyNCData.F <- cbind(EddyNCData.F, NEEnew = EddyNCData.F$NEE)
  EPThaNC.C <- sEddyProc$new('DE-Tha', EddyNCData.F, c('NEEnew', 'Rg', 'Tair', 'VPD', 'NEE_f', 'NEE_fqc', 'NEE_fmet', 'NEE_fwin', 'NEE_fs', 'NEE_fn'))
  # Fill gaps - with ustar through NEE_fqc=0
  EPThaNC.C$sMDSGapFill('NEEnew', 'NEE_fqc', 0, FillAll.b=T, Verbose.b=T)
  
  # Plot difference between old and new MDS
  plot(EPThaNC.C$sTEMP$NEEnew_f ~ EPThaNC.C$sDATA$NEE_f, col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3)
  
  # Plot combination of the two BUT this is mixing with and without ustar filtering...
  plot(EPTha98.C$sTEMP$NEE_f ~ EPThaNC.C$sTEMP$NEEnew_f[35089:52608], col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3)
  plot(MDSData.F$NEE_f ~ EPThaNC.C$sDATA$NEE_f[35089:52608], col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3)
}
