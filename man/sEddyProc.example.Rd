\name{sEddyProc.example}
\alias{sEddyProc.example}
\title{sEddyProc - Example code}
\description{Dummy function to show code example for sEddyProc provided below}
\usage{sEddyProc.example()}



\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
#+++ Simple example code for using the sEddyProc reference class +++

if (FALSE) { #Do not always execute (e.g. on package installation)
  
  #+++ Load data with one header and one unit row from (tab-delimited) text file
  Dir.s <- paste(system.file(package='REddyProc'), '/data', sep='')
  EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
  #+++ If not provided, calculate VPD from Tair and rH
  EddyData.F <- cbind(EddyData.F,VPD=fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair))
  
  #+++ Add time stamp in POSIX time format
  EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='Day', Hour.s='Hour')

  #+++ Initalize R5 reference class sEddyProc for processing of eddy data with all variables needed for processing later
  EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD'))
  
  #+++ Generate plots of all data in directory \plots (of current R working dir)
  EddyProc.C$sPlotHHFluxes('NEE')
  EddyProc.C$sPlotFingerprint('Rg')
  EddyProc.C$sPlotDiurnalCycle('Tair')
  #+++ Plot individual years to screen (of current R graphics device)
  EddyProc.C$sPlotHHFluxesY('NEE', Year.i=1998)
  EddyProc.C$sPlotFingerprintY('NEE', Year.i=1998)
  
  #+++ Fill gaps in variables with MDS gap filling algorithm
  EddyProc.C$sMDSGapFill('NEE', Verbose.b=T)
  EddyProc.C$sMDSGapFill('Rg', Verbose.b=T)
  
  #+++ Generate plots of filled data in directory \plots (of current R working dir)
  EddyProc.C$sPlotHHFluxes('NEE_f')
  EddyProc.C$sPlotFingerprint('NEE_f')
  EddyProc.C$sPlotDailySums('NEE_f','NEE_fsd')
  EddyProc.C$sPlotDiurnalCycle('NEE_f')
  
  #+++ Plot other individual years/months to screen (of current R graphics device)
  EddyProc.C$sPlotDailySumsY('NEE_f','NEE_fsd', Year.s=1998)
  EddyProc.C$sPlotDiurnalCycleM('NEE_f', Month.i=1)
  
  #+++ Export gap filled data to standard data frame
  FilledEddyData.F <- EddyProc.C$sExportResults()
  
  #+++ Save results into (tab-delimited) text file in directory \out
  CombinedData.F <- cbind(EddyData.F, FilledEddyData.F)
  fWriteDataframeToFile(CombinedData.F, 'DE-Tha-Results.txt', 'out')
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Extra: Examples of extended usage for advanced users
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++ Add some (non-sense) example vectors:
  #+++ Quality flag vector (e.g. from applying ustar filter)
  EddyDataWithPosix.F <- cbind(EddyDataWithPosix.F, QF=rep(c(1,0,1,0,1,0,0,0,0,0),nrow(EddyData.F)/10))
  #+++ Step function vector to simulate e.g. high/low water table
  EddyDataWithPosix.F <- cbind(EddyDataWithPosix.F, Step= ifelse(EddyData.F$Day < 200 | EddyData.F$Day > 250, 0, 1))

  #+++ Initialize eddy processing class with more columns
  EddyTest.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE', 'LE', 'H', 'Rg', 'Tair', 'Tsoil', 'Rh', 'VPD', 'QF', 'Step'))

  #+++ Gap fill variable after applying quality flag QF with (non-default) variables and limits
  EddyTest.C$sMDSGapFill('LE', QFVar.s='QF', QFValue.n=0, V1.s='Rg', T1.n=30, V2.s='Tsoil', T2.n=2, 'Step', 0.1, Verbose.b=T)

  #+++ Try the gap filling subroutines individually for different window sizes and up to five variables and limits
  EddyTest.C$sFillInit('NEE') #Initalize to fill variable 'NEE'
  Result_Step1.F <- EddyTest.C$sFillLUT(3, 'Rg',50, 'Rh',15, 'Tair',2.5, 'Tsoil',1, 'Step',0.1)
  Result_Step2.F <- EddyTest.C$sFillLUT(6, 'Tair',2.5, 'VPD',3, 'Step',0.1)
  Result_Step3.F <- EddyTest.C$sFillMDC(3)
  EddyTest.C$sPlotHHFluxes('VAR_f') #Individual fill result columns are called 'VAR_...'

}
}
