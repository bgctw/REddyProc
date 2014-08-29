\name{sEddyProc.example}
\alias{sEddyProc.example}
\title{sEddyProc - Example code}
\description{Dummy function to show code example for sEddyProc provided below}
\usage{sEddyProc.example()}



\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
#+++ Simple example code for using the sEddyProc reference class +++

if( FALSE ) { #Do not always execute example code (e.g. on package installation)
  
  #+++ Load data with one header and one unit row from (tab-delimited) text file
  Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
  EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
  #+++ If not provided, calculate VPD from Tair and rH
  EddyData.F <- cbind(EddyData.F,VPD=fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair))
  
  #+++ Add time stamp in POSIX time format
  EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')

  #+++ Initalize R5 reference class sEddyProc for processing of eddy data
  #+++ with all variables needed for processing later
  EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD'))
  
  #+++ Generate plots of all data in directory \plots (of current R working dir)
  EddyProc.C$sPlotHHFluxes('NEE')
  EddyProc.C$sPlotFingerprint('Rg')
  EddyProc.C$sPlotDiurnalCycle('Tair')
  #+++ Plot individual years to screen (of current R graphics device)
  EddyProc.C$sPlotHHFluxesY('NEE', Year.i=1998)
  EddyProc.C$sPlotFingerprintY('NEE', Year.i=1998)
  
  #+++ Fill gaps in variables with MDS gap filling algorithm
  EddyProc.C$sMDSGapFill('Rg', FillAll.b=FALSE) #Fill only gaps of meteo condition
  EddyProc.C$sMDSGapFill('NEE', FillAll.b=TRUE) #Fill all values to estimate flux uncertainties

  #+++ Generate plots of filled data in directory \plots (of current R working dir)
  EddyProc.C$sPlotHHFluxes('NEE_f')
  EddyProc.C$sPlotFingerprint('NEE_f')
  EddyProc.C$sPlotDailySums('NEE_f','NEE_fsd')
  EddyProc.C$sPlotDiurnalCycle('NEE_f')
  
  #+++ Plot individual years/months to screen (of current R graphics device)
  EddyProc.C$sPlotHHFluxesY('NEE_f', Year.i=1998)
  EddyProc.C$sPlotFingerprintY('NEE_f', Year.i=1998)
  EddyProc.C$sPlotDailySumsY('NEE_f','NEE_fsd', Year.i=1998)
  EddyProc.C$sPlotDiurnalCycleM('NEE_f', Month.i=1)
	
  #+++ Partition NEE into GPP and respiration
  EddyProc.C$sMDSGapFill('Tair', FillAll.b=FALSE)		# gap-filled Tair and NEE is needed for partitioning 
  EddyProc.C$sMRFluxPartition( Lat_deg.n=51, Long_deg.n=7, TimeZone_h.n=1 )  # location of DE-Tharandt
  EddyProc.C$sPlotDiurnalCycleM('GPP_f', Month.i=6)	# plot calculated GPP and respiration 
  EddyProc.C$sPlotDiurnalCycleM('Reco', Month.i=6)	
  
  #+++ Export gap filled and partitioned data to standard data frame
  FilledEddyData.F <- EddyProc.C$sExportResults()
  
  #+++ Save results into (tab-delimited) text file in directory \out
  CombinedData.F <- cbind(EddyData.F, FilledEddyData.F)
  fWriteDataframeToFile(CombinedData.F, 'DE-Tha-Results.txt', 'out')
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Extra: Examples of extended usage for advanced users
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++ Add some (non-sense) example vectors:
  #+++ Quality flag vector (e.g. from applying ustar filter)
  EddyDataWithPosix.F <- cbind(EddyDataWithPosix.F, QF=rep(c(1,0,1,0,1,0,0,0,0,0), nrow(EddyData.F)/10))
  #+++ Step function vector to simulate e.g. high/low water table
  EddyDataWithPosix.F <- cbind(EddyDataWithPosix.F, Step=ifelse(EddyData.F$DoY < 200 | EddyData.F$DoY > 250, 0, 1))

  #+++ Initialize eddy processing class with more columns
  EddyTest.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, 
                              c('NEE', 'LE', 'H', 'Rg', 'Tair', 'Tsoil', 'rH', 'VPD', 'QF', 'Step'))

  #+++ Gap fill variable with (non-default) variables and limits including preselection with quality flag QF 
  EddyTest.C$sMDSGapFill('LE', QFVar.s='QF', QFValue.n=0, V1.s='Rg', T1.n=30, V2.s='Tsoil', T2.n=2, 'Step', 0.1)

  #+++ Use individual gap filling subroutines with different window sizes and up to five variables and limits
  EddyTest.C$sFillInit('NEE') #Initalize 'NEE' as variable to fill
  Result_Step1.F <- EddyTest.C$sFillLUT(3, 'Rg',50, 'rH',30, 'Tair',2.5, 'Tsoil',2, 'Step',0.5)
  Result_Step2.F <- EddyTest.C$sFillLUT(6, 'Tair',2.5, 'VPD',3, 'Step',0.5)
  Result_Step3.F <- EddyTest.C$sFillMDC(3)
  EddyTest.C$sPlotHHFluxesY('VAR_fall', Year.i=1998) #Individual fill result columns are called 'VAR_...'
}
}
