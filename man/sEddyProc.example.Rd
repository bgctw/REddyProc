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
  EddyProc.C$sMDSGapFill('NEE', FillAll.b=TRUE) #Fill all values to estimate flux uncertainties
  EddyProc.C$sMDSGapFill('Rg', FillAll.b=FALSE) #Fill only the gaps for the meteo condition, e.g. 'Rg'

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
  EddyProc.C$sMDSGapFill('Tair', FillAll.b=FALSE)		# Gap-filled Tair and NEE needed for partitioning 
  EddyProc.C$sMRFluxPartition( Lat_deg.n=51.0, Long_deg.n=13.6, TimeZone_h.n=1 )  # location of DE-Tharandt
  EddyProc.C$sPlotDiurnalCycleM('GPP_f', Month.i=6)	# Plot of calculated GPP and respiration 
  EddyProc.C$sPlotDiurnalCycleM('Reco', Month.i=6)	
  
  #+++ Export gap filled and partitioned data to standard data frame
  FilledEddyData.F <- EddyProc.C$sExportResults()
  
  #+++ Save results into (tab-delimited) text file in directory \out
  CombinedData.F <- cbind(EddyData.F, FilledEddyData.F)
  fWriteDataframeToFile(CombinedData.F, 'DE-Tha-Results.txt', 'out')
  
  #+++ Ustar filtering is still in the works but if the ustar threshold is known,
  #the gap filling can be applied with filtering
  EddyProcUstar.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
  EddyProcUstar.C$sMDSGapFillAfterUstar(UstarThres.n=0.35)
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Extra: Examples of extended usage for advanced users
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++ Add some (non-sense) example vectors:
  #+++ Quality flag vector (e.g. from applying ustar filter)
  QF.V.n <- rep(c(1,0,1,0,1,0,0,0,0,0), nrow(EddyData.F)/10)
  #+++ Dummy step function vector to simulate e.g. high/low water table
  Step.V.n <- ifelse(EddyData.F$DoY < 200 | EddyData.F$DoY > 250, 0, 1)

  #+++ Initialize eddy processing class with more columns
  EddyTest.C <- sEddyProc$new('DE-Tha', cbind(EddyDataWithPosix.F, Step=Step.V.n, QF=QF.V.n), 
                              c('NEE', 'LE', 'H', 'Rg', 'Tair', 'Tsoil', 'rH', 'VPD', 'QF', 'Step'))

  #+++ Gap fill variable with (non-default) variables and limits including preselection of data with quality flag QF==0 
  EddyTest.C$sMDSGapFill('LE', QFVar.s='QF', QFValue.n=0, V1.s='Rg', T1.n=30, V2.s='Tsoil', T2.n=2, 'Step', 0.1)

  #+++ Use individual gap filling subroutines with different window sizes and up to five variables and limits
  EddyTest.C$sFillInit('NEE') #Initialize 'NEE' as variable to fill
  Result_Step1.F <- EddyTest.C$sFillLUT(3, 'Rg',50, 'rH',30, 'Tair',2.5, 'Tsoil',2, 'Step',0.5)
  Result_Step2.F <- EddyTest.C$sFillLUT(6, 'Tair',2.5, 'VPD',3, 'Step',0.5)
  Result_Step3.F <- EddyTest.C$sFillMDC(3)
  EddyTest.C$sPlotHHFluxesY('VAR_fall', Year.i=1998) #Individual fill result columns are called 'VAR_...'

  #+++ Explicit demonstration of MDS gap filling algorithm for filling NEE
  EddyTestMDS.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE', 'Rg', 'Tair', 'VPD'))
  #Initialize 'NEE' as variable to fill
  EddyTestMDS.C$sFillInit('NEE')
  # Set variables and tolerance intervals
  V1.s='Rg'; T1.n=50 # Global radiation 'Rg' within ±50 W m-2
  V2.s='VPD'; T2.n=5 # Vapour pressure deficit 'VPD' within 5 hPa
  V3.s='Tair'; T3.n=2.5 # Air temperature 'Tair' within ±2.5 degC
  # Step 1: Look-up table with window size ±7 days
  Result_Step1.F <- EddyTestMDS.C$sFillLUT(7, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n)
  # Step 2: Look-up table with window size ±14 days
  Result_Step2.F <- EddyTestMDS.C$sFillLUT(14, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n)
  # Step 3: Look-up table with window size ±7 days, Rg only
  Result_Step3.F <- EddyTestMDS.C$sFillLUT(7, V1.s, T1.n)
  # Step 4: Mean diurnal course with window size 0 (same day) 
  Result_Step4.F <- EddyTestMDS.C$sFillMDC(0)
  # Step 5: Mean diurnal course with window size ±1, ±2 days 
  Result_Step5a.F <- EddyTestMDS.C$sFillMDC(1)
  Result_Step5b.F <- EddyTestMDS.C$sFillMDC(2) 
  # Step 6: Look-up table with window size ±21, ±28, ..., ±70 
  for( WinDays.i in seq(21,70,7) ) Result_Step6.F <- EddyTestMDS.C$sFillLUT(WinDays.i, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n)
  # Step 7: Look-up table with window size ±14, ±21, ..., ±70, Rg only
  for( WinDays.i in seq(14,70,7) ) Result_Step7.F <- EddyTestMDS.C$sFillLUT(WinDays.i, V1.s, T1.n)
  # Step 8: Mean diurnal course with window size ±7, ±14, ..., ±210 days  
  for( WinDays.i in seq(7,210,7) ) Result_Step8.F <- EddyTestMDS.C$sFillMDC(WinDays.i)
  # Export results, columns are named 'VAR_'
  FilledEddyData.F <- EddyTestMDS.C$sExportResults()
}
}
