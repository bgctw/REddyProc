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
	#library(REddyProc)
	
	#+++ Load data with one header and one unit row from (tab-delimited) text file
	Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
	EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
	# note: use \code{fFilterAttr} to subset rows while keeping the units attributes
	
	#+++ If not provided, calculate VPD from Tair and rH
	EddyData.F <- cbind(EddyData.F,VPD=fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair))
	
	#+++ Add time stamp in POSIX time format
	EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
	
	#+++ Initalize R5 reference class sEddyProc for processing of eddy data
	#+++ with all variables needed for processing later
	EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD', 'Ustar'))
	EddyProc.C$sSetLocationInfo(Lat_deg.n=51.0, Long_deg.n=13.6, TimeZone_h.n=1)  #Location of DE-Tharandt
	
	#+++ Generate plots of all data in directory \plots (of current R working dir)
	EddyProc.C$sPlotHHFluxes('NEE')
	EddyProc.C$sPlotFingerprint('Rg')
	EddyProc.C$sPlotDiurnalCycle('Tair')
	#+++ Plot individual months/years to screen (of current R graphics device)
	EddyProc.C$sPlotHHFluxesY('NEE', Year.i=1998)
	EddyProc.C$sPlotFingerprintY('NEE', Year.i=1998)
	
	#+++ Fill gaps in variables with MDS gap filling algorithm (without prior ustar filtering)
	EddyProc.C$sMDSGapFill('NEE', FillAll.b=TRUE) #Fill all values to estimate flux uncertainties
	EddyProc.C$sMDSGapFill('Rg', FillAll.b=FALSE) #Fill only the gaps for the meteo condition, e.g. 'Rg'
	
	#+++ Example plots of filled data to screen or to directory \plots
	EddyProc.C$sPlotFingerprintY('NEE_f', Year.i=1998)
	EddyProc.C$sPlotDailySumsY('NEE_f','NEE_fsd', Year.i=1998) #Plot of sums with uncertainties
	EddyProc.C$sPlotDailySums('NEE_f','NEE_fsd')
	
	#+++ Partition NEE into GPP and respiration
	EddyProc.C$sMDSGapFill('Tair', FillAll.b=FALSE)  	# Gap-filled Tair (and NEE) needed for partitioning 
	EddyProc.C$sMDSGapFill('VPD', FillAll.b=FALSE)  	# Gap-filled Tair (and NEE) needed for partitioning 
	EddyProc.C$sMRFluxPartition()	# night time partitioning -> Reco, GPP
	EddyProc.C$sGLFluxPartition()	# day time partitioning -> Reco_DT, GPP_DT
	#EddyProc.C$sGLFluxPartition(controlGLPart.l=partGLControl(isBoundLowerNEEUncertainty=FALSE))	# day time partitioning -> Reco_DT, GPP_DT
	#plot( EddyProc.C$sTEMP$GPP_DT ~ EddyProc.C$sTEMP$GPP_f); abline(0,1)
	#plot( -EddyProc.C$sTEMP$GPP_DT + EddyProc.C$sTEMP$Reco_DT ~ EddyProc.C$sTEMP$NEE_f ); abline(0,1)
	#names(EddyProc.C$sTEMP)
	# there are some constraints, that might be too strict for some datasets
	# e.g. in the tropics the required temperature range might be too large.
	# Its possible to change these constraints
	#EddyProc.C$sMRFluxPartition(parsE0Regression=list(TempRange.n=2.0, optimAlgorithm="LM")	)  
	
	
	#+++ Example plots of calculated GPP and respiration 
	EddyProc.C$sPlotFingerprintY('GPP_f', Year.i=1998)
	EddyProc.C$sPlotFingerprint('GPP_f')
	EddyProc.C$sPlotHHFluxesY('Reco', Year.i=1998)
	EddyProc.C$sPlotHHFluxes('Reco')
	
	#+++ Processing with ustar filtering before  
	EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD', 'Ustar'))
	EddyProc.C$sSetLocationInfo(Lat_deg.n=51.0, Long_deg.n=13.6, TimeZone_h.n=1)  #Location of DE-Tharandt
	# estimating the thresholds based on the data
	(uStarTh <- EddyProc.C$sEstUstarThreshold()$uStarTh)
	# plot saturation of NEE with UStar for one season
	EddyProc.C$sPlotNEEVersusUStarForSeason( levels(uStarTh$season)[3] )
	# Gapfilling by default it takes the annually aggregated estimate is used to mark periods with low uStar
	# for other options see Example 4
	EddyProc.C$sMDSGapFillAfterUstar('NEE')
	colnames(EddyProc.C$sExportResults()) # Note the collumns with suffix _WithUstar	
	EddyProc.C$sMDSGapFill('Tair', FillAll.b=FALSE)
	EddyProc.C$sMRFluxPartition(Lat_deg.n=51.0, Long_deg.n=13.6, TimeZone_h.n=1, Suffix.s='WithUstar')  # Note suffix
	EddyProc.C$sMRFluxPartition(Lat_deg.n=51.0, Long_deg.n=13.6, TimeZone_h.n=1, Suffix.s='WithUstar')  # Note suffix
	
	#+++ Export gap filled and partitioned data to standard data frame
	FilledEddyData.F <- EddyProc.C$sExportResults()
	#+++ Save results into (tab-delimited) text file in directory \out
	CombinedData.F <- cbind(EddyData.F, FilledEddyData.F)
	#+++ May rename variables to correspond to Ameriflux 
	colnames(CombinedDataAmeriflux.F <- renameVariablesInDataframe(CombinedData.F, getBGC05ToAmerifluxVariableNameMapping() ))
	CombinedDataAmeriflux.F$TIMESTAMP_END <- POSIXctToBerkeleyJulianDate( EddyProc.C$sExportData()[[1]] )
	head(tmp <- BerkeleyJulianDateToPOSIXct( CombinedDataAmeriflux.F$TIMESTAMP_END ))
	#colnames(tmp <- renameVariablesInDataframe(CombinedData.F, getAmerifluxToBGC05VariableNameMapping() ))
	fWriteDataframeToFile(CombinedData.F, 'DE-Tha-Results.txt', 'out')
	
	#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	#+++ Example 1 for advanced users: Processing different setups on the same site data
	#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	
	#+++ Initialize new sEddyProc processing class
	EddySetups.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
	EddySetups.C$sSetLocationInfo(Lat_deg.n=51.0, Long_deg.n=13.6, TimeZone_h.n=1)  #Location of DE-Tharandt
	
	#+++ When running several processing setup, a string suffix declaration is needed
	#+++ Here: Gap filling with and without ustar threshold
	EddySetups.C$sMDSGapFill('NEE', FillAll.b=FALSE, Suffix.s='NoUstar')
	EddySetups.C$sMDSGapFillAfterUstar('NEE', FillAll.b=FALSE, UstarThres.df=0.3, UstarSuffix.s='Thres1')
	EddySetups.C$sMDSGapFillAfterUstar('NEE', FillAll.b=FALSE, UstarThres.df=0.4, UstarSuffix.s='Thres2')
	EddySetups.C$sMDSGapFill('Tair', FillAll.b=FALSE)    # Gap-filled Tair needed for partitioning
	EddySetups.C$sMDSGapFill('VPD', FillAll.b=FALSE)    # Gap-filled VPD needed for daytime partitioning
	colnames(EddySetups.C$sExportResults()) # Note the suffix in output columns
	
	#+++ Flux partitioning of the different gap filling setups
	EddySetups.C$sMRFluxPartition(Suffix.s='NoUstar')
	EddySetups.C$sMRFluxPartition(Suffix.s='Thres1')
	EddySetups.C$sMRFluxPartition(Suffix.s='Thres2')
	EddySetups.C$sGLFluxPartition(Suffix.s='NoUstar')
	colnames(EddySetups.C$sExportResults()) # Note the suffix in output columns also of GPP, Reco, GPP_DT, and Reco_DT
	
	#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# Example 2 for advanced users: Extended usage of the gap filling algorithm
	#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	
	#+++ Add some (non-sense) example vectors:
	#+++ Quality flag vector (e.g. from applying ustar filter)
	QF.V.n <- rep(c(1,0,1,0,1,0,0,0,0,0), nrow(EddyData.F)/10)
	#+++ Dummy step function vector to simulate e.g. high/low water table
	Step.V.n <- ifelse(EddyData.F$DoY < 200 | EddyData.F$DoY > 250, 0, 1)
	
	#+++ Initialize new sEddyProc processing class with more columns
	EddyTest.C <- sEddyProc$new('DE-Tha', cbind(EddyDataWithPosix.F, Step=Step.V.n, QF=QF.V.n), 
			c('NEE', 'LE', 'H', 'Rg', 'Tair', 'Tsoil', 'rH', 'VPD', 'QF', 'Step'))
	EddyTest.C$sSetLocationInfo(Lat_deg.n=51.0, Long_deg.n=13.6, TimeZone_h.n=1)  #Location of DE-Tharandt
	
	#+++ Gap fill variable with (non-default) variables and limits including preselection of data with quality flag QF==0 
	EddyTest.C$sMDSGapFill('LE', QFVar.s='QF', QFValue.n=0, V1.s='Rg', T1.n=30, V2.s='Tsoil', T2.n=2, 'Step', 0.1)
	
	#+++ Use individual gap filling subroutines with different window sizes and up to five variables and limits
	EddyTest.C$sFillInit('NEE') #Initialize 'NEE' as variable to fill
	Result_Step1.F <- EddyTest.C$sFillLUT(3, 'Rg',50, 'rH',30, 'Tair',2.5, 'Tsoil',2, 'Step',0.5)
	Result_Step2.F <- EddyTest.C$sFillLUT(6, 'Tair',2.5, 'VPD',3, 'Step',0.5)
	Result_Step3.F <- EddyTest.C$sFillMDC(3)
	EddyTest.C$sPlotHHFluxesY('VAR_fall', Year.i=1998) #Individual fill result columns are called 'VAR_...' 
	
	
	#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# Example 3 for advanced users: Explicit demonstration of MDS gap filling algorithm for filling NEE
	#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	
	#+++ Initialize new sEddyProc processing class
	EddyTestMDS.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE', 'Rg', 'Tair', 'VPD'))
	EddyTestMDS.C$sSetLocationInfo(Lat_deg.n=51.0, Long_deg.n=13.6, TimeZone_h.n=1)  #Location of DE-Tharandt
	#Initialize 'NEE' as variable to fill
	EddyTestMDS.C$sFillInit('NEE')
	# Set variables and tolerance intervals
	# twutz: \u00B1 is the unicode for '+over-'
	V1.s='Rg'; T1.n=50 # Global radiation 'Rg' within \u00B150 W m-2
	V2.s='VPD'; T2.n=5 # Vapour pressure deficit 'VPD' within 5 hPa
	V3.s='Tair'; T3.n=2.5 # Air temperature 'Tair' within \u00B12.5 degC
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
	for( WinDays.i in seq(21,70,7) ) Result_Step6.F <- EddyTestMDS.C$sFillLUT(WinDays.i, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n)
	# Step 7: Look-up table with window size \u00B114, \u00B121, ..., \u00B170, Rg only
	for( WinDays.i in seq(14,70,7) ) Result_Step7.F <- EddyTestMDS.C$sFillLUT(WinDays.i, V1.s, T1.n)
	# Step 8: Mean diurnal course with window size \u00B17, \u00B114, ..., \u00B1210 days  
	for( WinDays.i in seq(7,210,7) ) Result_Step8.F <- EddyTestMDS.C$sFillMDC(WinDays.i)
	# Export results, columns are named 'VAR_'
	FilledEddyData.F <- EddyTestMDS.C$sExportResults()
	
	#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# Example 4 for advanced users: Processing of different UStar-Threshold setups
	#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	
	#+++ Provide a single user-defined uStarThreshold
	EddySetups.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
	EddySetups.C$sSetLocationInfo(Lat_deg.n=51.0, Long_deg.n=13.6, TimeZone_h.n=1)  #Location of DE-Tharandt
	Ustar.V.n <- 0.46  
	EddySetups.C$sMDSGapFillAfterUstar('NEE', UstarThres.df=Ustar.V.n)
	
	# Type 'vignette(DEGebExample)' to view an example
	#  - using tailored seasons of differing uStar dynamics with vegetation changes (crop)
	#  - using seasonal instead of annual uStar threshold estimates in gapfilling
	#  - Bootstrapping uncertainty associated with uStar Threshold estimation
	#  - Using change point detection instead of moving point method
	# The vignette is only available if REddyProc was installed from binary package.
	# A version can be seen at https://github.com/bgctw/REddyProc/blob/master/vignettes/DEGebExample.md
}
}
