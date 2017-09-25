#Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
#EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
EddyData.F <- fLoadTXTIntoDataframe( getExamplePath('Example_DETha98.txt'))
# note: use \code{fFilterAttr} to subset rows while keeping the units attributes

DTS.n=48L
.tmp.TestHourlyData <- function(){
	# take only rows of full hour, to test if this causes problems
	# seems ok
	head(EddyData.F)
	EddyData.F <- EddyData.F[ (EddyData.F$Hour %% 1 < 1e-5), ]
	DTS.n=24L
	head(EddyData.F)
}

#+++ If not provided, calculate VPD from Tair and rH
EddyData.F <- cbind(EddyData.F,VPD=fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair))

#+++ Add time stamp in POSIX time format
EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')

#+++ Initalize R5 reference class sEddyProc for processing of eddy data
#+++ with all variables needed for processing later
EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD', 'Ustar'), DTS.n=DTS.n)
EddyProc.C$sSetLocationInfo(Lat_deg.n=51.0, Long_deg.n=13.6, TimeZone_h.n=1)  #Location of DE-Tharandt

#+++ Fill gaps in variables with MDS gap filling algorithm (without prior ustar filtering)
EddyProc.C$sMDSGapFill('NEE', FillAll.b=TRUE) #Fill all values to estimate flux uncertainties
EddyProc.C$sMDSGapFill('Rg', FillAll.b=FALSE) #Fill only the gaps for the meteo condition, e.g. 'Rg'

#+++ Partition NEE into GPP and respiration
EddyProc.C$sMDSGapFill('Tair', FillAll.b=FALSE)  	# Gap-filled Tair (and NEE) needed for partitioning 
EddyProc.C$sMDSGapFill('VPD', FillAll.b=FALSE)  	# Gap-filled Tair (and NEE) needed for partitioning 
EddyProc.C$sMRFluxPartition()	# night time partitioning -> Reco, GPP

.tmp.f <- function(){
	#save(EddyProc.C, file="tmp/EddyProcC_Tharandt.RData")
	load(file="tmp/EddyProcC_Tharandt.RData")
}
#EddyProc.C$sGLFluxPartition()	# day time partitioning -> Reco_DT, GPP_DT
#EddyProc.C$sGLFluxPartition(controlGLPart.l=partGLControl(isBoundLowerNEEUncertainty=FALSE, ))	# day time partitioning -> Reco_DT, GPP_DT
EddyProc.C$sGLFluxPartition(controlGLPart=partGLControl(
	nBootUncertainty=0L, isAssociateParmsToMeanOfValids=FALSE, isLasslopPriorsApplied=TRUE, isBoundLowerNEEUncertainty=FALSE
))

plot( EddyProc.C$sTEMP$FP_R_refNight ~ EddyProc.C$sTEMP$sDateTime)
points( EddyProc.C$sTEMP$FP_R_ref ~ EddyProc.C$sTEMP$sDateTime, col="blue")

plot( EddyProc.C$sTEMP$FP_E0 ~ EddyProc.C$sTEMP$sDateTime)
plot( EddyProc.C$sTEMP$FP_R_ref ~ EddyProc.C$sTEMP$FP_R_refNight); abline(0,1)
plot( EddyProc.C$sTEMP$GPP_DT ~ EddyProc.C$sTEMP$GPP_f); abline(0,1)
plot( EddyProc.C$sTEMP$Reco_DT ~ EddyProc.C$sTEMP$Reco ); abline(0,1)
#plot( -EddyProc.C$sTEMP$GPP_f + EddyProc.C$sTEMP$Reco ~ EddyProc.C$sTEMP$NEE_f ); abline(0,1)
plot( -EddyProc.C$sTEMP$GPP_DT + EddyProc.C$sTEMP$Reco_DT ~ EddyProc.C$sTEMP$NEE_f ); abline(0,1)
