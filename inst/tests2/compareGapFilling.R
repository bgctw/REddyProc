packageDir <- paste(system.file(package='REddyProc'), 'examples', sep='/')
origData <- fLoadTXTIntoDataframe('Example_DETha98.txt', packageDir)
origData <- cbind(origData,VPD=fCalcVPDfromRHandTair(origData$rH, origData$Tair))
data <- fConvertTimeToPosix(origData, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
eddy <- sEddyProc$new('DE-Tha', data, c('NEE','Rg','Tair','VPD', 'Ustar'))
time1 <- system.time( eddy$sMDSGapFill('NEE', FillAll.b=FALSE) ) 
eddy2 <- sEddyProc$new('DE-Tha', data, c('NEE','Rg','Tair','VPD', 'Ustar'))
time1a <- system.time( eddy2$sMDSGapFill('NEE', FillAll.b=TRUE) ) 

expected <- eddy$sTEMP

.tmp.debug <- function(){
	# in recover after 7 days of gapfilling 
	sTEMP <- eddy$sTEMP
	fTol <- gfCreateRgToleranceFunction(
			tolerance=c(Rg=50,VPD=5,Tair=2.5)
			, iRgColumns=1L )
	res7 <- gfGapFillLookupTable(data$NEE, 7L*48L, data[,c('Rg','VPD','Tair')], fTolerance=fTol)
	tmp <- cbind(data, data.frame(mean=NA,fnum=NA,fsd=NA))
	nrow(res7)
	tmp[res7[,"index"],c("mean","fnum","fsd")] <- res7[,c("mean","fnum","fsd")] 
	table(sTEMP$VAR_fqc)	# three fewer records?
	bo <- is.finite(res7[,"mean"]) && sTEMP$VAR_f 
	plot( tmp$mean ~ sTEMP$VAR_f)
	plot( tmp$fnum ~ sTEMP$VAR_fnum); abline(0,1)
	plot( tmp$fsd ~ sTEMP$VAR_fsd); abline(0,1)

	time2 <- system.time(tmp <- gfGapFillMDS(data))
	sTEMP <- eddy$sTEMP	
	plot( tmp$filled ~ sTEMP$NEE_f)
	max(abs(tmp$filled - sTEMP$NEE_f),na.rm=TRUE) < 1e-8
	plot( tmp$fnum ~ sTEMP$NEE_fnum)
	!length(iDiff <- which( tmp$fnum != sTEMP$NEE_fnum))
	plot( tmp$fsd ~ sTEMP$NEE_fsd)
	max(abs(tmp$fsd - sTEMP$NEE_fsd), na.rm=TRUE) < 1e-8

	#TODO check the differences in quality flags
	!length(iDiff <- which( tmp$fmeth!= sTEMP$NEE_fmeth))
	!length(iDiff <- which( tmp$fqc!= sTEMP$NEE_fqc))
	
	
	time2a <- system.time(tmp <- gfGapFillMDS(data, isFillAll=TRUE))
	sTEMP <- eddy2$sTEMP	
	#speedup of about 6
}

