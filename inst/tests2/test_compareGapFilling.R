# testing that results of gfGapFillMDS equal those of sMDSGapFill (regression test)

packageDir <- paste(system.file(package='REddyProc'), 'examples', sep='/')
origData <- fLoadTXTIntoDataframe('Example_DETha98.txt', packageDir)
origData <- cbind(origData,VPD=fCalcVPDfromRHandTair(origData$rH, origData$Tair))
data <- fConvertTimeToPosix(origData, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')

expected <- eddy$sTEMP

test_that("gfGapFillMDS fillAll=FALSE",{
			eddy <- sEddyProc$new('DE-Tha', data, c('NEE','Rg','Tair','VPD', 'Ustar'))
			time1 <- system.time( eddy$sMDSGapFill('NEE', FillAll.b=FALSE) )
			#
			time2 <- system.time(tmp <- gfGapFillMDS(data))
			sTEMP <- eddy$sTEMP
			.tmp.plot <- function(){
				plot( tmp$filled ~ sTEMP$NEE_f)
				plot( tmp$fnum ~ sTEMP$NEE_fnum)
				plot( tmp$fsd ~ sTEMP$NEE_fsd)
			}
			expect_true( max(abs(tmp$filled - sTEMP$NEE_f),na.rm=TRUE) < 1e-8 )
			expect_true( !length(iDiff <- which( tmp$fnum != sTEMP$NEE_fnum)) )
			expect_true( max(abs(tmp$fsd - sTEMP$NEE_fsd), na.rm=TRUE) < 1e-8 )
			#quality flags
			expect_true( !length(iDiff <- which( tmp$fmeth!= sTEMP$NEE_fmeth)) )
			expect_true( !length(iDiff <- which( tmp$fqc!= sTEMP$NEE_fqc)) )
			#sTEMP[head(iDiff),]; tmp[head(iDiff),]
			time1/time2 #speedup of about 3
		})

test_that("gfGapFillMDS fillAll=FALSE",{
			eddy2 <- sEddyProc$new('DE-Tha', data, c('NEE','Rg','Tair','VPD', 'Ustar'))
			time1a <- system.time( eddy2$sMDSGapFill('NEE', FillAll.b=TRUE) ) 
			#
			time2a <- system.time(tmp2 <- gfGapFillMDS(data, isFillAll=TRUE))
			sTEMP <- eddy2$sTEMP	
			time1a/time2a #speedup of about 5
			.tmp.plot <- function(){
				plot( tmp2$filled ~ sTEMP$NEE_f)
				plot( tmp2$fnum ~ sTEMP$NEE_fnum)
				plot( tmp2$fsd ~ sTEMP$NEE_fsd)
			}
			expect_true( max(abs(tmp2$filled - sTEMP$NEE_f),na.rm=TRUE) < 1e-8 )
			expect_true( !length(iDiff <- which( tmp2$fnum != sTEMP$NEE_fnum)) )
			expect_true( max(abs(tmp2$fsd - sTEMP$NEE_fsd), na.rm=TRUE) < 1e-8 )
			#quality flags
			expect_true( !length(iDiff <- which( tmp2$fmeth!= sTEMP$NEE_fmeth)) )
			expect_true( !length(iDiff <- which( tmp2$fqc!= sTEMP$NEE_fqc)) )
			#sTEMP[head(iDiff),]; tmp2[head(iDiff),]
		})


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
	#table(expected$NEE_fqc)	# three fewer records? after only 7days pass
}

