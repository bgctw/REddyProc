#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for fConvertTimeToPosix functions +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: TW
#require(testthat)
context("UStarFilter")
# Furher context: fCheckColNames, fCheckColNumeric, fCheckOutsideRange

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
EddyDataWithPosix.F <- ds <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
dss <- subset(EddyDataWithPosix.F, DoY >= 150 & DoY <= 250)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("binUstar classes are correct",{
	res <- binUstar(dss$NEE, dss$Ustar)
	UstarClasses <- controlUstarSubsetting()$UstarClasses
	Ust_bin_size <- round(nrow(ds.f)/UstarClasses)
	# create df of NEE and UStar ordered by Ustar
	ds.f <- data.frame(NEE=dss$NEE,Ustar=dss$Ustar)
	ds.f <- arrange(ds.f,ds.f[,2])
	# create bin id column 
	ds.f$bin <- UstarClasses # due to rounding, the last classes may be left and neet to get the last class
	ds.f$bin[1:(UstarClasses*Ust_bin_size)]	 <-	 rep(1:UstarClasses, each=Ust_bin_size)
	# do the averaging
	tmp1 <- ddply( ds.f, .(bin), function(dsBin){ c(Ust_avg=mean(dsBin[,2], na.rm=TRUE),NEE_avg=mean(dsBin[,1], na.rm=TRUE))})
	expect_that( res, equals(tmp1[,-1]))
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

