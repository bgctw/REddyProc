#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for sEddyProc functions +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM
#require(testthat)
context("sEddyProc-Class")

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
#Include POSIX time stamp column
EddyDataWithPosix.F <- suppressMessages(fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour'))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Check sEddyProc initialization: POSIX time stamp
test_that("POSIX time stamp: correct format",{   
  EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD'))
  expect_that(as.numeric(EddyProc.C$sDATA$sDateTime[1]), equals(883613700))
})
test_that("POSIX time stamp: missing column",{
  expect_error( 
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyData.F, c('NEE','Rg', 'Tair', 'VPD'))
  )
})
test_that("POSIX time stamp: wrong column type",{        
  expect_error( 
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyData.F, c('NEE','Rg', 'Tair', 'VPD'), 'Year')
    )
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Check sEddyProc initialization: Time series problems
test_that("Invalid number of daily time steps",{
  expect_error( 
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD'), DTS.n=12)
  )
})
test_that("Time series not in equidistant steps",{
  expect_error(
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[c(-50,-60),], c('NEE','Rg', 'Tair', 'VPD'))
  )
  expect_error( #Pseudo hourly by [c(F,T),]
    EddyProcH.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[c(F,T),][c(-50,-60),], c('NEE','Rg', 'Tair', 'VPD'), DTS.n=24)
  ) 
})
test_that("Time series not stamped on the (half-)hour",{
  #Shift half-hourly time stamp
  EddyDataShiftedPosix.F <- EddyDataWithPosix.F
  EddyDataShiftedPosix.F$DateTime <- EddyDataShiftedPosix.F$DateTime - (15 * 60)
  expect_error(
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataShiftedPosix.F, c('NEE','Rg', 'Tair', 'VPD'))
  )
  expect_error(
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataShiftedPosix.F[c(F,T),], c('NEE','Rg', 'Tair', 'VPD'), DTS.n=24)
  )
})
test_that("Time series not in full days and starting at end of first (half-)hour (and ending at midnight).",{
  expect_warning( 
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[1:(nrow(EddyDataWithPosix.F)-1),], c('NEE','Rg', 'Tair', 'VPD'))
    )
  expect_warning( 
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[c(F,T),][1:(nrow(EddyDataWithPosix.F[c(F,T),])-1),], c('NEE','Rg', 'Tair', 'VPD'), DTS.n=24)
  )
  expect_warning( 
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[2:(nrow(EddyDataWithPosix.F)-47),], c('NEE','Rg', 'Tair', 'VPD'))  
    )
  expect_warning( 
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[c(F,T),][2:(nrow(EddyDataWithPosix.F[c(F,T),])-23),], c('NEE','Rg', 'Tair', 'VPD'), DTS.n=24)
  )
})
test_that("Time series less than three month of data",{
  expect_error( 
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[1:(48*(3*30-1)),], c('NEE','Rg', 'Tair', 'VPD'))
  )
  expect_error( 
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[c(F,T),][1:(24*(3*30-1)),], c('NEE','Rg', 'Tair', 'VPD'), DTS.n=24)
  )
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Test sGetData",{
  EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD'))
  Data.F <- EddyProc.C$sGetData()
  expect_that(Data.F[,1], equals(EddyProc.C$sDATA[,1]))
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Test sMDSGapFill",{
  EddyDataWithPosix.F <- cbind(EddyDataWithPosix.F, QF=c(1,0,1,0,1,0,0,0,0,0))
  EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[1:(48*3*30),], c('NEE','Rg', 'Tair', 'VPD', 'QF'))
  expect_error( #Not existing variable
    EddyProc.C$sMDSGapFill('fee','QF', 0, Verbose.b=F)
  )
  expect_warning( #Empty variable to fill
    EddyProc.C$sMDSGapFill('Rg','QF', 100 , Verbose.b=F)
  )
  EddyProc.C$sMDSGapFill('NEE', Verbose.b=F)
  EddyProc.C$sMDSGapFill('Tair','QF', 0, Verbose.b=F)
  Results.F <- EddyProc.C$sExportResults()
  # Regression test of results
  expect_that(Results.F[1,'NEE_fnum'], equals(54)) #Equal to 53 with old MR PV-Wave congruent settings
  expect_that(Results.F[1,'Tair_fnum'], equals(173)) #Equal to 96 with old MR PV-Wave congruent settings
  # Shorter version for hourly  
  EddyHour.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[c(F,T),][1:(24*3*30),], c('NEE','Rg', 'Tair', 'VPD', 'QF'), DTS.n=24)
  EddyHour.C$sMDSGapFill('Tair','QF', 0, Verbose.b=F)
  Results.F <- EddyHour.C$sExportResults()
  expect_that(Results.F[1,'Tair_fnum'], equals(124)) #Equal to 68 with old MR PV-Wave congruent settings
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that("Test sMDSGapFillUStar",{
			# single value
			EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[1:(48*3*30),], c('NEE','Rg', 'Tair', 'VPD','Ustar'))
			EddyProc.C$sMDSGapFillUStar('NEE', Verbose.b=F, ustar.m=0.42)
			Results.F <- EddyProc.C$sExportResults()
			expect_true( "NEE_f" %in% colnames(Results.F) ) # unchanged column name
			#
			# several values 
			EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[1:(48*3*30),], c('NEE','Rg', 'Tair', 'VPD','Ustar'))
			suffix.v <-  c("05","95")
			EddyProc.C$sMDSGapFillUStar('NEE', Verbose.b=F, ustar.m=c(0.38,0.42), suffix.v =suffix.v )
			Results.F <- EddyProc.C$sExportResults()
			expect_true( all(c("NEE05_f","NEE95_f") %in% colnames(Results.F)) ) # unchanged column name
			EddyProc.C$sMDSGapFillUStar('Tair', Verbose.b=F, ustar.m=c(0.38,0.42), suffix.v =suffix.v )
			#suffix.s <- suffix.v[1]
			for( suffix.s in suffix.v ){
				EddyProc.C$sMRFluxPartition(
						, Lat_deg.n=51, Long_deg.n=7, TimeZone_h.n=1
						, suffix.s=suffix.s
					)
			}
			Results.F <- EddyProc.C$sExportResults()
			# TODO: Test still fails
			# expect_true( all(c("GPP05_f","GPP95_f","Reco05","Reco95") %in% colnames(Results.F)) ) # unchanged column name
			#with(Results.F,  plot( NEE05_f ~ NEE95_f))
			#
			# several values for several years
			EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[1:(48*3*30),], c('NEE','Rg', 'Tair', 'VPD','Ustar'))
			ustar.m = matrix(c(0.38,0.42), byrow=TRUE, ncol=2, nrow=2, dimnames=list(years=c(1998,1999),probs=c("05","95") ))
			EddyProc.C$sMDSGapFillUStar('NEE', Verbose.b=F, ustar.m=ustar.m, suffix.v = colnames(ustar.m) )
			Results.F <- EddyProc.C$sExportResults()
			expect_true( all(c("NEE05_f","NEE95_f") %in% colnames(Results.F)) ) # unchanged column name
			# NA case
			EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[1:(48*3*30),], c('NEE','Rg', 'Tair', 'VPD','Ustar'))
			ustar.m = matrix(c(NA, NA, 0.38,0.42), byrow=TRUE, ncol=2, nrow=2, dimnames=list(years=c(1998,1999),probs=c("05","95") ))
			EddyProc.C$sMDSGapFillUStar('NEE', Verbose.b=F, ustar.m=ustar.m, suffix.v = colnames(ustar.m) )
			Results.F <- EddyProc.C$sExportResults()
			expect_true( all(c("NEE05_f","NEE95_f") %in% colnames(Results.F)) ) # unchanged column name
		})

