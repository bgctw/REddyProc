#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for sEddyProc functions +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM
#require(testthat)
context("sEddyProc-Class")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Load data
if( sum(grepl("REddyProc", (.packages()))) == 1 ) {  #REddyProc already package loaded
  # data(Example_DETha98)   # Problem with unit row and thus read in as string factors
  Dir.s <- paste(system.file(package='REddyProc'), '/data', sep='')
  EddyData.F <- suppressMessages(fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s))
} else if( file.exists('data/Example_DETha98.txt') ) {
  EddyData.F <- suppressMessages(fLoadTXTIntoDataframe('Example_DETha98.txt','data'))
} else if( file.exists('../../data/Example_DETha98.txt') ) {
  EddyData.F <- suppressMessages(fLoadTXTIntoDataframe('Example_DETha98.txt','../../data'))
} else {
  stop('test_sEddyProc.R::: Data could not be loaded.')
}
#Include POSIX time stamp column
EddyDataWithPosix.F <- suppressMessages(fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='Day', Hour.s='Hour'))

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
test_that("Time series not in equidistant steps",{
  expect_error(
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[c(-50,-60),], c('NEE','Rg', 'Tair', 'VPD'))
  )
})
test_that("Time series not stamped on the half-hour",{
  EddyDataShiftedPosix.F <- EddyDataWithPosix.F
  EddyDataShiftedPosix.F$DateTime <- EddyDataShiftedPosix.F$DateTime - (15 * 60)
  expect_error(
    #Shifted time stamp
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataShiftedPosix.F, c('NEE','Rg', 'Tair', 'VPD'))
  )
})
test_that("Time series less than three month of data",{
  expect_error( 
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[1:3999,], c('NEE','Rg', 'Tair', 'VPD'))
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
  EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[1:4000,], c('NEE','Rg', 'Tair', 'VPD'))
  EddyProc2.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[1:4000,], c('NEE','Rg', 'Tair', 'VPD', 'QF'))
  expect_error( #Not existing variable
    Data2.F <- EddyProc2.C$sMDSGapFill('fee','QF','0', Verbose.b=F)
  )
  expect_error( #Empty variable to fill
    Data2.F <- EddyProc2.C$sMDSGapFill('NEE','QF','100', Verbose.b=F)
  )
  Data.F <- EddyProc.C$sMDSGapFill('NEE', Verbose.b=F)
  expect_that(Data.F[1,'NEE_fnum'], equals(53))
  Data2.F <- EddyProc2.C$sMDSGapFill('NEE','QF','0', Verbose.b=F)
  expect_that(Data2.F[1,'NEE_fnum'], equals(34))
})


