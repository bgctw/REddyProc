#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for sEddyProc functions regarding partioning +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: TW
#require(testthat)
context("sEddyProc-Class partitioning")

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

EddyHour.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F[c(F,T),][1:(24*3*30),], c('NEE','Rg', 'Tair', 'VPD'), DTS.n=24)
EddyHour.C$sMDSGapFill('Tair', Verbose.b=F)
EddyHour.C$sMDSGapFill('NEE', Verbose.b=F)



test_that("sMRFluxPartition Standard",{
            EddyHour.C$sMRFluxPartition( Lat_deg.n=51, Long_deg.n=7, TimeZone_h.n=1 )   
            expect_that( EddyHour.C$sTEMP$E_0[1], equals(133.7, tolerance = .1))
        })


test_that("Using fixed E0",{
            E0 <- 120
            #EddyHour.C$trace("sMRFluxPartition", recover )            # EddyHour.C$untrace("sMRFluxPartition")
            EddyHour.C$sMRFluxPartition( Lat_deg.n=51, Long_deg.n=7, TimeZone_h.n=1, debug.l=list(fixedE0=E0) )   # calling sRegrE0fromShortTerm
            expect_that( EddyHour.C$sTEMP$E_0[1], equals(E0))
            #colnames(EddyHour.C$sTEMP)
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
            expect_true( all(c("NEE05_f","NEE95_f") %in% colnames(Results.F)) ) # column names according to suffix.v
            EddyProc.C$sMDSGapFillUStar('Tair', Verbose.b=F, ustar.m=c(0.38,0.42), suffix.v =suffix.v )
            Results.F <- EddyProc.C$sExportResults()
            expect_true( all(c("Tair05_f","Tair95_f") %in% colnames(Results.F)) ) # column names according to suffix.v
            # test whether sMRFluxPartition can use changed column names
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

