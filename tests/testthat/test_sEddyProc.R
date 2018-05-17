#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for sEddyProc functions +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM
#require(testthat)
context("sEddyProc-Class")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Example is accessible if package is installed, otherwise need to load it from data directory below package root
if( !exists("Example_DETha98")) load("data/Example_DETha98.RData")
EddyData.F <- Example_DETha98

#Include POSIX time stamp column
EddyDataWithPosix.F <- suppressMessages(fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour'))
# construct multiyear dataset
EddyData99.F <- EddyData.F
EddyData99.F$Year <- 1999
EddyDataWithPosix2yr.F <- suppressMessages(fConvertTimeToPosix(rbind(EddyData.F, EddyData99.F), 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour'))
rm( EddyData99.F )
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
  EddyDataWithPosix2.F <- cbind(EddyDataWithPosix.F, QF=c(1,0,1,0,1,0,0,0,0,0))
  EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix2.F[1:(48*3*30),], c('NEE','Rg', 'Tair', 'VPD', 'QF'))
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
  EddyHour.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix2.F[c(F,T),][1:(24*3*30),], c('NEE','Rg', 'Tair', 'VPD', 'QF'), DTS.n=24)
  EddyHour.C$sMDSGapFill('Tair','QF', 0, Verbose.b=F)
  Results.F <- EddyHour.C$sExportResults()
  expect_that(Results.F[1,'Tair_fnum'], equals(124)) #Equal to 68 with old MR PV-Wave congruent settings
})

# see .profileGapFill in develop/profile/profile.R

test_that("Test sMDSGapFillAfterUStar default case",{
			EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD', 'Ustar'))
			uStarTh <- EddyProc.C$sEstUstarThreshold()$uStarTh
			uStar98 <- subset(uStarTh, aggregationMode=="year" & seasonYear==1998, "uStar" )[1,1]
			#EddyProc.C$trace("sMDSGapFillAfterUstar", recover)	#EddyProc.C$untrace("sMDSGapFillAfterUstar")
			EddyProc.C$sMDSGapFillAfterUstar('NEE', FillAll.b = FALSE)
			expect_equal( uStar98, min(EddyProc.C$sDATA$Ustar[ EddyProc.C$sTEMP$NEE_WithUstar_fqc==0 & (EddyProc.C$sDATA$Rg < 10)], na.rm=TRUE), tolerance = 0.05  )
		})

test_that("Test sMDSGapFillAfterUStar single value",{
			EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD', 'Ustar'))
			uStarFixed <- 0.46
			EddyProc.C$sMDSGapFillAfterUstar('NEE', FillAll.b = FALSE, UstarThres.df=uStarFixed)
			expect_equal( uStarFixed, min(EddyProc.C$sDATA$Ustar[ EddyProc.C$sTEMP$NEE_WithUstar_fqc==0 & (EddyProc.C$sDATA$Rg < 10)], na.rm=TRUE), tolerance = 0.05  )
		})

test_that("Test sMDSGapFillAfterUStar error on season mismatch",{
			EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD', 'Ustar'))
			uStarTh <- EddyProc.C$sEstUstarThreshold()$uStarTh
			UstarThres.df <- usGetAnnualSeasonUStarMap(uStarTh)[-1, ,drop=FALSE]
			expect_error(
				EddyProc.C$sMDSGapFillAfterUstar('NEE', UstarThres.df=UstarThres.df, FillAll.b = FALSE)
			)
		})

test_that("Test sMDSGapFillAfterUStar error on na-values",{
			EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD', 'Ustar'))
			uStarTh <- EddyProc.C$sEstUstarThreshold()$uStarTh
			UstarThres.df <- usGetAnnualSeasonUStarMap(uStarTh)
			UstarThres.df[1,2] <- NA
			expect_error(
					EddyProc.C$sMDSGapFillAfterUstar('NEE', UstarThres.df=UstarThres.df, FillAll.b = FALSE)
			)
		})


test_that("Test sMDSGapFillAfterUStarDistr standard and colnames in FluxPartitioning",{
			EddySetups.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
			# Note that for each period a distribution of estimates is obtained, and quantiles are reported
			(uStarRes <- EddySetups.C$sEstUstarThresholdDistribution( nSample=3L ))
			(UstarThres.df <- usGetAnnualSeasonUStarMap(uStarRes))
			EddySetups.C$sMDSGapFillAfterUStarDistr('NEE', UstarThres.df=UstarThres.df, FillAll.b = FALSE)
			# Note the columns with differnt suffixes for different uStar estimates (uStar, U05, U50, U95)
			cNames <- grep("U50", colnames(EddySetups.C$sExportResults()), value = TRUE)
			expect_true( all(c("Ustar_U50_Thres", "Ustar_U50_fqc", "NEE_U50_orig", "NEE_U50_f",
									"NEE_U50_fqc", "NEE_U50_fall", "NEE_U50_fall_qc", "NEE_U50_fnum",
									"NEE_U50_fsd", "NEE_U50_fmeth", "NEE_U50_fwin")
			%in% cNames) )
			#
			EddySetups.C$sMDSGapFill('Tair', FillAll.b = FALSE)
			EddySetups.C$sSetLocationInfo(Lat_deg.n=51.0, Long_deg.n=13.6, TimeZone_h.n=1)
			for( suffix in c('U05', 'U50')){
				EddySetups.C$sMRFluxPartition(Suffix.s = suffix)
			}
			cNames2 <- grep("U50", colnames(EddySetups.C$sExportResults()), value = TRUE)
			expect_true( all(			c("PotRad_U50",	"FP_NEEnight_U50", "FP_Temp_U50"
									, "E_0_U50", "R_ref_U50", "Reco_U50",
									"GPP_U50_f", "GPP_U50_fqc")
									%in% cNames2) )
		})

test_that("Test sMDSGapFillAfterUStarDistr single row",{
			EddySetups.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
			# Note that for each period a distribution of estimates is obtained, and quantiles are reported
			(uStarRes <- EddySetups.C$sEstUstarThresholdDistribution( nSample=3L ))
			# take only the first row, would throw an error in test on season mismatch, but with one row applied for all
			(UstarThres.df <- usGetAnnualSeasonUStarMap(uStarRes)[1, c(1,3,4),drop=FALSE])
			EddySetups.C$sMDSGapFillAfterUStarDistr('NEE', UstarThres.df=UstarThres.df, FillAll.b = FALSE)
			# Note the columns with differnt suffixes for different uStar estimates (uStar, U05, U50, U95)
			cNames <- grep("U50", colnames(EddySetups.C$sExportResults()), value = TRUE)
			expect_true( all(c("Ustar_U50_Thres", "Ustar_U50_fqc", "NEE_U50_orig", "NEE_U50_f",
											"NEE_U50_fqc", "NEE_U50_fall", "NEE_U50_fall_qc", "NEE_U50_fnum",
											"NEE_U50_fsd", "NEE_U50_fmeth", "NEE_U50_fwin")
									%in% cNames) )
		})

