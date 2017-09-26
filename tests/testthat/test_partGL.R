#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for fConvertTimeToPosix functions +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: TW
#require(testthat)
context("partGL")

if( !exists(".partGPAssociateSpecialRows") ) .partGPAssociateSpecialRows <- REddyProc:::.partGPAssociateSpecialRows
if( !exists(".binUstar") ) .binUstar <- REddyProc:::.binUstar

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# create meteo-gapfilled dataset
# cache in ReddyProc example directory
exampleBaseName <- "Example_DETha98_Filled.RData"
if( !length(getExamplePath(exampleBaseName)) ){
	Example_DETha98_Date <- fConvertTimeToPosix(Example_DETha98, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
	Example_DETha98_sDate <- cbind(sDateTime=Example_DETha98_Date$DateTime - 15*60,  Example_DETha98_Date)
	EddyProc.C <- sEddyProc$new('DE-Tha', Example_DETha98_sDate, c('NEE','Rg','Tair','VPD', 'Ustar'))
	EddyProc.C$sSetLocationInfo(Lat_deg.n=51.0, Long_deg.n=13.6, TimeZone_h.n=1)  
	EddyProc.C$sCalcPotRadiation()
	EddyProc.C$sMDSGapFill('NEE', FillAll.b=FALSE)
	EddyProc.C$sMDSGapFill('Rg', FillAll.b=FALSE)
	EddyProc.C$sMDSGapFill('Tair', FillAll.b=FALSE)  	 
	EddyProc.C$sMDSGapFill('VPD', FillAll.b=FALSE)
	Example_DETha98_Filled <- cbind(Example_DETha98_sDate, EddyProc.C$sExportResults() )
	save( Example_DETha98_Filled, file=file.path(system.file(package='REddyProc'), 'examples', exampleBaseName))
}
# 10 days from June from Example_DETha98.txt shipped with REddyProc
load(getExamplePath(exampleBaseName))
dsNEE <- subset(Example_DETha98_Filled, sDateTime >= "1998-06-01 00:15:00 GMT" & sDateTime <= "1998-06-09 21:45:00 GMT")
		
dsNEE$Temp <- dsNEE$Tair_f
dsNEE$isNight <- (dsNEE$Rg_f <= 4 & dsNEE$PotRad_NEW == 0)
dsNEE$isDay=(dsNEE$Rg_f > 4 & dsNEE$PotRad_NEW != 0)

# 8 first days of June from IT-MBo.2005.txt
# 10 days from June from Example_DETha98.txt shipped with REddyProc
.tmp.f <- function(){
	#save(dsNEE, file="tmp/dsNEE_Tharandt.RData")
	load("tmp/dsNEE_Tharandt.RData") # dsNEE
	dsNEE$Temp <- dsNEE$Tair_f
	dsNEE$Rg_f <- dsNEE$Rg
	dsNEE$isNight <- (dsNEE$Rg_f <= 4 & dsNEE$PotRad_NEW == 0)
	dsNEE$isDay=(dsNEE$Rg_f > 4 & dsNEE$PotRad_NEW != 0)
}

.tmp.f2 <- function(){
	# else stop in partitionNEEGL, and grap ds:
	attr(ds$sDateTime, "tzone") <- "UTC"
	dsJune <- ds
	dsNEE <- dsJune[ dsJune$sDateTime >= as.POSIXct("1998-06-01", tz="UTC") & dsJune$sDateTime < as.POSIXct("1998-06-10", tz="UTC")
		, c("sDateTime","NEE_f","NEE_fqc","NEE_fsd","Tair_f","Tair_fqc","VPD_f","VPD_fqc","Rg_f","PotRad_NEW")]
	save(dsNEE, file="tmp/dsNEE_Tharandt.RData")
	dput(dsNEE)
}


		
# regression result from dput in 
resLRCEx1 <- structure(list(iWindow = 1:4, dayStart = c(1L, 3L, 5L, 7L), dayEnd = c(4L, 
						6L, 8L, 9L), iRecStart = c(1L, 97L, 193L, 289L), iRecEnd = c(192L, 
						288L, 384L, 428L), iCentralRec = c(97L, 193L, 289L, 385L), nValidRec = c(98L, 
						98L, 75L, 56L), iMeanRec = c(97L, 194L, 283L, 385L), convergence = c(0L, 
						0L, 0L, 0L), parms_out_range = c(0L, 0L, 0L, 0L), k = c(0.0832223140512019, 
						0.10553126553889, 0.0941326216218045, 0.0733024906142386), beta = c(33.4262908992253, 
						39.3932779964595, 30.9890613347522, 33.4343580245859), alpha = c(0.101181211688144, 
						0.0638917861858381, 0.0895731637684381, 0.110240996451185), RRef = c(4.18758256302895, 
						3.70168220415253, 3.86284152024598, 5.17389942836253), E0 = structure(c(62.633343296311, 
								62.633343296311, 62.633343296311, 62.633343296311), .Dim = c(4L, 
								1L), .Dimnames = list(NULL, NULL)), k_sd = c(0.0181191107399481, 
						0.0213832968870677, 0.020912862135183, 0.0257265995686693), beta_sd = c(2.42442970337725, 
						5.55887367082592, 3.68730944903668, 4.42783528204267), alpha_sd = c(0.0229669073716477, 
						0.014903356702812, 0.0228984931534909, 0.0446236879898696), RRef_sd = c(1.32104929782898, 
						0.989282140039732, 1.08171114445532, 2.1891081447713), E0_sd = structure(c(4.25106879296635, 
								4.22511397480583, 4.22511362599932, 4.46593783063789), .Dim = c(4L, 
								1L), .Dimnames = list(NULL, NULL)), isValid = c(TRUE, TRUE, TRUE, 
						TRUE), resOpt = list(structure(list(thetaOpt = structure(c(0.0832223140512019, 
														33.4262908992253, 0.101181211688144, 4.18758256302895, 62.633343296311
												), .Names = c("k", "beta", "alpha", "RRef", "E0")), iOpt = 1:5, 
										thetaInitialGuess = structure(c(0.05, 24.3542, 0.1, 5.17329116845493, 
														62.633343296311), .Names = c("k", "beta", "alpha", "RRef", 
														"E0")), covParms = structure(c(0.000328302174006501, 0.0221014971127152, 
														-0.000234579653253025, -0.0138942101313942, 0, 0.0221014971127152, 
														5.8778593866179, -0.0217777550251566, -0.185716587265149, 
														0, -0.000234579653253025, -0.0217777550251567, 0.000527478834217845, 
														0.0270259299462335, 0, -0.0138942101313942, -0.18571658726515, 
														0.0270259299462335, 1.74517124729443, 0, 0, 0, 0, 0, 18.0715858825324
												), .Dim = c(5L, 5L), .Dimnames = list(structure(c("k", "beta", 
																		"alpha", "RRef", "E0"), .Names = c("k", "beta", "alpha", 
																		"RRef", "E0")), structure(c("k", "beta", "alpha", "RRef", 
																		"E0"), .Names = c("k", "beta", "alpha", "RRef", "E0")))), 
										convergence = 0L), .Names = c("thetaOpt", "iOpt", "thetaInitialGuess", 
										"covParms", "convergence")), structure(list(thetaOpt = structure(c(0.10553126553889, 
														39.3932779964595, 0.0638917861858381, 3.70168220415253, 62.633343296311
												), .Names = c("k", "beta", "alpha", "RRef", "E0")), iOpt = 1:5, 
										thetaInitialGuess = structure(c(0.05, 24.8403, 0.1, 5.14770049230269, 
														62.633343296311), .Names = c("k", "beta", "alpha", "RRef", 
														"E0")), covParms = structure(c(0.000457245385760478, 0.0986335414045973, 
														-0.000286934069218457, -0.0190583901680855, 0, 0.0986335414045973, 
														30.9010764882017, -0.0733014636241958, -3.50381345502819, 
														0, -0.000286934069218457, -0.0733014636241959, 0.00022211004101125, 
														0.0128553385928291, 0, -0.0190583901680855, -3.50381345502819, 
														0.0128553385928291, 0.978679152601592, 0, 0, 0, 0, 0, 17.8515881000995
												), .Dim = c(5L, 5L), .Dimnames = list(structure(c("k", "beta", 
																		"alpha", "RRef", "E0"), .Names = c("k", "beta", "alpha", 
																		"RRef", "E0")), structure(c("k", "beta", "alpha", "RRef", 
																		"E0"), .Names = c("k", "beta", "alpha", "RRef", "E0")))), 
										convergence = 0L), .Names = c("thetaOpt", "iOpt", "thetaInitialGuess", 
										"covParms", "convergence")), structure(list(thetaOpt = structure(c(0.0941326216218045, 
														30.9890613347522, 0.0895731637684381, 3.86284152024598, 62.633343296311
												), .Names = c("k", "beta", "alpha", "RRef", "E0")), iOpt = 1:5, 
										thetaInitialGuess = structure(c(0.05, 24.1194, 0.1, 5.14770049230269, 
														62.633343296311), .Names = c("k", "beta", "alpha", "RRef", 
														"E0")), covParms = structure(c(0.00043734780268517, 0.0573365880613315, 
														-0.000422983395692104, -0.0202552299638823, 0, 0.0573365880613314, 
														13.5962509729552, -0.0612954107333078, -1.93521924553093, 
														0, -0.000422983395692103, -0.0612954107333078, 0.000524340988700472, 
														0.0218569302946186, 0, -0.0202552299638823, -1.93521924553093, 
														0.0218569302946186, 1.17009900003883, 0, 0, 0, 0, 0, 17.8515851526051
												), .Dim = c(5L, 5L), .Dimnames = list(structure(c("k", "beta", 
																		"alpha", "RRef", "E0"), .Names = c("k", "beta", "alpha", 
																		"RRef", "E0")), structure(c("k", "beta", "alpha", "RRef", 
																		"E0"), .Names = c("k", "beta", "alpha", "RRef", "E0")))), 
										convergence = 0L), .Names = c("thetaOpt", "iOpt", "thetaInitialGuess", 
										"covParms", "convergence")), structure(list(thetaOpt = structure(c(0.0733024906142386, 
														33.4343580245859, 0.110240996451185, 5.17389942836253, 62.633343296311
												), .Names = c("k", "beta", "alpha", "RRef", "E0")), iOpt = 1:5, 
										thetaInitialGuess = structure(c(0.05, 21.665, 0.1, 5.21501814931109, 
														62.633343296311), .Names = c("k", "beta", "alpha", "RRef", 
														"E0")), covParms = structure(c(0.000661857925366655, 0.087957453256686, 
														-0.0010058808771553, -0.0459390136989103, 0, 0.0879574532566859, 
														19.6057252849019, -0.12488001895199, -3.81400043222147, 0, 
														-0.0010058808771553, -0.12488001895199, 0.00199127352981723, 
														0.0912451146654793, 0, -0.0459390136989103, -3.81400043222148, 
														0.0912451146654793, 4.79219446950406, 0, 0, 0, 0, 0, 19.9446007071226
												), .Dim = c(5L, 5L), .Dimnames = list(structure(c("k", "beta", 
																		"alpha", "RRef", "E0"), .Names = c("k", "beta", "alpha", 
																		"RRef", "E0")), structure(c("k", "beta", "alpha", "RRef", 
																		"E0"), .Names = c("k", "beta", "alpha", "RRef", "E0")))), 
										convergence = 0L), .Names = c("thetaOpt", "iOpt", "thetaInitialGuess", 
										"covParms", "convergence"))), E0_bootstrap_sd = c(4.25106879296635, 
						4.22511397480583, 4.22511362599932, 4.46593783063789), RRef_night = c(5.17329116845493, 
						5.14770049230269, 5.14770049230269, 5.21501814931109)), .Names = c("iWindow", 
				"dayStart", "dayEnd", "iRecStart", "iRecEnd", "iCentralRec", 
				"nValidRec", "iMeanRec", "convergence", "parms_out_range", "k", 
				"beta", "alpha", "RRef", "E0", "k_sd", "beta_sd", "alpha_sd", 
				"RRef_sd", "E0_sd", "isValid", "resOpt", "E0_bootstrap_sd", "RRef_night"
		), row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"
		))

resLRCEx1Nonrectangular <- structure(list(iWindow = 1:4, dayStart = c(1L, 3L, 5L, 7L), dayEnd = c(4L, 
						6L, 8L, 9L), iRecStart = c(1L, 97L, 193L, 289L), iRecEnd = c(192L, 
						288L, 384L, 428L), iCentralRec = c(97L, 193L, 289L, 385L), nValidRec = c(98L, 
						98L, 75L, 56L), iMeanRec = c(97L, 194L, 283L, 385L), convergence = c(0L, 
						1005L, 0L, 0L), parms_out_range = c(0L, NA, 0L, 0L), k = c(0.083168648011042, 
						NA, 0.098565862707431, 0.0736345011666004), beta = c(30.839628687034, 
						NA, 30.1678300447587, 33.4145830069625), alpha = c(0.0825555267791741, 
						NA, 0.0810413516218528, 0.109517964714619), RRef = c(3.64569296211628, 
						NA, 3.48751584956092, 5.22043422214259), E0 = structure(c(62.633343296311, 
								62.633343296311, 62.633343296311, 62.633343296311), .Dim = c(4L, 
								1L), .Dimnames = list(NULL, NULL)), logitconv = c(-0.534563545055753, 
						NA, -1.61563346579258, -3.08616360625019), k_sd = c(0.0177304205815635, 
						NA, 0.0221003154605623, 0.0290277851827706), beta_sd = c(6.47216716214799, 
						NA, 4.89915999234226, 4.82983939824434), alpha_sd = c(0.0379444212322333, 
						NA, 0.027496504523545, 0.051069790354041), RRef_sd = c(1.57025981089475, 
						NA, 1.1111318580789, 2.45830951563389), E0_sd = structure(c(4.25106879296635, 
								4.22511397480583, 4.22511362599932, 4.46593783063789), .Dim = c(4L, 
								1L), .Dimnames = list(NULL, NULL)), logitconv_sd = c(3.02392209198105, 
						NA, 3.98858344328946, 3.32115891501617), isValid = c(TRUE, NA, 
						TRUE, TRUE), resOpt = list(structure(list(thetaOpt = structure(c(0.083168648011042, 
														30.839628687034, 0.0825555267791741, 3.64569296211628, 62.633343296311, 
														-0.534563545055753), .Names = c("k", "beta", "alpha", "RRef", 
														"E0", "logitconv")), iOpt = c(1, 2, 3, 4, 6, 5), thetaInitialGuess = structure(c(0.05, 
														24.3542, 0.1, 5.17329116845493, 62.633343296311, 1.09861228866811
												), .Names = c("k", "beta", "alpha", "RRef", "E0", "logitconv"
												)), covParms = structure(c(0.000314367813999132, 0.0265144720624879, 
														-9.39511398356683e-05, -0.00958153592050263, 0, -0.00446189853212235, 
														0.0265144720624885, 41.8889477747868, 0.199301773957873, 6.33935502913162, 
														0, -18.4990539185711, -9.39511398356615e-05, 0.199301773957874, 
														0.00143977910264916, 0.0519981219366233, 0, -0.103182252423609, 
														-0.00958153592050246, 6.33935502913168, 0.0519981219366235, 2.46571587371122, 
														0, -3.05107004068001, 0, 0, 0, 0, 18.0715858825324, 0, -0.00446189853212271, 
														-18.4990539185711, -0.103182252423608, -3.05107004067998, 0, 
														9.14410481837104), .Dim = c(6L, 6L), .Dimnames = list(structure(c("k", 
																		"beta", "alpha", "RRef", "E0", "logitconv"), .Names = c("k", 
																		"beta", "alpha", "RRef", "E0", "logitconf")), structure(c("k", 
																		"beta", "alpha", "RRef", "E0", "logitconv"), .Names = c("k", 
																		"beta", "alpha", "RRef", "E0", "logitconf")))), convergence = 0L), .Names = c("thetaOpt", 
										"iOpt", "thetaInitialGuess", "covParms", "convergence")), NULL, 
						structure(list(thetaOpt = structure(c(0.098565862707431, 
														30.1678300447587, 0.0810413516218528, 3.48751584956092, 62.633343296311, 
														-1.61563346579258), .Names = c("k", "beta", "alpha", "RRef", 
														"E0", "logitconv")), iOpt = c(1, 2, 3, 4, 6, 5), thetaInitialGuess = structure(c(0.05, 
														24.1194, 0.1, 5.14770049230269, 62.633343296311, 1.09861228866811
												), .Names = c("k", "beta", "alpha", "RRef", "E0", "logitconv"
												)), covParms = structure(c(0.000488423943456369, 0.0537352574710337, 
														-0.000428737278036794, -0.0212633400281008, 0, 0.00875123254723377, 
														0.0537352574710336, 24.001768630567, 0.00685190459513276, 
														-0.60636944280428, 0, -12.8763708661608, -0.000428737278036795, 
														0.00685190459513258, 0.000756057761013328, 0.0256900144587214, 
														0, -0.0749819487065727, -0.0212633400281008, -0.606369442804284, 
														0.0256900144587214, 1.23461400603787, 0, -1.57258070932799, 
														0, 0, 0, 0, 17.8515851526051, 0, 0.00875123254723385, -12.8763708661608, 
														-0.0749819487065728, -1.572580709328, 0, 15.9087978840828
												), .Dim = c(6L, 6L), .Dimnames = list(structure(c("k", "beta", 
																		"alpha", "RRef", "E0", "logitconv"), .Names = c("k", "beta", 
																		"alpha", "RRef", "E0", "logitconf")), structure(c("k", "beta", 
																		"alpha", "RRef", "E0", "logitconv"), .Names = c("k", "beta", 
																		"alpha", "RRef", "E0", "logitconf")))), convergence = 0L), .Names = c("thetaOpt", 
										"iOpt", "thetaInitialGuess", "covParms", "convergence")), 
						structure(list(thetaOpt = structure(c(0.0736345011666004, 
														33.4145830069625, 0.109517964714619, 5.22043422214259, 62.633343296311, 
														-3.08616360625019), .Names = c("k", "beta", "alpha", "RRef", 
														"E0", "logitconv")), iOpt = c(1, 2, 3, 4, 6, 5), thetaInitialGuess = structure(c(0.05, 
														21.665, 0.1, 5.21501814931109, 62.633343296311, 1.09861228866811
												), .Names = c("k", "beta", "alpha", "RRef", "E0", "logitconv"
												)), covParms = structure(c(0.000842612312617077, 0.110943571957266, 
														-0.00132882714081546, -0.0608767360022503, 0, 0.00375523057937961, 
														0.110943571957266, 23.3273486128332, -0.159209227029033, 
														-5.54998072404295, 0, -2.72022001016764, -0.00132882714081546, 
														-0.159209227029033, 0.0026081234868057, 0.11884992015202, 
														0, -0.0307981045789783, -0.0608767360022503, -5.54998072404293, 
														0.11884992015202, 6.04328567465613, 0, -0.9707144736349, 
														0, 0, 0, 0, 19.9446007071226, 0, 0.00375523057937956, -2.72022001016764, 
														-0.0307981045789782, -0.970714473634895, 0, 11.0300965387914
												), .Dim = c(6L, 6L), .Dimnames = list(structure(c("k", "beta", 
																		"alpha", "RRef", "E0", "logitconv"), .Names = c("k", "beta", 
																		"alpha", "RRef", "E0", "logitconf")), structure(c("k", "beta", 
																		"alpha", "RRef", "E0", "logitconv"), .Names = c("k", "beta", 
																		"alpha", "RRef", "E0", "logitconf")))), convergence = 0L), .Names = c("thetaOpt", 
										"iOpt", "thetaInitialGuess", "covParms", "convergence"))), 
				E0_bootstrap_sd = c(4.25106879296635, NA, 4.22511362599932, 
						4.46593783063789), RRef_night = c(5.17329116845493, 5.14770049230269, 
						5.14770049230269, 5.21501814931109)), .Names = c("iWindow", 
				"dayStart", "dayEnd", "iRecStart", "iRecEnd", "iCentralRec", 
				"nValidRec", "iMeanRec", "convergence", "parms_out_range", "k", 
				"beta", "alpha", "RRef", "E0", "logitconv", "k_sd", "beta_sd", 
				"alpha_sd", "RRef_sd", "E0_sd", "logitconv_sd", "isValid", "resOpt", 
				"E0_bootstrap_sd", "RRef_night"), row.names = c(NA, -4L), class = c("tbl_df", 
				"tbl", "data.frame"))

		
				
test_that("replaceMissingSdByPercentage",{
			n <- 100
			x <- rnorm(n,10)
			sdX <- sdX0 <- pmax(0.7, rnorm(n, 10*0.1 ))  
			iMissing <- sample(n, 20)
			sdX[iMissing] <- NA
			sdXf <- replaceMissingSdByPercentage(sdX,x, 0.2, 1.7)
			#plot( sdXf ~ x, col="blue" );points( sdX ~ x )
			expect_equal( sdXf[-iMissing], sdX[-iMissing])
			expect_true( all(is.finite(sdXf)))
			expect_true( all(sdXf[iMissing] >= 1.7))
			expect_true( all(sdXf[iMissing] >= 0.2*x[iMissing]))
			#
			sdX <- sdX0
			iMissing <- TRUE
			sdX[iMissing] <- NA
			sdXf <- replaceMissingSdByPercentage(sdX,x, 0.2, 1.7)
			#plot( sdXf ~ x, col="blue" );points( sdX ~ x )
			expect_true( all(is.finite(sdXf)))
			expect_true( all(sdXf[iMissing] >= 1.7))
			expect_true( all(sdXf[iMissing] >= 0.2*x[iMissing]))
			#
			sdX <- sdX0 
			iMissing <- sample(n, 20)
			sdX[iMissing] <- NA
			sdXf <- replaceMissingSdByPercentage(sdX,x, NA, 1.7)
			expect_true( all(sdXf[iMissing] == 1.7))
			#plot( sdXf ~ x, col="blue" );points( sdX ~ x )
			sdXf <- replaceMissingSdByPercentage(sdX,x, 0.2, NA)
			expect_true( all(sdXf[iMissing] == 0.2*x[iMissing]))
			sdXf <- replaceMissingSdByPercentage(sdX,x, NA, NA)
			expect_true( all(is.na(sdXf[iMissing])))
		})				
		
test_that("estimating temperature sensitivity oneWindow are in accepted range",{
			dss <- dsNEE[ dsNEE$Rg_f <= 0 & dsNEE$PotRad_NEW <= 0 & as.POSIXlt(dsNEE$sDateTime)$mday %in% 1:8, ]
			dss <- dss[ order(dss$Temp), ]
			dss$NEE <- dss$NEE_f
			resE0 <- partGLEstimateTempSensInBoundsE0Only(dss$NEE_f, dss$Temp+273.15)
			expect_true( resE0$E0 >= 50 && resE0$E0 < 400 )
			medianResp <- median(dss$NEE_f,na.rm=TRUE)
			expect_true( abs(resE0$RRefFit - medianResp)/medianResp < 0.2 )
			E0Win <- as.data.frame(resE0)
			res <- partGLFitNightRespRefOneWindow( dss, data.frame(iWindow=1L), E0Win=E0Win)
			RRef <- res[1]
			expect_true( RRef >= 0)
			.tmp.plot <- function(){
				plot( NEE_f ~ Temp, dss)		# FP_VARnight negative?
				lines( fLloydTaylor(RRef, resE0$E0, dss$Temp+273.15, T_ref.n=273.15+15) ~ dss$Temp)#
			}
		})

test_that("estimating temperature sensitivity on record with some freezing temperatures",{
			dss <- dsNEE[ dsNEE$Rg_f <= 0 & dsNEE$PotRad_NEW <= 0 & as.POSIXlt(dsNEE$sDateTime)$mday %in% 1:8, ]
			dss$NEE <- dss$NEE_f
			dss <- dss[ order(dss$Temp), ]
			dss$Temp <- dss$Temp - dss$Temp[ nrow(dss)-14L ] -1	# only the last 13 records will have temperature above -1degC
			isValid <- isValidNightRecord(dss)
			expect_true(sum(isValid) >= 13 && sum(isValidNightRecord(dss)) < nrow(dss) )
			#
			resE0 <- partGLEstimateTempSensInBoundsE0Only(dss$NEE_f[isValid], dss$Temp[isValid]+273.15)
			expect_true( is.na(resE0$E0) )
			.tmp.f <- function(){
				expect_true( resE0$E0 >= 50 && resE0$E0 <= 400 )
				medianResp <- median(dss$NEE_f[isValid],na.rm=TRUE)
				expect_true( abs(resE0$RRefFit - medianResp)/medianResp < 0.2 )
				E0Win <- as.data.frame(resE0)
				res <- partGLFitNightRespRefOneWindow( dss, data.frame(iWindow=1L), E0Win=E0Win)
				RRef <- res[[2]]$RRef[1]
				expect_true( RRef >= 0 )
			}
			.tmp.plot <- function(){
				plot( NEE_f ~ Temp, dss)		# FP_VARnight negative?
				points( NEE_f ~ Temp, dss[isValid,], col="red")
				lines( fLloydTaylor(RRef, resE0$E0, dss$Temp+273.15, T_ref.n=273.15+15) ~ dss$Temp)#
			}
		})

test_that("applyWindows",{
			nRec <- nrow(dsNEE)
			nRecInDay <- 10L
			ds <- within(dsNEE, {
						iRec <-1:nRec
						iDayOfRec <- ((c(1:nRec)-1L) %/% nRecInDay)+1L 			# specifying the day for each record assuming equidistand records
					})
			fReportTime <- function(dss, winInfo, prevRes){
				nRecS <- nrow(dss)
				list( res2=data.frame(
								startRec=dss$iRec[1]	
								,endRec=dss$iRec[nRecS]
								,startDay=dss$iDayOfRec[1]
								,endDay=dss$iDayOfRec[nRecS]
						)
						,sumRes = prevRes$sumRes +1L
				)
			}
			prevRes <- list(sumRes=0)
			fReportTime(ds,0,prevRes)
			resApply <- applyWindows( ds, fReportTime, prevRes, winSizeInDays=6L, nRecInDay=nRecInDay )	# larger than reference window of 4 days
			#resApply <- applyWindows( ds[1:41,], fReportTime, prevRes, winSizeInDays=6L, nRecInDay=nRecInDay )	# larger than reference window of 4 days
			res <- cbind( resApply$winInfo, tmp <- do.call( rbind, lapply(resApply$resFUN, "[[", 1L)))
			nRecRes <- nrow(res)
			expect_equal( res$dayStart, res$startDay )	
			expect_equal( res$dayEnd, res$endDay )	
			expect_equal( res$iRecStart, res$startRec )	
			expect_equal( res$iRecEnd, res$endRec )	
			#
			expect_true( all(diff(res$startDay[-1])==2L))	# shifted starting day
			# day boundary before startRec
			expect_true( all((ds$iDayOfRec[res$startRec[-1]] - ds$iDayOfRec[res$startRec[-1]-1])==1L))   
			# day boundary after endRec
			expect_true( all((ds$iDayOfRec[res$startRec[-nRecRes]]+1 - ds$iDayOfRec[res$startRec[-nRecRes]])==1L))
			# prevRes accumulated
			expect_equal( resApply$resFUN[[nRecRes]]$sumRes, nrow(res) )
		})

test_that("simplifyApplyWindows",{
			nRec <- nrow(dsNEE)
			nRecInDay <- 10L
			ds <- within(dsNEE, {
						iRec <-1:nRec
						iDayOfRec <- ((c(1:nRec)-1L) %/% nRecInDay)+1L 			# specifying the day for each record assuming equidistand records
					})
			fReportTimeSimple <- function(dss, winInfo, prevRes=list()){
				nRecS <- nrow(dss)
				c(
								startRec=dss$iRec[1]	
								,endRec=dss$iRec[nRecS]
								,startDay=dss$iDayOfRec[1]
								,endDay=dss$iDayOfRec[nRecS]
						)
			}
			fReportTimeSimple(ds,0)
			resApply <- applyWindows( ds, fReportTimeSimple, winSizeInDays=6L, nRecInDay=nRecInDay )	# larger than reference window of 4 days
			res <- simplifyApplyWindows(resApply)
			#resApply <- applyWindows( ds[1:41,], fReportTime, prevRes, winSizeInDays=6L, nRecInDay=nRecInDay )	# larger than reference window of 4 days
			nRecRes <- nrow(res)
			expect_equal( res$dayStart, res$startDay )	
			expect_equal( res$dayEnd, res$endDay )	
			expect_equal( res$iRecStart, res$startRec )	
			expect_equal( res$iRecEnd, res$endRec )
			#
			# repeat with function returning a single-row data.frame
			fReportTimeSimpleDs <- function(dss, winInfo, prevRes=list()){
				nRecS <- nrow(dss)
				data.frame(
						startRec=dss$iRec[1]	
						,endRec=dss$iRec[nRecS]
						,startDay=dss$iDayOfRec[1]
						,endDay=dss$iDayOfRec[nRecS]
				)
			}
			fReportTimeSimpleDs(ds,0)
			resApply <- applyWindows( ds, fReportTimeSimpleDs, winSizeInDays=6L, nRecInDay=nRecInDay )	# larger than reference window of 4 days
			resDs <- simplifyApplyWindows(resApply)
			expect_equal(res, resDs )
		})

test_that("estimating temperature sensitivity windows outputs are in accepted range",{
			dss <- dsNEE[ dsNEE$Rg_f <= 0 & dsNEE$PotRad_NEW <= 0 & as.POSIXlt(dsNEE$sDateTime)$mday %in% 1:12, ]
			dss$NEE <- dss$NEE_f
			dss <- dss[ order(dss$Temp), ]
			res <- simplifyApplyWindows( applyWindows(dss, partGLFitNightTempSensOneWindow, prevRes=data.frame(E0=NA)
					, winSizeInDays=12L
			#,controlGLPart=controlGLPart	
			))
			#res <- partGLEstimateTempSensInBounds(dss$NEE_f, dss$Temp+273.15)
			expect_true( res$E0 >= 50 && res$E0 <= 400 )
			expect_true( res$RRefFit > 0 )
		})


test_that("partGLFitLRCWindows outputs are in accepted range",{
			ds <- partGLExtractStandardData(dsNEE)
			#
			#yday <- as.POSIXlt(dsNEE$sDateTime)$yday
			dsTempSens <- dsTempSens0 <- partGLFitNightTimeTRespSens( ds
					, nRecInDay=48L
					, controlGLPart=partGLControl()
			)
			lrcFitter <- RectangularLRCFitter()
			#lrcFitter <- NonrectangularLRCFitter()
			resFits <- partGLFitLRCWindows(ds, nRecInDay=48L, dsTempSens=dsTempSens, controlGLPart=partGLControl(nBootUncertainty=10L), lrcFitter=lrcFitter)
			expect_true( var(resFits$k) > 0)
			expect_true( all( sapply(resFits$resOpt, length) != 0 ))
			.tmp.f <- function(){
				# in order to replicate, use nBoot=0L 
				resParms0 <- partGLFitLRCWindows(ds, nRecInDay=48L, dsTempSens=dsTempSens, controlGLPart=partGLControl(nBootUncertainty=0L), lrcFitter=lrcFitter)
				#resParms0 <- partGLFitLRCWindows(ds, nRecInDay=48L, dsTempSens=dsTempSens, controlGLPart=partGLControlLasslopCompatible(), lrcFitter=lrcFitter)				
				iTooMany <- which(resParms0$iRecStart >= nrow(ds)-2*48)
				if( length(iTooMany)){
					resParms0 <- slice(resParms0,-iTooMany)
				}
				# regression result for resLRCEx1
				# resLRCEx1 <- resParms0
				# resLRCEx1Nonrectangular <- resParms0
				dput(resParms0)
			}
			# check the conditions of Lasslop10 Table A1
			expect_true( all(resFits$E0 >= 50 & resFits$E0 <= 400) )
			expect_true( all(resFits$RRef > 0) )
			expect_true( all(resFits$alpha >= 0 ) )
			expect_true( all(resFits$alpha[-1] < 0.22) )	# first value may be greater, due to previous estimate
			expect_true( all(resFits$beta >= 0 & resFits$beta < 250) )
			expect_true( all(ifelse(resFits$beta > 100, resFits$beta_sd < resFits$beta, TRUE) ))
			expect_true( all(resFits$k >= 0) )
			expect_true( !all(is.na(resFits$RRef_sd)))
			expect_true( all(resFits$iMeanRec < nrow(ds)) )
			expect_true( all(resFits$iCentralRec < nrow(ds)) )
})

test_that(".partGPAssociateSpecialRows correct next lines",{
			expect_error(
					res <- .partGPAssociateSpecialRows(integer(0),9)
			)
			res <- .partGPAssociateSpecialRows(c(3,6,7,9),12)
			expect_true( all(res$wBefore+res$wAfter == 1)) 
			# special rows
			expect_equal( c(iRec=3,iSpecialBefore=1L,iSpecialAfter=1L,iBefore=3, iAfter=3, wBefore=0.5, wAfter=0.5), unlist(res[3,])) 
			expect_equal( c(iRec=6,iSpecialBefore=2L,iSpecialAfter=2L,iBefore=6, iAfter=6, wBefore=0.5, wAfter=0.5), unlist(res[6,]))
			# first rows and last rows
			expect_equal( c(iRec=1,iSpecialBefore=1L,iSpecialAfter=1L,iBefore=3, iAfter=3, wBefore=0.5, wAfter=0.5), unlist(res[1,])) 
			expect_equal( c(iRec=12,iSpecialBefore=4L,iSpecialAfter=4L,iBefore=9, iAfter=9, wBefore=0.5, wAfter=0.5), unlist(res[12,])) 
			# weights after and before special row 6
			expect_equal( rep(3,2), unlist(res[4:5,"iBefore"])) 
			expect_equal( rep(6,2), unlist(res[4:5,"iAfter"])) 
			expect_equal( rep(1,2), unlist(res[4:5,"iSpecialBefore"])) 
			expect_equal( rep(2,2), unlist(res[4:5,"iSpecialAfter"])) 
			expect_equal( (2:1)/3, unlist(res[4:5,"wBefore"])) 
			expect_equal( (1:2)/3, unlist(res[4:5,"wAfter"])) 
			# test last row is special
			res <- .partGPAssociateSpecialRows(c(3,6,7,9),9)
		})
			
testGLInterpolateFluxes <- function( lrcFitter, resEx ){
			tmp <- partGLInterpolateFluxes( dsNEE$Rg_f, dsNEE$VPD_f, dsNEE$Temp, resEx, lrcFitter=lrcFitter)
			expect_equal( nrow(dsNEE), nrow(tmp) )
			.tmp.plot <- function(){
				tmp$time <- dsNEE$sDateTime
				plot( Reco ~ time, tmp)
				plot( GPP ~ time, tmp)
			}
			expect_true( all(c("GPP","Reco") %in% names(tmp) ))
			tmp <- partGLInterpolateFluxes( dsNEE$Rg_f, dsNEE$VPD_f, dsNEE$Temp, resEx
							,controlGLPart=partGLControl(isSdPredComputed=TRUE), lrcFitter=lrcFitter)
			expect_equal( nrow(dsNEE), nrow(tmp) )
			expect_true( all(c("GPP","Reco") %in% names(tmp) ))
			expect_true( all(c("sdGPP","sdReco") %in% names(tmp) ))
		}
test_that("partGLInterpolateFluxes runs with rectangular LRCFitter",{
			lrcFitter=RectangularLRCFitter(); resEx <- resLRCEx1
			testGLInterpolateFluxes( lrcFitter, resEx)			
		})
test_that("partGLInterpolateFluxes runs with rectangular LRCFitter",{
			lrcFitter=NonrectangularLRCFitter(); resEx <- resLRCEx1Nonrectangular
			testGLInterpolateFluxes( lrcFitter, resEx)			
		})
			

#resLRCEx1
test_that("partitionNEEGL",{
			dsNEE1 <- dsNEE
			resEx <- resLRCEx1
			#DoY.V.n <- as.POSIXlt(dsNEE1$sDateTime)$yday + 1L
			#Hour.V.n <- as.POSIXlt(dsNEE1$sDateTime)$hour + as.POSIXlt(dsNEE1$sDateTime)$min/60
			#dsNEE1$PotRad_NEW <- fCalcPotRadiation(DoY.V.n, Hour.V.n, Lat_deg.n=45.0, Long_deg.n=1, TimeZone_h.n=0 )
			tmp <- partitionNEEGL( dsNEE1,RadVar.s='Rg_f')
			#tmp <- partitionNEEGL( dsNEE1,RadVar.s='Rg_f', controlGLPart=partGLControl(nBootUncertainty=0L, isAssociateParmsToMeanOfValids=FALSE))
			expect_equal( nrow(dsNEE1), nrow(tmp) )
			#tmp[ is.finite(tmp$FP_beta), ]	# note FP_dRecPar is not zero, because iCentralRec != iMeanRec
			#iPar <- which(is.finite(tmp$FP_E0))
			#plot( tmp$FP_beta[iPar] ~ dsNEE1$sDateTime[iPar],type="l", ylim=range(c(tmp$FP_beta[iPar],tmp$FP_GPP2000[iPar]))); lines( tmp$FP_GPP2000[iPar] ~ dsNEE1$sDateTime[iPar], col="red")
			#
			# now test with different suffix: u50
			dsNEE2 <- dsNEE 
			names(dsNEE2)[ match(c("NEE_f", "NEE_fqc", "NEE_fsd"),names(dsNEE2))] <- c("NEE_u50_f", "NEE_u50_fqc", "NEE_u50_fsd")
			tmp <- partitionNEEGL( dsNEE2, RadVar.s='Rg_f', Suffix.s="u50", controlGLPart=partGLControl(nBootUncertainty=0L, isAssociateParmsToMeanOfValids=FALSE) )
			#tmp <- partitionNEEGL( dsNEE2, RadVar.s='Rg_f', Suffix.s="u50", controlGLPart=partGLControl(isAssociateParmsToMeanOfValids=FALSE) )
			expect_equal( nrow(dsNEE1), nrow(tmp) )
			expect_true( all(is.finite(tmp$GPP_DT_u50)))
			expect_true( all(tmp$GPP_DT_u50 >= 0))
			expect_true( all(tmp$GPP_DT_u50 < 250))
			expect_true( all(tmp$Reco_DT_u50 < 10))
			expect_true( all(tmp$Reco_DT_u50 > 0))
			expect_true( all(tmp$Reco_DT_u50_SD > 0))
			expect_true( all(tmp$GPP_DT_u50_SD >= 0))
			expect_true( all(abs(diff(tmp$Reco_DT_u50)) < 0.6))	#smooth
			# reporting good values at central records
			# tmp[resEx$iCentralRec,]
			iRowsOpt <- which(is.finite(tmp$FP_E0))
			expect_true( length(iRowsOpt) == nrow(resEx) ) 
			expect_true( all((tmp$FP_alpha[resEx$iCentralRec] - resEx$alpha)[resEx$parms_out_range==0L] < 1e-2) )
			.tmp.plot <- function(){
				tmp$time <- dsNEE1$sDateTime
				plot( Reco_DT_u50 ~ time, tmp)
				#plot( diff(Reco_DT_u50) ~ time[-1], tmp)
				plot( GPP_DT_u50 ~ time, tmp)
				plot( FP_RRef_Night ~ time, tmp)
				plot( FP_RRef ~ FP_RRef_Night, tmp)
			}
		})

test_that("partitionNEEGL with Lasslop options",{
			dsNEE1 <- dsNEE
			#ds <- data.frame( NEE=dsNEE1$NEE_f, sdNEE=dsNEE1$NEE_fsd, Rg=dsNEE1$Rg_f, VPD=dsNEE1$VPD_f, Temp=dsNEE1$Temp, isDay=dsNEE1$isDay, isNight=dsNEE$isNight )
			resEx <- resLRCEx1
			dsNEE2 <- dsNEE 
			partGLControl()
			names(dsNEE2)[ match(c("NEE_f", "NEE_fqc", "NEE_fsd"),names(dsNEE2))] <- c("NEE_u50_f", "NEE_u50_fqc", "NEE_u50_fsd")
			dsNEE2$NEE_u50_fsd[24] <- NA
			tmp <- partitionNEEGL( dsNEE2, RadVar.s='Rg_f', Suffix.s="u50"
				, controlGLPart=partGLControlLasslopCompatible() )
			tmp$sDateTime <- dsNEE2$sDateTime
			tmp$iRec <- 1:nrow(tmp)
			#subset(tmp, is.finite(FP_errorcode))
			#tmp <- partitionNEEGL( dsNEE2, RadVar.s='Rg_f', Suffix.s="u50", controlGLPart=partGLControl(isAssociateParmsToMeanOfValids=FALSE) )
			expect_equal( nrow(dsNEE1), nrow(tmp) )
			expect_true( all(is.finite(tmp$GPP_DT_u50)))
			expect_true( all(tmp$GPP_DT_u50 >= 0))
			expect_true( all(tmp$GPP_DT_u50 < 250))
			expect_true( all(tmp$Reco_DT_u50 < 10))
			expect_true( all(tmp$Reco_DT_u50 > 0))
			expect_true( all(tmp$Reco_DT_u50_SD > 0))
			expect_true( all(tmp$GPP_DT_u50_SD >= 0))
			expect_true( all(abs(diff(tmp$Reco_DT_u50)) < 0.6))	#smooth
			# reporting good values at central records
			# tmp[resEx$iCentralRec,]
			#expect_true( sum( is.finite(tmp$FP_alpha) ) == nrow(resEx) ) 
			expect_true( all(abs(tmp$FP_alpha[resEx$iCentralRec] - resEx$alpha)[resEx$parms_out_range==0L & is.finite(tmp$FP_alpha[resEx$iCentralRec])] < 0.05) )
			#expect_true( all((is.na(tmp$FP_alpha[resLRCEx1$iFirstRec] - resLRCEx1$a)[resLRCEx1$parms_out_range!=0L])) )
			expect_true( length(is.finite(tmp$FP_RRef_Night)) > 0 )
			.tmp.plot <- function(){
				tmp$time <- dsNEE1$sDateTime
				plot( Reco_DT_u50 ~ time, tmp)
				#plot( diff(Reco_DT_u50) ~ time[-1], tmp)
				plot( GPP_DT_u50 ~ time, tmp)
				plot( FP_RRef_Night ~ time, tmp)
				plot( FP_RRef ~ FP_RRef_Night, tmp)
			}
		})		


test_that("partitionNEEGL sparse data",{
			dsNEE1 <- dsNEE
			#flag  all data except one day bad in order  to associate the same data with several windows
			dsNEE1$NEE_fqc[ (dsNEE$sDateTime >= "1998-06-03 00:00:00 GMT") & (dsNEE$sDateTime < "1998-06-05 00:00:00 GMT" | dsNEE$sDateTime >= "1998-06-06 00:00:00 GMT") ] <- 2L
			#plot( NEE_f ~ sDateTime, dsNEE1 )
			ds <- partGLExtractStandardData(dsNEE1)
			#
			dsTempSens <- partGLFitNightTimeTRespSens( ds
					, nRecInDay=48L
					, controlGLPart=partGLControl()
			)
			resLRC <- partGLFitLRCWindows(ds, dsTempSens=dsTempSens, lrcFitter=RectangularLRCFitter() )
			expect_true( resLRC$iMeanRec[2] == resLRC$iMeanRec[3])
			#
			tmp <- partitionNEEGL( dsNEE1)
			expect_equal( nrow(dsNEE1), nrow(tmp) )
			#
			expect_true( all(is.finite(tmp$GPP_DT)))
			expect_true( all(tmp$GPP_DT >= 0))
			expect_true( all(tmp$GPP_DT < 250))
			expect_true( all(tmp$Reco_DT < 10))
			expect_true( all(tmp$Reco_DT > 0))
			expect_true( all(tmp$Reco_DT_SD > 0))
			expect_true( all(tmp$GPP_DT_SD >= 0))
			#TODO expect_true( all(abs(diff(tmp$Reco_DT)) < 0.6))	#smooth
			# reporting good values at first row
			expect_true( sum( is.finite(tmp$FP_alpha) ) == sum(is.finite(resLRC$alpha)) ) 
			expect_true( all((is.na(tmp$FP_alpha[resLRC$iCentralRec] - resLRC$alpha)[resLRC$parms_out_range!=0L & is.finite(tmp$FP_alpha[resLRC$iCentralRec])])) )
			.tmp.plot <- function(){
				tmp$time <- dsNEE1$sDateTime
				plot( Reco_DT ~ time, tmp)
				#plot( diff(Reco_DT_u50) ~ time[-1], tmp)
				plot( GPP_DT ~ time, tmp)
			}
})

test_that("partitionNEEGL isNeglectVPDEffect",{
			dsNEE1 <- dsNEE
			#flag all VPD data except one day as bad to associate the same data with several windows
			dsNEE1$VPD_fqc[ (dsNEE$sDateTime >= "1998-06-03 00:00:00 GMT") & (dsNEE$sDateTime < "1998-06-05 00:00:00 GMT" | dsNEE$sDateTime >= "1998-06-06 00:00:00 GMT") ] <- 2L
			#plot(VPD_fqc ~ sDateTime, dsNEE1)
			ctrl <- partGLControl(isFilterMeteoQualityFlag=TRUE, isNeglectVPDEffect=TRUE)
			ds <- partGLExtractStandardData(dsNEE1, controlGLPart=ctrl)
			#plot(VPD ~ sDateTime, ds)
			#
			dsTempSens <- partGLFitNightTimeTRespSens( ds
					, nRecInDay=48L
					, controlGLPart=ctrl
			)
			resLRC <- partGLFitLRCWindows(ds, dsTempSens=dsTempSens, lrcFitter=RectangularLRCFitter() )
			expect_true( resLRC$iMeanRec[2] == resLRC$iMeanRec[3])
			#resLRC$nValidRec
			#
			resLRC2 <- partGLFitLRCWindows(ds, dsTempSens=dsTempSens, lrcFitter=RectangularLRCFitter()
				,controlGLPart=ctrl  )
			expect_true( resLRC2$iMeanRec[2] != resLRC2$iMeanRec[3])
			expect_true( all( resLRC2$nValidRec >=  50L) )	# used all records despite missing VPD
			#resLRC2
			#
			resLRC2C <- partGLFitLRCWindows(ds, dsTempSens=dsTempSens, lrcFitter=RectangularLRCFitterCVersion()
					,controlGLPart=ctrl  )
			expect_true( all( resLRC2C$nValidRec >=  50L) )	# used all records despite missing VPD
			iSd <- grep("_sd$|resOpt$",names(resLRC2)) # equal unless bootstrapped uncertainty and optimization result
			expect_equal( as.data.frame(resLRC2[,-iSd]), as.data.frame(resLRC2C[,-iSd]) )		
			#
			tmp <- partitionNEEGL( dsNEE1, controlGLPart=ctrl  )
			#tmp[ resLRC2$iCentralRec,]		
			expect_equal( tmp[ resLRC2$iCentralRec,"FP_alpha"], resLRC2$alpha )			
			expect_equal( nrow(dsNEE1), nrow(tmp) )
			#
			expect_true( all(is.finite(tmp$GPP_DT)))	# finite prediction despite missing VPD
			expect_true( all(tmp$GPP_DT >= 0))
			expect_true( all(tmp$GPP_DT < 250))
			expect_true( all(tmp$Reco_DT < 10))
			expect_true( all(tmp$Reco_DT > 0))
			expect_true( all(tmp$Reco_DT_SD > 0))
			expect_true( all(tmp$GPP_DT_SD >= 0))
		})

test_that("partGLPartitionFluxes missing night time data",{
			dsNEE1 <- dsNEE
			#set all data except one day to NA to associate the same data with several windows
			dsNEE1$hourOfDay <- as.POSIXlt(dsNEE1$sDateTime)$hour
			dsNEE1$NEE_f[ (dsNEE1$sDateTime >= "1998-06-01 00:00:00 GMT") & (dsNEE1$sDateTime < "1998-06-05 00:00:00 GMT") & 
							((dsNEE1$hourOfDay >= 18) | (dsNEE1$hourOfDay <= 6))] <- NA
			ds <- dsNEE1
			ds$NEE <- ds$NEE_f
			ds$sdNEE <- ds$NEE_fsd
			ds$Temp <- ds$Tair_f
			ds$VPD <- ds$VPD_f
			ds$Rg <- ds$Rg_f
			#
			dsTempSens <- partGLFitNightTimeTRespSens( ds
					, nRecInDay=48L
					, controlGLPart=partGLControl()
					, winSizeNight=4L, winExtendSizes=c()
			)
			expect_true( all( is.finite(dsTempSens$RRef)) )
			resLRC <- partGLFitLRCWindows(ds, lrcFitter=RectangularLRCFitter(), dsTempSens=dsTempSens )
			expect_true( all( is.finite(resLRC$RRef_night)) )
		})

	
test_that("partGLPartitionFluxes filter Meteo flag not enough without VPD",{
		dsNEE1 <- dsNEE
		# test setting VPD_fqc to other than zero, for omitting those rows
		dsNEE1$VPD_fqc[ (dsNEE1$sDateTime > "1998-06-03 00:00:00 GMT") & (dsNEE1$sDateTime < "1998-06-05 00:00:00 GMT" | dsNEE1$sDateTime >= "1998-06-06 00:00:00 GMT") ] <- 1L
		#plot( VPD_fqc ~ sDateTime, dsNEE1 )
		#plot( NEE_f ~ sDateTime, dsNEE1 )
		ds <- dsNEE1
		#ds$NEE <- ds$NEE_f
		#ds$sdNEE <- ds$NEE_fsd
		#ds$Temp <- ds$Tair_f
		#ds$VPD <- ds$VPD_f
		ds$Rg <- ds$Rg_f
		#
		.tmp.f <- function(){
			tmp0 <- partitionNEEGL( dsNEE1, controlGLPart=partGLControl(isFilterMeteoQualityFlag=TRUE) )
			tmpPar0 <- tmp0[is.finite(tmp0$FP_beta),]
		}
		tmp <- partitionNEEGL( ds, controlGLPart=partGLControl(isFilterMeteoQualityFlag=TRUE) )
		expect_equal( nrow(ds), nrow(tmp) )
		tmpPar <- tmp[is.finite(tmp$FP_RRef_Night),]
		expect_equal( sum(is.finite(tmpPar[,"FP_beta"])), 4L )	# estimate despite missing VPD
		expect_equal( tmpPar[4L,"FP_k"], 0 )	# k = 0 for missing VPD
		#
		# now set temperature to bad quality flag, 
		dsNEE1$Tair_fqc[ (dsNEE1$sDateTime > "1998-06-03 00:00:00 GMT") & (dsNEE1$sDateTime < "1998-06-05 00:00:00 GMT" | dsNEE1$sDateTime >= "1998-06-06 00:00:00 GMT") ] <- 1L
		ds <- dsNEE1
		ds$Rg <- ds$Rg_f
		tmp <- partitionNEEGL( ds, controlGLPart=partGLControl(isFilterMeteoQualityFlag=TRUE) )
		expect_equal( nrow(ds), nrow(tmp) )
		tmpPar <- tmp[is.finite(tmp$FP_RRef_Night),]
		expect_equal( sum(is.finite(tmpPar[,"FP_beta"])), 3L )	# one row less with parameter estimates
	})

test_that("partGLPartitionFluxes missing prediction VPD",{
			dsNEE1 <- dsNEE
			# test setting VPD_fqc to other than zero, for omitting those rows
			dsNEE1$VPD_fqc[ (dsNEE1$sDateTime > "1998-06-03 00:00:00 GMT") & (dsNEE1$sDateTime < "1998-06-05 00:00:00 GMT" | dsNEE1$sDateTime >= "1998-06-06 00:00:00 GMT") ] <- 1L
			#plot( VPD_fqc ~ sDateTime, dsNEE1 )
			#plot( NEE_f ~ sDateTime, dsNEE1 )
			iMissingVPD <- c(10,400)
			dsNEE1$VPD_f[iMissingVPD] <- NA
			ds <- dsNEE1
			#ds$NEE <- ds$NEE_f
			#ds$sdNEE <- ds$NEE_fsd
			#ds$Temp <- ds$Tair_f
			#ds$VPD <- ds$VPD_f
			ds$Rg <- ds$Rg_f
			#
			expect_warning(tmp <- partitionNEEGL( ds, controlGLPart=partGLControl(isFilterMeteoQualityFlag=TRUE, isRefitMissingVPDWithNeglectVPDEffect=FALSE) ))
			expect_equal( nrow(ds), nrow(tmp) )
			tmpPar <- tmp[is.finite(tmp$FP_RRef_Night),]
			expect_equal( sum(is.finite(tmpPar[,"FP_beta"])), 4L )	# estimate despite missing VPD
			expect_equal( tmpPar[4L,"FP_k"], 0 )	# k = 0 for missing VPD
			expect_true( is.na(tmp$GPP_DT[iMissingVPD[1]]) )	# default could not predict 
			expect_true( is.finite(tmp$GPP_DT[iMissingVPD[2]]) )	# second: both parameter sets with k=0 -> VPD not evaluated for prediction
			#
			# repeat with (default) refitting with neglecting VPD
			tmp2 <- partitionNEEGL( ds, controlGLPart=partGLControl(isFilterMeteoQualityFlag=TRUE, isRefitMissingVPDWithNeglectVPDEffect=TRUE) )
			expect_equal( tmp2[-iMissingVPD,"GPP_DT"], tmp[-iMissingVPD,"GPP_DT"] )
			expect_equal( tmp2[-iMissingVPD,"Reco_DT"], tmp[-iMissingVPD,"Reco_DT"] )
			expect_equal( tmp2[iMissingVPD[2],c("GPP_DT","Reco_DT")],  tmp2[iMissingVPD[2],c("GPP_DT","Reco_DT")])
			expect_true( is.finite(tmp2$GPP_DT[iMissingVPD[1]]) )	# now also estimate for first missing VPD 
		})



test_that("partitionNEEGL fixed tempSens",{
			dsNEE1 <- dsNEE
			#
			ds <- dsNEE1
			ds$NEE <- ds$NEE_f
			ds$sdNEE <- ds$NEE_fsd
			ds$Temp <- ds$Tair_f
			ds$VPD <- ds$VPD_f
			ds$Rg <- ds$Rg_f
			#
			dsTempSens0 <- partGLFitNightTimeTRespSens( ds )
			startRecs <- getStartRecsOfWindows( nrow(ds) )
			fixedTempSens <- data.frame(E0=80, sdE0=10 )
			tmp <- partitionNEEGL( dsNEE1, RadVar.s="Rg_f", controlGLPart=partGLControl(fixedTempSens=fixedTempSens) )
			expect_equal( nrow(dsNEE1), nrow(tmp) )
			#
			expect_true( all(is.finite(tmp$GPP_DT)))
			expect_true( all(tmp$GPP_DT >= 0))
			expect_true( all(tmp$GPP_DT < 250))
			expect_true( all(tmp$Reco_DT < 10))
			expect_true( all(tmp$Reco_DT > 0))
			expect_true( all(tmp$Reco_DT_SD > 0))
			expect_true( all(tmp$GPP_DT_SD >= 0))
			.tmp.plot <- function(){
				tmp$time <- dsNEE1$sDateTime
				plot( Reco_DT ~ time, tmp)
				#plot( diff(Reco_DT_u50) ~ time[-1], tmp)
				plot( GPP_DT ~ time, tmp)
			}
		})

test_that("partitionNEEGL: missing sdNEE",{
			dsNEE1 <- dsNEE
			#summary(dsNEE1)
			dsNEE1$NEE_fsd <- NA_real_
			#
			tmp <- partitionNEEGL( dsNEE1, RadVar.s="Rg_f", controlGLPart=partGLControl() )
			tmpWithPar <- tmp[ is.finite(tmp$FP_alpha),]
			expect_equal( nrow(tmpWithPar), 4)	# fitted with missing fsd
			#
			rm(tmp)
			expect_error(
					tmp <- partitionNEEGL( dsNEE1, RadVar.s="Rg_f", controlGLPart=partGLControl(replaceMissingSdNEEParms=c(NA,NA)) )
			)
		})

.profilePartGL <- function(){
	require(profr)
	p1 <- profr({
				for( i in 1:1 ){
					tmp <- partitionNEEGL( dsNEE1 )
				}
			}, 0.01 )
	plot(p1)
	
	
	
}

