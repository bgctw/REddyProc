#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for fConvertTimeToPosix functions +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: TW
#require(testthat)
context("partGL")

if (!exists(".partGPAssociateSpecialRows")) .partGPAssociateSpecialRows <-
    REddyProc:::.partGPAssociateSpecialRows
if (!exists(".binUstar")) .binUstar <- REddyProc:::.binUstar


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# 10 days from June from Example_DETha98.txt shipped with REddyProc
Example_DETha98_Filled <- getFilledExampleDETha98Data()

tzEx <- REddyProc:::getTZone(Example_DETha98_Filled$sDateTime)
test_that("example dataset starts at midngiht",{
  expect_true( Example_DETha98_Filled$sDateTime[1] ==
                 as.POSIXct("1998-01-01 00:15:00",tz = tzEx) )
})

dsNEE <- subset(Example_DETha98_Filled
                , sDateTime >= as.POSIXct("1998-06-01 00:15:00",tz = tzEx) &
                  sDateTime <= as.POSIXct("1998-06-09 21:45:00",tz = tzEx))

dsNEE$Temp <- dsNEE$Tair_f
dsNEE$isNight <- (dsNEE$Rg_f <= 4 & dsNEE$PotRad_NEW == 0)
dsNEE$isDay = (dsNEE$Rg_f > 4 & dsNEE$PotRad_NEW != 0)

# 8 first days of June from IT-MBo.2005.txt
# 10 days from June from Example_DETha98.txt shipped with REddyProc
.tmp.f <- function(){
  #save(dsNEE, file = "tmp/dsNEE_Tharandt.RData")
  load("tmp/dsNEE_Tharandt.RData") # dsNEE
  dsNEE$Temp <- dsNEE$Tair_f
  dsNEE$Rg_f <- dsNEE$Rg
  dsNEE$isNight <- (dsNEE$Rg_f <= 4 & dsNEE$PotRad_NEW == 0)
  dsNEE$isDay = (dsNEE$Rg_f > 4 & dsNEE$PotRad_NEW != 0)
}

.tmp.f2 <- function(){
  # else stop in partitionNEEGL, and grap ds:
  attr(ds$sDateTime, "tzone") <- "UTC"
  dsJune <- ds
  dsNEE <- dsJune[ dsJune$sDateTime >= as.POSIXct("1998-06-01", tz = "UTC") &
                     dsJune$sDateTime < as.POSIXct("1998-06-10", tz = "UTC")
                   , c("sDateTime","NEE_f","NEE_fqc","NEE_fsd","Tair_f","Tair_fqc","VPD_f"
                       ,"VPD_fqc","Rg_f","PotRad_NEW")]
  save(dsNEE, file = "tmp/dsNEE_Tharandt.RData")
  dput(dsNEE)
}


# regression result from dput in
resLRCEx1 <- structure(list(
  iWindow = 1:4, dayStart = c(1L, 3L, 5L, 7L), dayEnd = c(4L, 6L, 8L, 9L)
  , iRecStart = c(1L, 97L, 193L, 289L), iRecEnd = c(192L, 288L, 384L, 428L)
  , iCentralRec = c(97L, 193L, 289L, 385L), nValidRec = c(98L,98L, 75L, 56L)
  , iMeanRec = c(97L, 194L, 283L, 385L)
  , convergence = c(0L,0L, 0L, 0L), parms_out_range = c(0L, 0L, 0L, 0L)
  , k = c(
    0.15556543911127, 0.100336071150363, 0.0292272454449355, 0.110909241852741)
  , beta = c(
    34.2363908612952, 28.7053014210465, 11.965007963525, 35.6061046130658)
  , alpha = c(
    0.0408287988682114, 0.0317164138158062, 0.12184364495971, 0.0565989757582912)
  , RRef = c(
    1.30588274435898, 2.30342378833466, 4.39970416578445, 3.02347421699605)
  , E0 = structure(
    c(62.633343296311, 62.633343296311, 62.633343296311, 62.633343296311)
    , .Dim = c(4L, 1L), .Dimnames = list(NULL, NULL))
  , k_sd = c(
    0.0162347703672834, 0.00819228863397153, 0.0036484758449877, 0.0143126637303779)
  , beta_sd = c(
    2.6633120785356, 2.53043346467805, 0.480085981083196, 3.86498333963868)
  , alpha_sd = c(
    0.00282359981111383, 0.00245807510249875, 0.0189581631026536, 0.00624526974304941)
  , RRef_sd = c(
    0.243651749533536, 0.200219643557999, 0.379555581976541, 0.408032180867498)
  , E0_sd = structure(
    c(4.25106879296635, 4.22511397480583, 4.22511362599932, 4.46593783063789)
    , .Dim = c(4L, 1L), .Dimnames = list(NULL, NULL))
  , GPP2000 = c(
    24.1225750077742, 19.7622684102263, 11.4050231061514, 27.0862112802389)
  , isValid = c(TRUE, TRUE, TRUE, TRUE)
  , resOpt = list(structure(list(thetaOpt = structure(
    c(0.15556543911127, 34.2363908612952, 0.0408287988682114
      , 1.30588274435898, 62.633343296311)
    , .Names = c("k", "beta", "alpha", "RRef", "E0"))
    , iOpt = 1:5, thetaInitialGuess = structure(c(
      0.05, 24.3542, 0.1, 5.17329116845493, 62.633343296311)
      , .Names = c("k", "beta", "alpha", "RRef", "E0"))
    , covParms = structure(c(
      0.000263567768878423, 0.0299004485274175, -2.01722085100335e-05
      , -0.00170017321010028, 0, 0.0299004485274174, 7.09323122767363
      , -0.00442973195793744, -0.184157427431281, 0, -2.01722085100335e-05
      , -0.00442973195793744, 7.97271589332207e-06, 0.000574595628371077, 0
      , -0.00170017321010028, -0.18415742743128, 0.000574595628371077
      , 0.0593661750507529, 0, 0, 0, 0, 0, 18.0715858825324)
      , .Dim = c(5L, 5L), .Dimnames = list(
        structure(c("k", "beta", "alpha", "RRef", "E0")
                  , .Names = c("k", "beta", "alpha", "RRef", "E0"))
        , structure(c("k", "beta", "alpha", "RRef", "E0")
                    , .Names = c("k", "beta", "alpha", "RRef", "E0"))))
    , convergence = 0L)
    , .Names = c(
      "thetaOpt", "iOpt", "thetaInitialGuess", "covParms", "convergence"))
    , structure(list(thetaOpt = structure(
      c(0.100336071150363, 28.7053014210465, 0.0317164138158062
        , 2.30342378833466, 62.633343296311)
      , .Names = c("k", "beta", "alpha", "RRef", "E0"))
      , iOpt = 1:5
      , thetaInitialGuess = structure(
        c(0.05, 24.8403, 0.1, 5.14770049230269, 62.633343296311)
        , .Names = c("k", "beta", "alpha", "RRef", "E0"))
      , covParms = structure(
        c(6.71135930622992e-05, 0.0178801034106948, -1.74280275277554e-05
          , -0.00134394852514135, 0, 0.0178801034106948, 6.40309351916256
          , -0.00493560933704443, -0.271156219332448, 0, -1.74280275277554e-05
          , -0.00493560933704443, 6.04213320952425e-06, 0.000418391213333105
          , 0, -0.00134394852514135, -0.271156219332449, 0.000418391213333105
          , 0.040087905666492, 0, 0, 0, 0, 0, 17.8515881000995)
        , .Dim = c(5L, 5L), .Dimnames = list(
          structure(c("k", "beta", "alpha", "RRef", "E0")
                    , .Names = c("k", "beta", "alpha", "RRef", "E0"))
          , structure(c("k", "beta", "alpha", "RRef", "E0")
                      , .Names = c("k", "beta", "alpha", "RRef", "E0"))))
      , convergence = 0L)
      , .Names = c(
        "thetaOpt", "iOpt", "thetaInitialGuess", "covParms", "convergence"))
    , structure(list(thetaOpt = structure(
      c(0.0292272454449355, 11.965007963525, 0.12184364495971
        , 4.39970416578445, 62.633343296311)
      , .Names = c("k", "beta", "alpha", "RRef", "E0"))
      , iOpt = 1:5
      , thetaInitialGuess = structure(
        c(0.05, 24.1194, 0.1, 5.14770049230269, 62.633343296311)
        , .Names = c("k", "beta", "alpha", "RRef", "E0"))
      , covParms = structure(
        c(1.33113759914587e-05, 0.000439566354933103, -5.94748188903169e-05
          , -0.00111894361237909, 0, 0.000439566354933103, 0.230482549232615
          , -0.000116871653668728, 0.053260722726946, 0, -5.94748188903169e-05
          , -0.000116871653668727, 0.000359411948226816, 0.00657062463616557
          , 0, -0.00111894361237909, 0.0532607227269459, 0.00657062463616558
          , 0.144062439809551, 0, 0, 0, 0, 0, 17.8515851526051)
        , .Dim = c(5L, 5L), .Dimnames = list(
          structure(c("k", "beta", "alpha", "RRef", "E0")
                    , .Names = c("k", "beta", "alpha", "RRef", "E0"))
          , structure(c("k", "beta", "alpha", "RRef", "E0")
                      , .Names = c("k", "beta", "alpha", "RRef", "E0"))))
      , convergence = 0L)
      , .Names = c(
        "thetaOpt", "iOpt", "thetaInitialGuess", "covParms", "convergence"))
    , structure(list(thetaOpt = structure(
      c(0.110909241852741, 35.6061046130658, 0.0565989757582912
        , 3.02347421699605, 62.633343296311)
      , .Names = c("k", "beta", "alpha", "RRef", "E0"))
      , iOpt = 1:5
      , thetaInitialGuess = structure(
        c(0.05, 21.665, 0.1, 5.21501814931109, 62.633343296311)
        , .Names = c("k", "beta", "alpha", "RRef", "E0"))
      , covParms = structure(c(
        0.000204852343058874, 0.0495123146513288, -6.84478020597351e-05
        , -0.00386015942484132, 0, 0.0495123146513288, 14.9380962156846
        , -0.0178713249608098, -0.786540464781837, 0, -6.8447802059735e-05
        , -0.0178713249608097, 3.90033941634485e-05, 0.00226964418347153
        , 0, -0.00386015942484131, -0.786540464781834, 0.00226964418347153
        , 0.166490260623486, 0, 0, 0, 0, 0, 19.9446007071226)
        , .Dim = c(5L, 5L), .Dimnames = list(structure(
          c("k", "beta", "alpha", "RRef", "E0")
          , .Names = c("k", "beta", "alpha", "RRef", "E0"))
          , structure(
            c("k", "beta", "alpha", "RRef", "E0")
            , .Names = c("k", "beta", "alpha", "RRef", "E0"))))
      , convergence = 0L)
      , .Names = c(
        "thetaOpt", "iOpt", "thetaInitialGuess", "covParms", "convergence")))
  , E0_bootstrap_sd = c(
    4.25106879296635, 4.22511397480583, 4.22511362599932, 4.46593783063789)
  , RRef_night = c(
    5.17329116845493, 5.14770049230269, 5.14770049230269, 5.21501814931109))
  , .Names = c(
    "iWindow", "dayStart", "dayEnd", "iRecStart", "iRecEnd", "iCentralRec"
    , "nValidRec", "iMeanRec", "convergence", "parms_out_range", "k", "beta"
    , "alpha", "RRef", "E0", "k_sd", "beta_sd", "alpha_sd", "RRef_sd", "E0_sd"
    , "GPP2000", "isValid", "resOpt", "E0_bootstrap_sd", "RRef_night")
  , row.names = c(NA, -4L)
  , class = c("tbl_df", "tbl", "data.frame")
)


resLRCEx1Nonrectangular <- structure(list(
  iWindow = 1:4, dayStart = c(1L, 3L, 5L, 7L), dayEnd = c(4L, 6L, 8L, 9L)
  , iRecStart = c(1L, 97L, 193L, 289L), iRecEnd = c(192L, 288L, 384L, 428L)
  , iCentralRec = c(97L, 193L, 289L, 385L), nValidRec = c(98L, 98L, 75L, 56L)
  , iMeanRec = c(97L, 194L, 283L, 385L), convergence = c(1005L, 1005L, 1005L, 0L)
  , E0 = structure(c(
    62.633343296311, 62.633343296311, 62.633343296311, 62.633343296311)
    , .Dim = c(4L, 1L), .Dimnames = list( NULL, NULL))
  , E0_sd = structure(
    c(4.25106879296635, 4.22511397480583, 4.22511362599932, 4.46593783063789)
    , .Dim = c(4L, 1L), .Dimnames = list( NULL, NULL))
  , RRefNight = c(5.17329116845493, 5.14770049230269, 5.14770049230269, NA)
  , parms_out_range = c(NA, NA, NA, 0L), k = c(NA, NA, NA, 0.15301818456226)
  , beta = c(NA, NA, NA, 49.3407957035085 )
  , alpha = c(NA, NA, NA, 0.0444237102808321)
  , RRef = c(NA, NA, NA, 2.35368171175251)
  , logitconv = c(NA, NA, NA, -3.10163338239329 )
  , k_sd = c(NA, NA, NA, 0.0197088986436212)
  , beta_sd = c(NA, NA, NA, 7.58853578454936)
  , alpha_sd = c(NA, NA, NA, 0.00432079403235484 )
  , RRef_sd = c(NA, NA, NA, 0.350880271587216)
  , logitconv_sd = c(NA, NA, NA, 1.92633463362266)
  , GPP2000 = c(NA, NA, NA, 32.0432128516724 )
  , isValid = c(NA, NA, NA, TRUE)
  , resOpt = list(NULL, NULL, NULL, structure(
    list(thetaOpt = structure(c(
      0.15301818456226, 49.3407957035085, 0.0444237102808321, 2.35368171175251
      , 62.633343296311, -3.10163338239329 )
      , .Names = c("k", "beta", "alpha", "RRef", "E0", "logitconv" ))
      , iOpt = c(1, 2, 3, 4, 6, 5)
      , thetaInitialGuess = structure(c(
        0.05, 21.665, 0.1, 5.21501814931109, 62.633343296311, 1.09861228866811 )
        , .Names = c("k", "beta", "alpha", "RRef", "E0", "logitconv" ))
      , covParms = structure(c(
        0.000388440685744535, 0.13746635747882, -6.51906051810543e-05
        , -0.00449519721692684, 0, 0.00207560732637497, 0.13746635747882
        , 57.5858753533862, -0.0238832979670161, -1.36443384766771, 0
        , -1.52788892519458, -6.51906051810543e-05, -0.023883297967016
        , 1.86692610700332e-05, 0.00131289852541519, 0, -0.00181474371141437
        , -0.00449519721692684, -1.36443384766771, 0.00131289852541519
        , 0.123116964989119, 0, -0.0864363703508266, 0, 0, 0, 0, 19.9446007071226
        , 0, 0.00207560732637499, -1.52788892519458, -0.00181474371141437
        , -0.0864363703508268, 0, 3.71076512069413 )
        , .Dim = c(6L, 6L), .Dimnames = list(structure(c(
          "k", "beta", "alpha", "RRef", "E0", "logitconv")
          , .Names = c("k", "beta", "alpha", "RRef", "E0", "logitconf"))
          , structure(c(
            "k", "beta", "alpha", "RRef", "E0", "logitconv")
            , .Names = c("k", "beta", "alpha", "RRef", "E0", "logitconf"))))
      , convergence = 0L)
    , .Names = c(
      "thetaOpt", "iOpt", "thetaInitialGuess", "covParms", "convergence")))
  , E0_bootstrap_sd = c(
    4.25106879296635, 4.22511397480583, 4.22511362599932, 4.46593783063789)
  , RRef_night = c(
    5.17329116845493, 5.14770049230269, 5.14770049230269, 5.21501814931109))
  , .Names = c(
    "iWindow", "dayStart", "dayEnd", "iRecStart", "iRecEnd", "iCentralRec"
    , "nValidRec", "iMeanRec", "convergence", "E0", "E0_sd", "RRefNight"
    , "parms_out_range", "k", "beta", "alpha", "RRef", "logitconv", "k_sd"
    , "beta_sd", "alpha_sd", "RRef_sd", "logitconv_sd", "GPP2000", "isValid"
    , "resOpt", "E0_bootstrap_sd", "RRef_night")
  , row.names = c(NA, -4L)
  , class = c("tbl_df", "tbl", "data.frame")
)

test_that("replaceMissingSdByPercentage",{
  n <- 100
  x <- rnorm(n,10)
  sdX <- sdX0 <- pmax(0.7, rnorm(n, 10*0.1 ))
  iMissing <- sample(n, 20)
  sdX[iMissing] <- NA
  sdXf <- REddyProc:::replaceMissingSdByPercentage(sdX,x, 0.2, 1.7)
  #plot( sdXf ~ x, col = "blue" );points( sdX ~ x )
  expect_equal( sdXf[-iMissing], sdX[-iMissing])
  expect_true( all(is.finite(sdXf)))
  expect_true( all(sdXf[iMissing] >= 1.7))
  expect_true( all(sdXf[iMissing] >= 0.2*x[iMissing]))
  #
  sdX <- sdX0
  iMissing <- TRUE
  sdX[iMissing] <- NA
  sdXf <- REddyProc:::replaceMissingSdByPercentage(sdX,x, 0.2, 1.7)
  #plot( sdXf ~ x, col = "blue" );points( sdX ~ x )
  expect_true( all(is.finite(sdXf)))
  expect_true( all(sdXf[iMissing] >= 1.7))
  expect_true( all(sdXf[iMissing] >= 0.2*x[iMissing]))
  #
  sdX <- sdX0
  iMissing <- sample(n, 20)
  sdX[iMissing] <- NA
  sdXf <- REddyProc:::replaceMissingSdByPercentage(sdX,x, NA, 1.7)
  expect_true( all(sdXf[iMissing] == 1.7))
  #plot( sdXf ~ x, col = "blue" );points( sdX ~ x )
  sdXf <- REddyProc:::replaceMissingSdByPercentage(sdX,x, 0.2, NA)
  expect_true( all(sdXf[iMissing] == 0.2*x[iMissing]))
  sdXf <- REddyProc:::replaceMissingSdByPercentage(sdX,x, NA, NA)
  expect_true( all(is.na(sdXf[iMissing])))
})

test_that("estimating temperature sensitivity oneWindow are in accepted range",{
  dss <- dsNEE[ dsNEE$Rg_f <= 0 & dsNEE$PotRad_NEW <= 0
                & as.POSIXlt(dsNEE$sDateTime)$mday %in% 1:8, ]
  dss <- dss[ order(dss$Temp), ]
  dss$NEE <- dss$NEE_f
  resE0 <- REddyProc:::partGLEstimateTempSensInBoundsE0Only(
    dss$NEE_f, dss$Temp + 273.15)
  expect_true( resE0$E0 >= 50 && resE0$E0 < 400 )
  medianResp <- median(dss$NEE_f,na.rm = TRUE)
  expect_true( abs(resE0$RRefFit - medianResp)/medianResp < 0.2 )
  E0Win <- as.data.frame(resE0)
  res <- REddyProc:::partGLFitNightRespRefOneWindow(
    dss, data.frame(iWindow = 1L), E0Win = E0Win)
  RRef <- res[1]
  expect_true( RRef >= 0)
  .tmp.plot <- function(){
    plot( NEE_f ~ Temp, dss)		# FP_VARnight negative?
    lines( fLloydTaylor(RRef, resE0$E0, dss$Temp+273.15, TRef = 273.15+15) ~
             dss$Temp)
  }
})

test_that("estimating temperature sensitivity on record with some freezing temperatures",{
  dss <- dsNEE[ dsNEE$Rg_f <= 0 & dsNEE$PotRad_NEW <= 0 &
                  as.POSIXlt(dsNEE$sDateTime)$mday %in% 1:8, ]
  dss$NEE <- dss$NEE_f
  dss <- dss[ order(dss$Temp), ]
  # only the last 13 records will have temperature above -1degC
  dss$Temp <- dss$Temp - dss$Temp[ nrow(dss) - 14L ] - 1
  isValid <- REddyProc:::isValidNightRecord(dss)
  expect_true(sum(isValid) >= 13 &&
                sum(REddyProc:::isValidNightRecord(dss)) < nrow(dss) )
  #
  resE0 <- REddyProc:::partGLEstimateTempSensInBoundsE0Only(
    dss$NEE_f[isValid], dss$Temp[isValid] + 273.15)
  expect_true( is.na(resE0$E0) )
  .tmp.f <- function(){
    expect_true( resE0$E0 >= 50 && resE0$E0 <= 400 )
    medianResp <- median(dss$NEE_f[isValid],na.rm = TRUE)
    expect_true( abs(resE0$RRefFit - medianResp)/medianResp < 0.2 )
    E0Win <- as.data.frame(resE0)
    res <- REddyProc:::partGLFitNightRespRefOneWindow(
      dss, data.frame(iWindow = 1L), E0Win = E0Win)
    RRef <- res[[2]]$RRef[1]
    expect_true( RRef >= 0 )
  }
  .tmp.plot <- function(){
    plot( NEE_f ~ Temp, dss)		# FP_VARnight negative?
    points( NEE_f ~ Temp, dss[isValid,], col = "red")
    lines( fLloydTaylor(RRef, resE0$E0, dss$Temp+273.15, TRef = 273.15+15) ~
             dss$Temp)#
  }
})

test_that("applyWindows",{
  nRec <- nrow(dsNEE)
  nRecInDay <- 10L
  ds <- within(dsNEE, {
    iRec <- 1:nRec
    # specifying the day for each record assuming equidistand records
    iDayOfRec <- ((c(1:nRec) - 1L) %/% nRecInDay) + 1L
  })
  fReportTime <- function(dss, winInfo, prevRes){
    nRecS <- nrow(dss)
    list( res2 = data.frame(
      startRec = dss$iRec[1]
      ,endRec = dss$iRec[nRecS]
      ,startDay = dss$iDayOfRec[1]
      ,endDay = dss$iDayOfRec[nRecS]
    )
    ,sumRes = prevRes$sumRes + 1L
    )
  }
  prevRes <- list(sumRes = 0)
  fReportTime(ds,0,prevRes)
  # larger than reference window of 4 days
  resApply <- REddyProc:::applyWindows(
    ds, fReportTime, prevRes, winSizeInDays = 6L, nRecInDay = nRecInDay )
  res <- cbind( resApply$winInfo, tmp <- do.call(
    rbind, lapply(resApply$resFUN, "[[", 1L)))
  nRecRes <- nrow(res)
  expect_equal( res$dayStart, res$startDay )
  expect_equal( res$dayEnd, res$endDay )
  expect_equal( res$iRecStart, res$startRec )
  expect_equal( res$iRecEnd, res$endRec )
  #
  expect_true( all(diff(res$startDay[-1]) == 2L))	# shifted starting day
  # day boundary before startRec
  expect_true( all((ds$iDayOfRec[res$startRec[-1]] -
                      ds$iDayOfRec[res$startRec[-1] - 1]) == 1L))
  # day boundary after endRec
  expect_true( all((ds$iDayOfRec[res$startRec[-nRecRes]] + 1 -
                      ds$iDayOfRec[res$startRec[-nRecRes]]) == 1L))
  # prevRes accumulated
  expect_equal( resApply$resFUN[[nRecRes]]$sumRes, nrow(res) )
})

test_that("simplifyApplyWindows",{
  nRec <- nrow(dsNEE)
  nRecInDay <- 10L
  ds <- within(dsNEE, {
    iRec <- 1:nRec
    # specifying the day for each record assuming equidistand records
    iDayOfRec <- ((c(1:nRec) - 1L) %/% nRecInDay) + 1L
  })
  fReportTimeSimple <- function(dss, winInfo, prevRes = list()){
    nRecS <- nrow(dss)
    c(
      startRec = dss$iRec[1]
      ,endRec = dss$iRec[nRecS]
      ,startDay = dss$iDayOfRec[1]
      ,endDay = dss$iDayOfRec[nRecS]
    )
  }
  fReportTimeSimple(ds,0)
  # larger than reference window of 4 days
  resApply <- REddyProc:::applyWindows(
    ds, fReportTimeSimple, winSizeInDays = 6L, nRecInDay = nRecInDay )
  res <- REddyProc:::simplifyApplyWindows(resApply)
  nRecRes <- nrow(res)
  expect_equal( res$dayStart, res$startDay )
  expect_equal( res$dayEnd, res$endDay )
  expect_equal( res$iRecStart, res$startRec )
  expect_equal( res$iRecEnd, res$endRec )
  #
  # repeat with function returning a single-row data.frame
  fReportTimeSimpleDs <- function(dss, winInfo, prevRes = list()){
    nRecS <- nrow(dss)
    data.frame(
      startRec = dss$iRec[1]
      ,endRec = dss$iRec[nRecS]
      ,startDay = dss$iDayOfRec[1]
      ,endDay = dss$iDayOfRec[nRecS]
    )
  }
  fReportTimeSimpleDs(ds,0)
  # larger than reference window of 4 days
  resApply <- REddyProc:::applyWindows(
    ds, fReportTimeSimpleDs, winSizeInDays = 6L, nRecInDay = nRecInDay )
  resDs <- REddyProc:::simplifyApplyWindows(resApply)
  expect_equal(res, resDs )
})

test_that("estimating temperature sensitivity windows outputs are in accepted range",{
  dss <- dsNEE[ dsNEE$Rg_f <= 0 & dsNEE$PotRad_NEW <= 0 &
                  as.POSIXlt(dsNEE$sDateTime)$mday %in% 1:12, ]
  dss$NEE <- dss$NEE_f
  dss <- dss[ order(dss$Temp), ]
  res <- REddyProc:::simplifyApplyWindows(
    REddyProc:::applyWindows(
      dss, REddyProc:::partGLFitNightTempSensOneWindow
      , prevRes = data.frame(E0 = NA)
      , winSizeInDays = 12L
      #,controlGLPart = controlGLPart
    ))
  #res <- partGLEstimateTempSensInBounds(dss$NEE_f, dss$Temp+273.15)
  expect_true( res$E0 >= 50 && res$E0 <= 400 )
  expect_true( res$RRefFit > 0 )
})


test_that("partGLFitLRCWindows outputs are in accepted range",{
  ds <- partGLExtractStandardData(dsNEE)
  #
  #yday <- as.POSIXlt(dsNEE$sDateTime)$yday
  dsTempSens <- dsTempSens0 <- REddyProc:::partGLFitNightTimeTRespSens(
    ds
    , nRecInDay = 48L
    , controlGLPart = partGLControl()
  )
  lrcFitter <- RectangularLRCFitter()
  #lrcFitter <- NonrectangularLRCFitter()
  resFits <- REddyProc:::partGLFitLRCWindows(
    ds, nRecInDay = 48L, dsTempSens = dsTempSens
    , controlGLPart = partGLControl(nBootUncertainty = 10L)
    , lrcFitter = lrcFitter)
  expect_true( var(resFits$k) > 0)
  expect_true( all( sapply(resFits$resOpt, length) != 0 ))
  .tmp.f <- function(){
    # in order to replicate, use nBoot = 0L
    resParms0 <- REddyProc:::partGLFitLRCWindows(
      ds, nRecInDay = 48L, dsTempSens = dsTempSens
      , controlGLPart = partGLControl(nBootUncertainty = 0L)
      , lrcFitter = lrcFitter)
    iTooMany <- which(resParms0$iRecStart >= nrow(ds) - 2*48)
    if (length(iTooMany)) {
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
  # first value may be greater, due to previous estimate
  expect_true( all(resFits$alpha[-1] < 0.22) )
  expect_true( all(resFits$beta >= 0 & resFits$beta < 250) )
  expect_true( all(ifelse(resFits$beta > 100, resFits$beta_sd < resFits$beta, TRUE) ))
  expect_true( all(resFits$k >= 0) )
  expect_true( !all(is.na(resFits$RRef_sd)))
  expect_true( all(resFits$iMeanRec < nrow(ds)) )
  expect_true( all(resFits$iCentralRec < nrow(ds)) )
})

test_that("partGLFitLRCWindows error on low uncertainty",{
  # see LightResponseCurveFitter_optimLRCOnAdjustedPrior
  ds <- partGLExtractStandardData(dsNEE)
  ds$sdNEE[100:200] <- 0
  dsTempSens <- dsTempSens0 <- REddyProc:::partGLFitNightTimeTRespSens(
    ds
    , nRecInDay = 48L
    , controlGLPart = partGLControl()
  )
  lrcFitter <- RectangularLRCFitter()
  #lrcFitter <- NonrectangularLRCFitter()
  if (exists("resFits")) rm(resFits)
  expect_error(
    resFits <- REddyProc:::partGLFitLRCWindows(
      ds, nRecInDay = 48L, dsTempSens = dsTempSens
      , controlGLPart = partGLControl(nBootUncertainty = 10L)
      , lrcFitter = lrcFitter)
    ,"zeros in uncertainty"
  )
  expect_true(!exists("resFits"))
})


test_that(".partGPAssociateSpecialRows correct next lines",{
  expect_error(
    res <- REddyProc:::.partGPAssociateSpecialRows(integer(0),9)
  )
  res <- REddyProc:::.partGPAssociateSpecialRows(c(3,6,7,9),12)
  expect_true( all(res$wBefore + res$wAfter == 1))
  # special rows
  expect_equal( c(iRec = 3,iSpecialBefore = 1L,iSpecialAfter = 1L
                  , iBefore = 3, iAfter = 3, wBefore = 0.5, wAfter = 0.5)
                , unlist(res[3,]))
  expect_equal( c(iRec = 6,iSpecialBefore = 2L,iSpecialAfter = 2L
                  , iBefore = 6, iAfter = 6, wBefore = 0.5, wAfter = 0.5)
                , unlist(res[6,]))
  # first rows and last rows
  expect_equal( c(iRec = 1,iSpecialBefore = 1L,iSpecialAfter = 1L
                  , iBefore = 3, iAfter = 3, wBefore = 0.5, wAfter = 0.5)
                , unlist(res[1,]))
  expect_equal( c(iRec = 12,iSpecialBefore = 4L,iSpecialAfter = 4L
                  , iBefore = 9, iAfter = 9, wBefore = 0.5, wAfter = 0.5)
                , unlist(res[12,]))
  # weights after and before special row 6
  expect_equal( rep(3,2), unlist(res[4:5,"iBefore"]))
  expect_equal( rep(6,2), unlist(res[4:5,"iAfter"]))
  expect_equal( rep(1,2), unlist(res[4:5,"iSpecialBefore"]))
  expect_equal( rep(2,2), unlist(res[4:5,"iSpecialAfter"]))
  expect_equal( (2:1)/3, unlist(res[4:5,"wBefore"]))
  expect_equal( (1:2)/3, unlist(res[4:5,"wAfter"]))
  # test last row is special
  res <- REddyProc:::.partGPAssociateSpecialRows(c(3,6,7,9),9)
})

testGLInterpolateFluxes <- function(lrcFitter, resEx ){
  tmp <- REddyProc:::partGLInterpolateFluxes(
    dsNEE$Rg_f, dsNEE$VPD_f, dsNEE$Temp, resEx, lrcFitter = lrcFitter)
  expect_equal( nrow(dsNEE), nrow(tmp) )
  .tmp.plot <- function(){
    tmp$time <- dsNEE$sDateTime
    plot( Reco ~ time, tmp)
    plot( GPP ~ time, tmp)
  }
  expect_true( all(c("GPP","Reco") %in% names(tmp) ))
  tmp <- REddyProc:::partGLInterpolateFluxes(
    dsNEE$Rg_f, dsNEE$VPD_f, dsNEE$Temp, resEx
    , controlGLPart = partGLControl(isSdPredComputed = TRUE)
    , lrcFitter = lrcFitter)
  expect_equal( nrow(dsNEE), nrow(tmp) )
  expect_true( all(c("GPP","Reco") %in% names(tmp) ))
  expect_true( all(c("sdGPP","sdReco") %in% names(tmp) ))
}
test_that("partGLInterpolateFluxes runs with rectangular LRCFitter",{
  lrcFitter = RectangularLRCFitter(); resEx <- resLRCEx1
  testGLInterpolateFluxes( lrcFitter, resEx)
})
test_that("partGLInterpolateFluxes runs with rectangular LRCFitter",{
  lrcFitter = NonrectangularLRCFitter(); resEx <- resLRCEx1Nonrectangular
  testGLInterpolateFluxes( lrcFitter, resEx)
})


#resLRCEx1
test_that("partitionNEEGL",{
  skip_on_cran()
  dsNEE1 <- dsNEE
  resEx <- resLRCEx1
  #DoY.V.n <- as.POSIXlt(dsNEE1$sDateTime)$yday + 1L
  #Hour.V.n <- as.POSIXlt(dsNEE1$sDateTime)$hour + as.POSIXlt(dsNEE1$sDateTime)$min/60
  #dsNEE1$PotRad_NEW <- fCalcPotRadiation(DoY.V.n, Hour.V.n, LatDeg = 45.0, LongDeg = 1, TimeZoneHour = 0 )
  tmp <- partitionNEEGL( dsNEE1,RadVar = 'Rg_f')
  #tmp <- partitionNEEGL( dsNEE1,RadVar = 'Rg_f', isVerbose = FALSE) #no messages
  #tmp <- partitionNEEGL( dsNEE1,RadVar = 'Rg_f', controlGLPart = partGLControl(nBootUncertainty = 0L, isAssociateParmsToMeanOfValids = FALSE))
  expect_equal( nrow(dsNEE1), nrow(tmp) )
  #tmp[ is.finite(tmp$FP_beta), ]	# note FP_dRecPar is not zero, because iCentralRec != iMeanRec
  #iPar <- which(is.finite(tmp$FP_E0))
  #plot( tmp$FP_beta[iPar] ~ dsNEE1$sDateTime[iPar],type = "l", ylim = range(c(tmp$FP_beta[iPar],tmp$FP_GPP2000[iPar]))); lines( tmp$FP_GPP2000[iPar] ~ dsNEE1$sDateTime[iPar], col = "red")
  #
  # now test with different suffix: u50
  dsNEE2 <- dsNEE
  names(dsNEE2)[ match(c("NEE_f", "NEE_fqc", "NEE_fsd"),names(dsNEE2))] <-
    c("NEE_u50_f", "NEE_u50_fqc", "NEE_u50_fsd")
  tmp <- partitionNEEGL(
    dsNEE2, RadVar = 'Rg_f', suffix = "u50"
    , controlGLPart = partGLControl(
      nBootUncertainty = 0L, isAssociateParmsToMeanOfValids = FALSE) )
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
  #expect_true( all((tmp$FP_alpha[resEx$iCentralRec] - resEx$alpha)[
  #resEx$parms_out_range == 0L] < 1e-2) )
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
  skip_on_cran()
  dsNEE1 <- dsNEE
  #ds <- data.frame( NEE = dsNEE1$NEE_f, sdNEE = dsNEE1$NEE_fsd, Rg = dsNEE1$Rg_f, VPD = dsNEE1$VPD_f, Temp = dsNEE1$Temp, isDay = dsNEE1$isDay, isNight = dsNEE$isNight )
  resEx <- resLRCEx1
  dsNEE2 <- dsNEE
  partGLControl()
  names(dsNEE2)[ match(c("NEE_f", "NEE_fqc", "NEE_fsd"),names(dsNEE2))] <-
    c("NEE_u50_f", "NEE_u50_fqc", "NEE_u50_fsd")
  dsNEE2$NEE_u50_fsd[24] <- NA
  tmp <- partitionNEEGL( dsNEE2, RadVar = 'Rg_f', suffix = "u50"
                         , controlGLPart = partGLControlLasslopCompatible() )
  tmp$sDateTime <- dsNEE2$sDateTime
  tmp$iRec <- 1:nrow(tmp)
  #subset(tmp, is.finite(FP_errorcode))
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
  # options yield large differences, hence do not compare to resEx1
  #expect_true( all(abs(tmp$FP_alpha[resEx$iCentralRec] - resEx$alpha)[
  #resEx$parms_out_range == 0L & is.finite(tmp$FP_alpha[resEx$iCentralRec])] < 0.05) )
  #expect_true( all((is.na(tmp$FP_alpha[resLRCEx1$iFirstRec] - resLRCEx1$a)[
  #resLRCEx1$parms_out_range != 0L])) )
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

isTimeInTestPeriod <- function(sDateTime){
  (sDateTime >= as.POSIXct("1998-06-03 00:00:00",tz = tzEx)) &
    (sDateTime < as.POSIXct("1998-06-05 00:00:00",tz = tzEx) |
       sDateTime >= as.POSIXct("1998-06-05 22:00:00",tz = tzEx))
}

.tmp.f <- function(){
  isTimeInTestPeriodNoTemp <- function(sDateTime){
    # strange: 4 more valid NEE yiels in temperature estimate failing TODO inspect
    (sDateTime >= as.POSIXct("1998-06-03 00:00:00",tz = tzEx)) &
      (sDateTime < as.POSIXct("1998-06-05 00:00:00",tz = tzEx) |
         sDateTime >= as.POSIXct("1998-06-06 00:00:00",tz = tzEx))
  }
  dsNEE2 <- dsNEE
  dsNEE2$NEE_fqc[ isTimeInTestPeriodNoTemp(dsNEE$sDateTime)  ] <- 2L
  #table(dsNEE1$NEE_fqc)
  #plot( NEE_f ~ sDateTime, dsNEE1 )
  ds2 <- partGLExtractStandardData(dsNEE2)
  dsTempSens2 <- REddyProc:::partGLFitNightTimeTRespSens( ds
                                                          , nRecInDay = 48L
                                                          , controlGLPart = partGLControl()
  )
}

test_that("partitionNEEGL sparse data",{
  skip_on_cran()
  dsNEE1 <- dsNEE
  #flag  all data except one day bad in order  to associate the same
  #data with several windows
  dsNEE1$NEE_fqc[ isTimeInTestPeriod(dsNEE$sDateTime)  ] <- 2L
  #table(dsNEE1$NEE_fqc)
  #plot( NEE_f ~ sDateTime, dsNEE1 )
  ds <- partGLExtractStandardData(dsNEE1)
  #
  dsTempSens <- REddyProc:::partGLFitNightTimeTRespSens(
    ds
    , nRecInDay = 48L
    , controlGLPart = partGLControl()
  )
  resLRC <- REddyProc:::partGLFitLRCWindows(
    ds, dsTempSens = dsTempSens, lrcFitter = RectangularLRCFitter() )
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
  expect_true( all((is.na(tmp$FP_alpha[resLRC$iCentralRec] - resLRC$alpha)[
    resLRC$parms_out_range != 0L & is.finite(tmp$FP_alpha[resLRC$iCentralRec])])) )
  .tmp.plot <- function(){
    tmp$time <- dsNEE1$sDateTime
    plot( Reco_DT ~ time, tmp)
    #plot( diff(Reco_DT_u50) ~ time[-1], tmp)
    plot( GPP_DT ~ time, tmp)
  }
})

test_that("partitionNEEGL isNeglectVPDEffect",{
  skip_on_cran()
  dsNEE1 <- dsNEE
  #flag all VPD data except one day as bad to associate the same data with several windows
  dsNEE1$VPD_fqc[ (dsNEE$sDateTime >= as.POSIXct("1998-06-03 00:00:00",tz = tzEx)) &
                    (dsNEE$sDateTime < as.POSIXct("1998-06-05 00:00:00",tz = tzEx) |
                       dsNEE$sDateTime >= as.POSIXct("1998-06-06 00:00:00",tz = tzEx)) ] <- 2L
  #plot(VPD_fqc ~ sDateTime, dsNEE1)
  ctrl <- partGLControl(isFilterMeteoQualityFlag = TRUE, isNeglectVPDEffect = TRUE)
  ds <- partGLExtractStandardData(dsNEE1, controlGLPart = ctrl)
  #plot(VPD ~ sDateTime, ds)
  #
  dsTempSens <- REddyProc:::partGLFitNightTimeTRespSens( ds
                                                         , nRecInDay = 48L
                                                         , controlGLPart = ctrl
  )
  resLRC <- REddyProc:::partGLFitLRCWindows(
    ds, dsTempSens = dsTempSens, lrcFitter = RectangularLRCFitter() )
  expect_true( resLRC$iMeanRec[2] == resLRC$iMeanRec[3])
  #resLRC$nValidRec
  #
  resLRC2 <- REddyProc:::partGLFitLRCWindows(
    ds, dsTempSens = dsTempSens, lrcFitter = RectangularLRCFitter()
    ,controlGLPart = ctrl  )
  expect_true( resLRC2$iMeanRec[2] != resLRC2$iMeanRec[3])
  # used all records despite missing VPD
  expect_true( all( resLRC2$nValidRec >=  50L) )
  #resLRC2
  #
  resLRC2C <- REddyProc:::partGLFitLRCWindows(
    ds, dsTempSens = dsTempSens, lrcFitter = RectangularLRCFitterCVersion()
    ,controlGLPart = ctrl  )
  # used all records despite missing VPD
  expect_true( all( resLRC2C$nValidRec >=  50L) )
  # equal unless bootstrapped uncertainty and optimization result
  iSd <- grep("_sd$|resOpt$",names(resLRC2))
  expect_equal( as.data.frame(resLRC2[,-iSd]), as.data.frame(resLRC2C[,-iSd]) )
  #
  tmp <- partitionNEEGL( dsNEE1, controlGLPart = ctrl  )
  #tmp[ resLRC2$iCentralRec,]
  expect_equal( tmp[ resLRC2$iCentralRec,"FP_alpha"], resLRC2$alpha )
  expect_equal( nrow(dsNEE1), nrow(tmp) )
  #
  # finite prediction despite missing VPD
  expect_true( all(is.finite(tmp$GPP_DT)))
  expect_true( all(tmp$GPP_DT >= 0))
  expect_true( all(tmp$GPP_DT < 250))
  expect_true( all(tmp$Reco_DT < 10))
  expect_true( all(tmp$Reco_DT > 0))
  expect_true( all(tmp$Reco_DT_SD > 0))
  expect_true( all(tmp$GPP_DT_SD >= 0))
})

test_that("partGLPartitionFluxes missing night time data",{
  dsNEE1 <- dsNEE
  #set all data except one day to NA to associate the same data
  #with several windows
  dsNEE1$hourOfDay <- as.POSIXlt(dsNEE1$sDateTime)$hour
  dsNEE1$NEE_f[ (dsNEE1$sDateTime >= as.POSIXct("1998-06-01 00:00:00",tz = tzEx)) &
                  (dsNEE1$sDateTime < as.POSIXct("1998-06-05 00:00:00",tz = tzEx)) &
                  ((dsNEE1$hourOfDay >= 18) | (dsNEE1$hourOfDay <= 6))] <- NA
  ds <- dsNEE1
  ds$NEE <- ds$NEE_f
  ds$sdNEE <- ds$NEE_fsd
  ds$Temp <- ds$Tair_f
  ds$VPD <- ds$VPD_f
  ds$Rg <- ds$Rg_f
  #
  dsTempSens <- REddyProc:::partGLFitNightTimeTRespSens( ds
                                                         , nRecInDay = 48L
                                                         , controlGLPart = partGLControl()
                                                         , winSizeNight = 4L, winExtendSizes = c()
  )
  expect_true( all( is.finite(dsTempSens$RRef)) )
  resLRC <- REddyProc:::partGLFitLRCWindows(
    ds, lrcFitter = RectangularLRCFitter(), dsTempSens = dsTempSens )
  expect_true( all( is.finite(resLRC$RRef_night)) )
})


test_that("partGLPartitionFluxes filter Meteo flag not enough without VPD",{
  dsNEE1 <- dsNEE
  # test setting VPD_fqc to other than zero, for omitting those rows
  dsNEE1$VPD_fqc[ isTimeInTestPeriod(dsNEE1$sDateTime) ] <- 1L
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
    tmp0 <- partitionNEEGL(
      dsNEE1, controlGLPart = partGLControl(isFilterMeteoQualityFlag = TRUE) )
    tmpPar0 <- tmp0[is.finite(tmp0$FP_beta),]
  }
  tmp <- partitionNEEGL(
    ds, controlGLPart = partGLControl(isFilterMeteoQualityFlag = TRUE) )
  expect_equal( nrow(ds), nrow(tmp) )
  tmpPar <- tmp[is.finite(tmp$FP_RRef_Night),]
  # estimate despite missing VPD
  expect_equal( sum(is.finite(tmpPar[,"FP_beta"])), 4L )
  expect_equal( tmpPar[4L,"FP_k"], 0 )	# k = 0 for missing VPD
  #
  # now set temperature to bad quality flag,
  dsNEE1$Tair_fqc[ isTimeInTestPeriod(dsNEE1$sDateTime) ] <- 1L
  #dsNEE1$VPD_fqc[ (dsNEE1$sDateTime > as.POSIXct("1998-06-03 00:00:00",tz = tzEx)) &
  #(dsNEE1$sDateTime < as.POSIXct("1998-06-05 00:00:00",tz = tzEx) |
  #dsNEE1$sDateTime >= as.POSIXct("1998-06-06 00:00:00",tz = tzEx)) ] <- 1L
  ds <- dsNEE1
  ds$Rg <- ds$Rg_f
  tmp <- partitionNEEGL(
    ds, controlGLPart = partGLControl(isFilterMeteoQualityFlag = TRUE) )
  expect_equal( nrow(ds), nrow(tmp) )
  tmpPar <- tmp[is.finite(tmp$FP_RRef_Night),]
  # one row less with parameter estimates
  expect_equal( sum(is.finite(tmpPar[,"FP_beta"])), 3L )
})

test_that("partGLPartitionFluxes missing prediction VPD",{
  dsNEE1 <- dsNEE
  # test setting VPD_fqc to other than zero, for omitting those rows
  dsNEE1$VPD_fqc[ (dsNEE1$sDateTime > as.POSIXct("1998-06-03 00:00:00",tz = tzEx)) &
                    (dsNEE1$sDateTime < as.POSIXct("1998-06-05 00:00:00",tz = tzEx) |
                       dsNEE1$sDateTime >= as.POSIXct("1998-06-06 00:00:00",tz = tzEx)) ] <- 1L
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
  expect_warning(tmp <- partitionNEEGL(
    ds, controlGLPart = partGLControl(
      isFilterMeteoQualityFlag = TRUE
      , isRefitMissingVPDWithNeglectVPDEffect = FALSE) ))
  expect_equal( nrow(ds), nrow(tmp) )
  tmpPar <- tmp[is.finite(tmp$FP_RRef_Night),]
  # estimate despite missing VPD
  expect_equal( sum(is.finite(tmpPar[,"FP_beta"])), 4L )
  expect_equal( tmpPar[4L,"FP_k"], 0 )	# k = 0 for missing VPD
  expect_true( is.na(tmp$GPP_DT[iMissingVPD[1]]) )	# default could not predict
  # second: both parameter sets with k = 0 -> VPD not evaluated for prediction
  expect_true( is.finite(tmp$GPP_DT[iMissingVPD[2]]) )
  #
  # repeat with (default) refitting with neglecting VPD
  tmp2 <- partitionNEEGL(
    ds, controlGLPart = partGLControl(
      isFilterMeteoQualityFlag = TRUE
      , isRefitMissingVPDWithNeglectVPDEffect = TRUE) )
  expect_equal( tmp2[-iMissingVPD,"GPP_DT"], tmp[-iMissingVPD,"GPP_DT"] )
  expect_equal( tmp2[-iMissingVPD,"Reco_DT"], tmp[-iMissingVPD,"Reco_DT"] )
  expect_equal( tmp2[iMissingVPD[2],c("GPP_DT","Reco_DT")],
                tmp2[iMissingVPD[2],c("GPP_DT","Reco_DT")])
  # now also estimate for first missing VPD
  expect_true( is.finite(tmp2$GPP_DT[iMissingVPD[1]]) )
})



test_that("partitionNEEGL fixed tempSens",{
  skip_on_cran()
  dsNEE1 <- dsNEE
  #
  ds <- dsNEE1
  ds$NEE <- ds$NEE_f
  ds$sdNEE <- ds$NEE_fsd
  ds$Temp <- ds$Tair_f
  ds$VPD <- ds$VPD_f
  ds$Rg <- ds$Rg_f
  #
  dsTempSens0 <- REddyProc:::partGLFitNightTimeTRespSens( ds )
  startRecs <- REddyProc:::getStartRecsOfWindows( nrow(ds) )
  fixedTempSens <- data.frame(E0 = 80, sdE0 = 10 )
  tmp <- partitionNEEGL(
    dsNEE1, RadVar = "Rg_f", controlGLPart = partGLControl(
      fixedTempSens = fixedTempSens) )
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
  tmp <- partitionNEEGL(
    dsNEE1, RadVar = "Rg_f", controlGLPart = partGLControl() )
  tmpWithPar <- tmp[ is.finite(tmp$FP_alpha),]
  expect_equal( nrow(tmpWithPar), 4)	# fitted with missing fsd
  #
  rm(tmp)
  expect_error(
    tmp <- partitionNEEGL(
      dsNEE1, RadVar = "Rg_f"
      , controlGLPart = partGLControl(replaceMissingSdNEEParms = c(NA,NA)) )
  )
})

# see .profilePartGL in develop/profile/profile.R

test_that("partitionNEEGL long gap",{
  skip("cannot repoduce SmoothTempSens failing")
  dsNEE1 <- Example_DETha98_Filled
  # introduce a long four month gap
  bo <- dsNEE1$DoY >= 80 & dsNEE1$DoY <= 360
  dsNEE1$NEE_f[bo] <- NA
  dsNEE1$NEE_fqc[bo] <- 3
  tmp <- partitionNEEGL( dsNEE1)
  tmp2 <- cbind(select(dsNEE1, sDateTime), tmp)
  plot( FP_qc ~ sDateTime, tmp2)
  plot( GPP_DT ~ sDateTime, tmp2)
})

test_that("partitionNEETK",{
  skip_on_cran()
  dsNEE1 <- dsNEE
  resEx <- resLRCEx1
  #DoY.V.n <- as.POSIXlt(dsNEE1$sDateTime)$yday + 1L
  #Hour.V.n <- as.POSIXlt(dsNEE1$sDateTime)$hour + as.POSIXlt(dsNEE1$sDateTime)$min/60
  #dsNEE1$PotRad_NEW <- fCalcPotRadiation(DoY.V.n, Hour.V.n, LatDeg = 45.0, LongDeg = 1, TimeZoneHour = 0 )
  tmp <- partitionNEEGL(
    dsNEE1,RadVar = 'Rg_f'
    , controlGLPart = partGLControl(useNightimeBasalRespiration = TRUE))
  #tmp <- partitionNEEGL( dsNEE1,RadVar = 'Rg_f', controlGLPart = partGLControl(nBootUncertainty = 0L, isAssociateParmsToMeanOfValids = FALSE))
  expect_equal( nrow(dsNEE1), nrow(tmp) )
  #tmp[ is.finite(tmp$FP_beta), ]	# note FP_dRecPar is not zero, because iCentralRec != iMeanRec
  #iPar <- which(is.finite(tmp$FP_E0))
  #plot( tmp$FP_beta[iPar] ~ dsNEE1$sDateTime[iPar],type = "l", ylim = range(c(tmp$FP_beta[iPar],tmp$FP_GPP2000[iPar]))); lines( tmp$FP_GPP2000[iPar] ~ dsNEE1$sDateTime[iPar], col = "red")
  #
  # now test with different suffix: u50
  dsNEE2 <- dsNEE
  names(dsNEE2)[ match(c("NEE_f", "NEE_fqc", "NEE_fsd"),names(dsNEE2))] <-
    c("NEE_u50_f", "NEE_u50_fqc", "NEE_u50_fsd")
  tmp <- partitionNEEGL(
    dsNEE2, RadVar = 'Rg_f', suffix = "u50"
    , controlGLPart = partGLControl(
      nBootUncertainty = 0L, isAssociateParmsToMeanOfValids = FALSE
      ,useNightimeBasalRespiration = TRUE )
    )
  expect_equal( nrow(dsNEE1), nrow(tmp) )
  expect_true( all(is.finite(tmp$GPP_DT_u50)))
  expect_true( all(tmp$GPP_DT_u50 >= 0))
  expect_true( all(tmp$GPP_DT_u50 < 250))
  expect_true( all(tmp$Reco_DT_u50 < 10))
  expect_true( all(tmp$Reco_DT_u50 > 0))
  expect_true( all(tmp$Reco_DT_u50_SD >= 0))
  expect_true( all(tmp$GPP_DT_u50_SD >= 0))
  # jumps between daytime and nighttime
  #expect_true( all(abs(diff(tmp$Reco_DT_u50)) < 0.6))	#smooth
  # reporting good values at central records
  # tmp[resEx$iCentralRec,]
  iRowsOpt <- which(is.finite(tmp$FP_E0))
  expect_true( length(iRowsOpt) == nrow(resEx) )
  #expect_true( all((tmp$FP_alpha[resEx$iCentralRec] - resEx$alpha)[
  #resEx$parms_out_range == 0L] < 1e-2) )
  .tmp.plot <- function(){
    tmp$time <- dsNEE1$sDateTime
    plot( Reco_DT_u50 ~ time, tmp)
    #plot( diff(Reco_DT_u50) ~ time[-1], tmp)
    plot( GPP_DT_u50 ~ time, tmp)
    plot( FP_RRef_Night ~ time, tmp)
    plot( FP_RRef ~ FP_RRef_Night, tmp)
    plot( Reco_DT_u50_SD ~ time, tmp)
  }
})

test_that("report missing VPD_f column in error",{
  skip("only interactively test issue #34")
  DETha98 <- fConvertTimeToPosix(Example_DETha98, 'YDH', Year = 'Year',
                                 Day = 'DoY', Hour = 'Hour')[-(2:4)]
  EProc <- sEddyProc$new('DE-Tha', DETha98,
                         c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
  EProc$sMDSGapFillAfterUstar('NEE', uStarTh = 0.3, FillAll = TRUE)
  # missing 'VPD'...
  for (i in c('Tair', 'Rg')) EProc$sMDSGapFill(i, FillAll = TRUE)
  EProc$sSetLocationInfo(LatDeg = 51.0, LongDeg = 13.6, TimeZoneHour = 1)
  # ...produces Error here
  # Error should report missing column VPD_f
  expect_error(
    EProc$sGLFluxPartition(suffix = "uStar")
    ,"VPD_f"
  )
})

test_that("no nighttime data",{
  skip_on_cran()
  cols <- c('NEE','Rg','Tair','VPD', 'Ustar'
            ,"Tair_f","VPD_f","Rg_f","NEE_f"
            ,"NEE_fqc", "Tair_fqc", "Rg_fqc", "NEE_fsd")
  #ds <- Example_DETha98_Filled[,c('DateTime',cols)]
  ds <- subset(Example_DETha98_Filled
                  , sDateTime >= as.POSIXct("1998-05-01 00:15:00",tz = tzEx) &
                    sDateTime <= as.POSIXct("1998-09-01 21:45:00",tz = tzEx))
  ds <- ds[,c('DateTime',cols)]
  ds$NEE_f[ds$Rg_f <= 10] <- NA  # simulate having no night-time records
  EProc <- suppressWarnings(sEddyProc$new('DE-ThaSummer', ds, cols))
  EProc$sSetLocationInfo(LatDeg = 51.0, LongDeg = 13.6, TimeZoneHour = 1)
  EProc$sGLFluxPartition(controlGLPart = partGLControl(fixedTempSens = data.frame(
    E0 = 150, sdE0 = 50, RRef = 20)))
})

.test_sEddyProc_sGLFluxPartitionUStarScens <- function(){}
test_that("sEddyProc_sGLFluxPartitionUStarScens wrong suffix",{
  dsTest <- Example_DETha98_Filled %>% mutate(
    NEE_uStar_f = NEE, NEE_uStar_fqc = NEE_fqc, NEE_uStar_fsd = NEE_fsd,
    NEE_U50_f = NEE, NEE_U50_fqc = NEE_fqc, NEE_U50_fsd = NEE_fsd
    ) %>%
    select(-.data$sDateTime)
  EProc <- sEddyProc$new(
    'DE-Tha', dsTest, setdiff(names(dsTest), c("NEE_fnum","NEE_fwin")))
  EProc$sSetUStarSeasons(1L)
  EProc$sSetUstarScenarios(data.frame(season = 1L, uStar = 0.28, U50 = 0.38))
  EProc$sSetLocationInfo(LatDeg = 51.0, LongDeg = 13.6, TimeZoneHour = 1)
  expect_error(
    EProc$sGLFluxPartitionUStarScens(uStarScenKeep = "nonexisting")
    ,"nonexisting")
})

test_that("sEddyProc_sGLFluxPartitionUStarScens",{
  dsTest <- Example_DETha98_Filled %>% mutate(
    NEE_uStar_f = NEE, NEE_uStar_fqc = NEE_fqc, NEE_uStar_fsd = NEE_fsd,
    NEE_U50_f = NEE, NEE_U50_fqc = NEE_fqc, NEE_U50_fsd = NEE_fsd
  ) %>%
    select(-.data$sDateTime)
  EProc <- sEddyProc$new(
    'DE-Tha', dsTest, setdiff(names(dsTest), c("NEE_fnum","NEE_fwin")))
  EProc$sSetUStarSeasons(1L)
  EProc$sSetUstarScenarios(data.frame(season = 1L, uStar = 0.28, U50 = 0.38))
  EProc$sSetLocationInfo(LatDeg = 51.0, LongDeg = 13.6, TimeZoneHour = 1)
  #EProc$sMDSGapFill('Tair', FillAll = FALSE,  minNWarnRunLength = NA)
  #EProc$sMDSGapFill('VPD', FillAll = FALSE,  minNWarnRunLength = NA)
  #EProc$trace(sGLFluxPartitionUStarScens, recover); # EProc$untrace(sGLFluxPartitionUStarScens)
  #EProc$sGLFluxPartitionUStarScens(uStarScenKeep = "U50")
  EProc$sGLFluxPartitionUStarScens()
  expect_true(all(c("GPP_DT_uStar","GPP_DT_U50", "GPP_DT_U50_SD") %in%
                    names(EProc$sTEMP)))
})


