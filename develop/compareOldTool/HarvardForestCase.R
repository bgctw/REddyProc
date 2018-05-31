#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Analysis for Harvard Forest

library(ncdf)

.tmp.f <- function(){
	# Mirco's code refers to ncdf, which is not available for R3.2 on Windows
	# workaround copy functions from ncdf4
	library(ncdf4)
	open.ncdf <- nc_open
	get.var.ncdf <- function(...){ as.vector(ncvar_get(...))}
}


### Pvwave from the FLUXNET2015 database
#ipath <- "M:/data/DataStructureMDI/DATA/Incoming/Fluxnet/berkeley_012016/Data/HH/"
ipath <- "M:/data/DataStructureMDI/DATA/Incoming/Fluxnet/berkeley_012016/Data/HH/2016_01/"
fname <- "US-Ha1.HH.1991.2012.nc"

nc <- open.ncdf(paste(ipath,fname,sep=""))


GPP_DT_VUT_50 <- get.var.ncdf(nc, "GPP_DT_VUT_50")
GPP_DT_CUT_50 <- get.var.ncdf(nc, "GPP_DT_CUT_50")
YEAR          <- get.var.ncdf(nc, "year")
TIMESTAMP_START <- get.var.ncdf(nc, "TIMESTAMP_START")
TIMESTAMP_END <- get.var.ncdf(nc, "TIMESTAMP_END")
VPD_F <- get.var.ncdf(nc, "VPD_F")
VPD_F_QC <- get.var.ncdf(nc, "VPD_F_QC")
TA_F <- get.var.ncdf(nc, "TA_F")
TA_F_QC <- get.var.ncdf(nc, "TA_F_QC")
NEE_VUT_REF_RANDUNC <- get.var.ncdf(nc, "NEE_VUT_REF_RANDUNC")
NIGHT <- get.var.ncdf(nc, "NIGHT")
SW_IN_F <- get.var.ncdf(nc, "SW_IN_F")
SW_IN_F_QC <- get.var.ncdf(nc, "SW_IN_F_QC")
SW_IN_F_MDS <- get.var.ncdf(nc, "SW_IN_F_MDS")
SW_IN_F_MDS_QC <- get.var.ncdf(nc, "SW_IN_F_MDS_QC")
NEE_VUT_USTAR50 <- get.var.ncdf(nc, "NEE_VUT_USTAR50")
NEE_VUT_USTAR50_QC <- get.var.ncdf(nc, "NEE_VUT_USTAR50_QC")
NEE_VUT_USTAR50_RANDUNC <- get.var.ncdf(nc, "NEE_VUT_USTAR50_RANDUNC")
NEE_CUT_REF <- get.var.ncdf(nc, "NEE_CUT_REF")
NEE_CUT_REF <- get.var.ncdf(nc, "NEE_CUT_REF")
NEE_CUT_REF_QC <- get.var.ncdf(nc, "NEE_CUT_REF_QC")
NEE_CUT_REF_RANDUNC <- get.var.ncdf(nc,"NEE_CUT_REF_RANDUNC")
SW_IN_POT <- get.var.ncdf(nc,"SW_IN_POT")
RECO_DT_VUT_50 <- get.var.ncdf(nc,"RECO_DT_VUT_50")
RECO_DT_CUT_50 <- get.var.ncdf(nc,"RECO_DT_CUT_50")

# calculate modelled NEE
NEE_DT_VUT_50 <- -(GPP_DT_VUT_50 - RECO_DT_VUT_50)

# Creating DAY Variable
DAY <- 1-NIGHT


# Creating the data frame with the proper values of julian, day, month, year etc from the TIMESTAMP_START variable in FLUXNET dataframe


timeStampChar <- as.character(TIMESTAMP_START)
Time.F <- data.frame(
  year = as.integer(substr(timeStampChar,1,4))
  ,month = as.integer(substr(timeStampChar,5,6))
  ,day = as.integer(substr(timeStampChar,7,8))
  ,hour = as.integer(substr(timeStampChar,9,10)) +
    as.integer(substr(timeStampChar,11,12))/60)

# Removing NA for sites with hourly data
Time.F <- Time.F[!is.na(as.integer(substr(timeStampChar,1,4))),]
# Data.F$julian<-julian(Data.F$month,Data.F$day,Data.F$year)-julian(1,1,na.omit(unique(Data.F$year))[1])


#Creating data frame with the variable of interest
# Variables needed for the partitioning are:
# - NEE - NEE time series (gap filled)
# - NEE_QC - quality flag indicating which values are measured (e.g. NEE_VUT_USTAR50_QC == 0)
# - SW_IN_F/VPD_F - Shortwave radiation, VPD,
# - SW_IN_POT SW potential radiation
# - DAY: 0 - night; 1 - day --> Trevor this is the variable that you have to define for your excercise (see below)

Data.F  <- data.frame(SW_IN_F=SW_IN_F,
                      SW_IN_F_QC=SW_IN_F_QC,
                      NEE=NEE_VUT_USTAR50,
                      NEE_QC=NEE_VUT_USTAR50_QC,
                      VPD_F=VPD_F,
                      VPD_F_QC=VPD_F_QC,
                      TA_F=TA_F,
                      TA_F_QC=TA_F_QC,
                      NEE_SE=NEE_VUT_USTAR50_RANDUNC,
                      SW_IN_POT=SW_IN_POT,
                      NIGHT=NIGHT,
                      DAY=DAY,
                      YEAR=YEAR,
                      GPP_DT_VUT_50=GPP_DT_VUT_50,
                      GPP_DT_CUT_50=GPP_DT_CUT_50,
                      RECO_DT_VUT_50=RECO_DT_VUT_50,
                      RECO_DT_CUT_50=RECO_DT_CUT_50,
                      NEE_DT_VUT_50=NEE_DT_VUT_50)  #This should be changed according to the needs

Data.F <- Data.F[!is.na(as.integer(substr(timeStampChar,1,4))),]   # same condition as for Time.F above

Data.F <- cbind(Time.F,Data.F)
# add timesStamp column DataTime
Data.F <- fConvertTimeToPosix(Data.F, 'YMDH', Year.s='year', Month.s='month',Day.s='day', Hour.s='hour')


#dfall_posix  <- fConvertTimeToPosix(Data.F, 'YMDH', Year.s = 'year', Month.s='month', Day.s = 'day', Hour.s = 'hour')

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# START - RUN THE REddyProc DT partitioning
# -- set different threshold for day and night

#xTrevor: I changed the line 65 in the script PartitioningLasslop10.R
# ORIGINAL isNight <- (ds[,RadVar.s] <= 4 & ds[[PotRadVar.s]] == 0) # 4 W/m2 is the threshold used in Lasslop
# MODIFIED isNight <- (ds[[PotRadVar.s]] == 0)
# Where PotRadVar.s can be a binary variable that you pass to the function (Data.F$DAY). YOu can define what is day and night in the script and useing different thresholds
#xTrevor: Please source PartitioningLasslop10_xTrevor.R at the beginning of the script

#-- Below I report the example to run the partitioning considering DAY when SW_IN_F > 4 W/m2, and at line 131 > 10W/m2

nRecInDay.i=24

# split dataframe into years (otherwise its too big and R crashes)
years <- unique(YEAR)
years <- years[-c(1,2,3)]   # model does not work for 1993 ("system is computationally singular")


#yr <- 2009
#yr <- 2001

ctrl <- partGLControl(nBootUncertainty=0L, isAssociateParmsToMeanOfValids=FALSE,
		isLasslopPriorsApplied=TRUE,isBoundLowerNEEUncertainty=FALSE,
		isFilterMeteoQualityFlag=FALSE
		,smoothTempSensEstimateAcrossTime=FALSE
)
#ctrl <- partGLControl()

.tmp.deprecatedCrashOnLongTimeSeries <- function(){
	for (yr in years){  # --> split into individual years or it will crash!
	  cat("starting year",yr,fill=T)
	  if (yr == years[1]){
		#dataYr <- Data.F
	    dataYr <- Data.F[Data.F[,"YEAR"] == yr,]
	    df.REddy_Ha1 <- partitionNEEGL(
	      dataYr,NEEVar.s="NEE",QFNEEVar.s="NEE_QC",QFNEEValue.n = 0,NEESdVar.s="NEE_SE",
	      TempVar.s="TA_F",QFTempVar.s="TA_F_QC",VPDVar.s="VPD_F",QFVPDVar.s="VPD_F_QC",
	      RadVar.s="SW_IN_F",PotRadVar.s="DAY",suffix="", nRecInDay= nRecInDay.i,
	      controlGLPart=ctrl)
	  } else {  # important: make sure it's identical to the first call!!
	    df.REddy_Ha1_year <- tmp <- partitionNEEGL(
	      dataYr,NEEVar.s="NEE",QFNEEVar.s="NEE_QC",QFNEEValue.n = 0,NEESdVar.s="NEE_SE",
	      TempVar.s="TA_F",QFTempVar.s="TA_F_QC",VPDVar.s="VPD_F",QFVPDVar.s="VPD_F_QC",
	      RadVar.s="SW_IN_F",PotRadVar.s="DAY",suffix="", nRecInDay= nRecInDay.i,
	      controlGLPart=ctrl)

	    df.REddy_Ha1 <- rbind(df.REddy_Ha1,df.REddy_Ha1_year)
	  }
	}
}



df.REddy_Ha1 <- partitionNEEGL(Data.F,NEEVar.s="NEE",QFNEEVar.s="NEE_QC",QFNEEValue.n = 0,NEESdVar.s="NEE_SE",
		TempVar.s="TA_F",QFTempVar.s="TA_F_QC",VPDVar.s="VPD_F",QFVPDVar.s="VPD_F_QC",
		RadVar.s="SW_IN_F",PotRadVar.s="DAY",suffix="", nRecInDay= nRecInDay.i,
		controlGLPart=ctrl)

.tmp.f <- function(){
	#ds <- Data.F[Data.F[,"YEAR"] == yr,]
	ds <- Data.F[Data.F[,"YEAR"] == yr,]
	plot( ds$NEE ~ ds$DateTime )
}

# df.REddy_Ha1 <- df.REddy_Ha1_year
df.hf <- df.REddy_Ha1
NEE_DT <- -(df.hf$GPP_DT - df.hf$Reco_DT)

GPP_good <- df.hf$GPP_DT
Reco_good <- df.hf$Reco_DT
GPP_good[df.hf$FP_qc > 0] <- NA
Reco_good[df.hf$FP_qc > 0 ] <- NA
Data.F2 <- Data.F #Data.F[Time.F[,"year"] %in% years,]

df.hf <- cbind(df.REddy_Ha1,Data.F2,NEE_DT,GPP_good,Reco_good)

.tmp.plotParamterTimeSeries <- function(){
 ## Parameter timeseries
 par(mfrow=c(2,3))
 plot(df.hf$FP_R_ref,xlab="Timestep",ylab="R_ref")
 plot(df.hf$FP_R_refNight,xlab="Timestep",ylab="R_refNight")
 plot(df.hf$FP_E0,xlab="Timestep",ylab="E0")
 plot(df.hf$FP_alpha,xlab="Timestep",ylab="alpha")
 plot(df.hf$FP_beta,xlab="Timestep",ylab="beta")
 plot(df.hf$FP_k,xlab="Timestep",ylab="k")

 ## GPP and Reco timeseries
 par(mfrow=c(1,2))
 plot(df.hf$GPP_DT,col="grey",xlab="Timestep",ylab="GPP")
 points(GPP_good,col="black")
 plot(df.hf$Reco_DT,col="grey",xlab="Timestep",ylab="REco")
 points(Reco_good,col="black")
}


plot(df.hf$FP_E0[is.finite(df.hf$FP_E0)],xlab="Timestep",ylab="E0", type="b")


### closer look at individual years
selyear <- c(1994:2012)
pr2 <- df.hf[df.hf$year %in% selyear,]


## GPP
par(mfrow=c(1,2))
plot(pr2$GPP_DT,col="grey",xlab="Timestep",ylab="GPP",ylim=c(0,40))
points(pr2$GPP_good,col="black")
points(pr2$GPP_DT_VUT_50,col="green")
#points(pr2$GPP_DT_CUT_50,col="red")
legend("topleft",legend=c("GPP_REddyProc","GPP_REddyProc_high_quality","GPP_FN15"),col=c("grey","black","green"),
       bty="n",pch=1,x.intersp=0.4,pt.lwd=2,y.intersp=0.5)

## Reco
plot(pr2$Reco_DT,col="grey",xlab="Timestep",ylab="Reco",ylim=c(0,13))
points(pr2$Reco_good,col="black")
points(pr2$RECO_DT_VUT_50,col="green")

dev.copy2pdf(file=paste0(path,"Plots/HarvardForest/HF_GPP_Reco_timeseries_",selyear,"_all.pdf"),
             width=11,height=7,pointsize=11)



par(mfrow=c(2,3))
plot(pr2$FP_R_ref,xlab="Timestep",ylab="R_ref")
plot(pr2$FP_R_refNight,xlab="Timestep",ylab="R_refNight")
plot(pr2$FP_E0,xlab="Timestep",ylab="E0")
plot(pr2$FP_alpha,xlab="Timestep",ylab="alpha")
plot(pr2$FP_beta,xlab="Timestep",ylab="beta")
plot(pr2$FP_k,xlab="Timestep",ylab="k")

dev.copy2pdf(file=paste0(path,"Plots/HarvardForest/HF_parameters_timeseries_",selyear,".pdf"),
             width=8,height=5,pointsize=11)




### compare modeled to measured NEE
par(mfrow=c(1,2))
plot(pr2$NEE ~ pr2$NEE_DT,xlab="NEE_DT",ylab="NEE_observed")
curve(1*x,from=-100,to=100,col="red",add=T)

plot(pr2$NEE ~ pr2$NEE_DT_CUT_50,xlab="NEE_DT",ylab="NEE_observed")
curve(1*x,from=-100,to=100,col="red",add=T)

dev.copy2pdf(file=paste0(path,"Plots/HarvardForest/HF_NEE_obs_mod_",selyear,"_all.pdf"),
             width=11,height=7,pointsize=11)

# dev.copy2pdf(file=paste0(path,"Plots/HarvardForest/HF_NEE_obs_mod_all.pdf"),
#              width=11,height=7,pointsize=11)




## meteo filter?
## replace 1 with 0
#
# #### To compare: the same command from above
# # df.REddy <- partitionNEEGL(dfall,NEEVar.s="NEE_f",QFNEEVar.s="NEE_fqc",QFNEEValue.n = 0,NEESdVar.s="NEE_fs_unc",
# #                            TempVar.s="Tair_f",QFTempVar.s="Tair_fqc",QFTempValue.n=0,VPDVar.s="VPD_f",QFVPDVar.s="VPD_fqc",
# #                            QFVPDValue.n=0,RadVar.s="Rg",PotRadVar.s="day",suffix=""
# #                            ,controlGLPart.l=partGLControl(nBootUncertainty=0L, isAssociateParmsToMeanOfValids=FALSE, isLasslopPriorsApplied=TRUE,
# #                                                           isBoundLowerNEEUncertainty=FALSE))
#
#

