# Script for running REddyProc with different options on 29 site-years 
#   same as was used in Wutzler17 for benchmarking daytime data

# install_github(repo="bgctw/REddyProc",branch="master")

if( !exists("partitionNEEGL") ) library(REddyProc)	# only load library if not sourced its already
.tmp.f <- function(){
	library(Rcpp)
	library(plyr)
	source("R/GeoFunctions.R")
	source("R/FileHandling.R")
	source("R/DataFunctions.R")
	source("R/PartitioningLasslop10.R")
	pkg <- 'REddyProc'
	loadDll <- function (dynFilenameLocal = file.path( system.file(package=pkg), "libs", "x64"
							, paste0(pkg, .Platform$dynlib.ext)), pkg = pkg, isWarningDllNotFound = TRUE
	){
		if (file.exists(dynFilenameLocal)) {
			dyn.load(dynFilenameLocal)
		}
		else if (isWarningDllNotFound) {
			warning(paste("dynLib", dynFilenameLocal, "not found."))
		}
	}
	loadDll(pkg=pkg)
	source("R/RcppExports.R")
}

library(tibble)

library(foreach)
#library(doMC)		#twutz: doMC is not supported for Windows any more, may swith to same functionality with doParallel	
library(doParallel)

CLUSTER<- if( .Platform$OS.type=="windows") FALSE else TRUE
#CLUSTER<-FALSE

#registerDoMC(8) 
#doMC::registerDoMC(cores=48) # or however many cores you have access to
if( CLUSTER ){
	registerDoParallel(cores=32L)
} else {
	registerDoParallel(cores=2L)
}

## aggregation sd
aggregate.sd <- function(x){  # where x is the sd of the variable
  n      <- sum(!is.na(x)) 
  agg_sd <- sqrt(sum(x^2,na.rm=T)) / n
  return(agg_sd)
}


##############
## settings ##
##############
scen <- "default"
#scen <- "omitSaturationFilter"
#scen <- "SaturationPenalty50"
#scen <- "SaturationPenalty5"

scenarioConfigurations = tribble(
		~scenario, ~ouputPath,  ~ctrl, ~comment,
		#"Lasslop10", "Lasslop10",  partGLControlLasslopCompatible(), "most Lasslop compatible"			
		"default", "default1709",  partGLControl(), "",
		"omitSaturationPenalty","default_options1709/omitSaturationPenalty", partGLControl(minPropSaturation=NA, weightMisfitPar2000=NA), "omit filtering windows that did not go to saturation: minPropSaturation=NA", 
		"SaturationPenalty50","default_options1709/saturationPenalty50", partGLControl(minPropSaturation=NA, weightMisfitPar2000=50), "omit filtering saturation but penalize misfit (weight=50) in saturation at PAR=2000" 
		,"SaturationPenalty5","default_options1709/saturationPenalty5", partGLControl(minPropSaturation=NA, weightMisfitPar2000=5), "omit filtering saturation but moderately penalize misfit (weight=5) in saturation at PAR=2000" 
	)
scenConf <- as.list(subset(scenarioConfigurations, scenario==scen))


####################
## site selection ##
####################
##here is my site selection based on Mirco's mail:
sitesMircoUsed <- c("BR-Ma2","BR-Sa1","CA-Let","CA-NS7","CA-TP3","CH-Oe2","CN-HaM","DE-Hai","DK-Sor","ES-E1","ES-VDA","FI-Hyy","FI-Kaa","FR-Fon","FR-Gri","FR-Hes","FR-Lq1","FR-Lq2","FR-Pue","IE-Dri","IL-Yat","IT-Amp","IT-MBo","IT-Pia","IT-SRo","JP-Tef","PT-Esp","RU-Cok","SE-Nor","TH98_new","US-Ha1","US-MMS","US-SO2","US-Ton","VU-Coc")
sitesRestricted <- c("BR-Ma2","BR-Sa1","CA-Let","CN-HaM","JP-Tef","US-SO2","IT-Pia")

siteMapping = list(TH98_new = "DE-Tha", "ES-E1"="ES-ES1")

sitesUsed <- setdiff( c( unlist(siteMapping), setdiff(sitesMircoUsed,sitesRestricted)), names(siteMapping))
sites <- sort(sitesUsed)

bgiDir <- if( .Platform$OS.type=="windows") "M:" else "/Net/Groups/BGI"

path  <- file.path(bgiDir,"work_3/REddyProcRelease/Eval_GL_Partitioning")
flist <- list.files(file.path(path,"MR_GL_partitioning"), pattern="*DataSetafterFluxpart.txt") #[-31]
flist <- flist[is.element(substr(flist,1,6),sites)]

## exclude sites that were not processed in the online tool
sites <- sites[is.element(sites,substr(flist,1,6))]

latLongSites <- rbind( 
		data.frame(site="DE-Tha", lat=51, long=11, timeOffset=-1	)
		,data.frame(site="IT-MBo", lat=45.0, long=1, timeOffset=0	)
)
tmp <- read.csv(file.path(path,"CommonAnc.csv"), colClasses=c(Site.ID="character", Latitude="numeric", Longitude="numeric", UTC="numeric"), na.strings="TBD")
latLongSites <- data.frame(site=tmp$Site.ID, lat=tmp$Latitude, long=tmp$Longitude, timeOffset=floor(-tmp$UTC))

# create output directory if not existing
if( !dir.exists(file.path(path,"Results",scenConf$ouputPath)) ) dir.create( file.path(path,"Results",scenConf$ouputPath) )
	

###########################
#---- Reading data

# NEE_orig already ustar filtered!! as in Papale_2006 most conservative threshold, 
# the seasons are computed in a different manner (not e.g. JFM) 
# gapfilling according to Reichstein_2005



#s <- grep("CA-TP3",sites)[1]
s <- grep("DE-Tha",sites)[1]
s <- grep("FR-Gri",sites)[1]
s <- grep("ES-VDA",sites)[1]
s <- grep("IL-Yat",sites)[1]

siteName 	   <- sites[s] 
fname        <- flist[s]

iProcSites <- seq_along(sites)
#iProcSites <- 2:3
#try(rm(REddy.mm.all)); try(REddy.yy.all)


computeSite <- function(siteName, fileName, scenConf){
	year         <- as.numeric(substr(fname, nchar(fname)-28, nchar(fname)-25))
	latLongSite  <- unlist(subset(latLongSites, site==siteName)[1,2:4])
	#+++ Loading data from MR partitioning and data for running the partitioning
	dfall             <- fLoadTXTIntoDataframe(fname, file.path(path,"MR_GL_partitioning"))
	.tmp.readPvWave <- function(){
		fname.PVwave <- paste(siteName,'.',year,'.','DataSetafterFluxpartGL2010.txt', sep="")
		dfall.Lass.PVwave <- read.table(file.path(path,"MR_GL_partitioning",fname.PVwave,sep=""),skip=2)
		title <- scan(file.path(path,"MR_GL_partitioning",fname.PVwave,sep=""), nlines = 1, sep = "", strip.white=TRUE,
				what=list(rep('character',17))) 
		names(dfall.Lass.PVwave) <- title[[1]]
	}
	dfall$PotRad <- as.numeric(fCalcPotRadiation(dfall$julday,dfall$Hour,latLongSite["lat"],latLongSite["long"],latLongSite["timeOffset"]))
	# here fake potential radiation to be high on all records where Lasslop Partitioning quantified non night 
	dfall$day    <- (1 - dfall$night)*100  
	dfall_posix  <- ds <- fConvertTimeToPosix(dfall, 'YMDH', Year.s = 'Year', Month.s='Month', Day.s = 'Day', Hour.s = 'Hr')
	# ds <- subset(dfall_posix, Month==5L)
	#
	# START - RUN THE REddyProc DT partitioning
	ctrl <- scenConf$ctrl[[1]]
	dsRes <- partitionNEEGL(ds,NEEVar.s="NEE_f",QFNEEVar.s="NEE_fqc",QFNEEValue.n = 0,NEESdVar.s="NEE_fs_unc",
			TempVar.s="Tair_f",QFTempVar.s="Tair_fqc",QFTempValue.n=0,VPDVar.s="VPD_f",QFVPDVar.s="VPD_fqc",
			QFVPDValue.n=0,RadVar.s="Rg",PotRadVar.s="day",Suffix.s="",
			controlGLPart=ctrl
	)
	dsRes$DateTime <- ds$DateTime
	.tmp.debug <- function(){
		dsResDefault <- partitionNEEGL(ds,NEEVar.s="NEE_f",QFNEEVar.s="NEE_fqc",QFNEEValue.n = 0,NEESdVar.s="NEE_fs_unc",
				TempVar.s="Tair_f",QFTempVar.s="Tair_fqc",QFTempValue.n=0,VPDVar.s="VPD_f",QFVPDVar.s="VPD_fqc",
				QFVPDValue.n=0,RadVar.s="Rg",PotRadVar.s="day",Suffix.s="",
				controlGLPart=partGLControl()
		)
		dsResDefault$DateTime <- ds$DateTime
		dsRes$Day <- dsResDefault$Day <- ds$Day
		plot(FP_beta ~ DateTime, dsRes, col="red", pch="x")
		points(FP_beta ~ DateTime, dsResDefault)
		# in partGLFitLRCOneWindow:
		#if( as.POSIXlt(dsDay$sDateTime[1])$mday+2L > 27 ) recover()
		# save(dsDay, file="tmp/dsDayDebug.RData")
		load(file="tmp/dsDayDebug.RData")
		dsDay <- arrange_(dsDay, ~Rg)
		plot( -NEE ~ Rg, dsDay)
		subset( dsResDefault, is.finite(FP_beta), c("Day","FP_beta","FP_alpha","FP_E0","FP_k","FP_RRef","FP_RRef_Night"))
		subset( dsRes, is.finite(FP_beta), c("Day","FP_beta","FP_alpha","FP_E0","FP_k","FP_RRef","FP_RRef_Night"))
		DayInspect <- 11
		(thetaDef <- unlist(subset(dsResDefault, Day == DayInspect & is.finite(FP_beta), c("FP_k","FP_beta","FP_alpha","FP_RRef","FP_E0","FP_RRef_Night") )))
		(thetaOpt <- unlist(subset(dsRes, Day == DayInspect & is.finite(FP_beta), c("FP_k","FP_beta","FP_alpha","FP_RRef","FP_E0","FP_RRef_Night") )))
		lrcFitter <- RectangularLRCFitter()
		dsDay$GPPDefault <- lrcFitter$predictGPP( dsDay$Rg, thetaDef["FP_beta"], thetaDef["FP_alpha"])
		dsDay$GPPOpt <- lrcFitter$predictGPP( dsDay$Rg, thetaOpt["FP_beta"], thetaOpt["FP_alpha"])
		dsDay$NEPDefault <- lrcFitter$predictLRC( thetaDef, dsDay$Rg, dsDay$VPD, dsDay$Temp)$NEP
		dsDay$NEPOpt <- lrcFitter$predictLRC( thetaOpt, dsDay$Rg, dsDay$VPD, dsDay$Temp)$NEP
		ylim <- range(-dsDay$NEE, na.rm=TRUE); ylim[2] <- max(ylim[2], thetaDef["FP_beta"], thetaOpt["FP_beta"], na.rm=TRUE )*1.1
		maxRg <- max(1200,max(dsDay$Rg,na.rm=TRUE))
		maxRg <- max(2000,max(dsDay$Rg,na.rm=TRUE))
		#
		plot( -NEE ~ Rg, dsDay)
		lines( NEPDefault ~ Rg, dsDay )
		lines( NEPOpt ~ Rg, dsDay, col="red" )
		points(maxRg, thetaDef["FP_beta"]); points(maxRg, thetaOpt["FP_beta"], col="red", pch="x")
		#
		plot( GPPDefault ~ Rg, dsDay, xlim=c(0,maxRg), ylim=ylim)
		plot( GPPOpt ~ Rg, dsDay, xlim=c(0,maxRg), ylim=ylim, col="red")
		abline(h=thetaDef["FP_beta"]); abline(h= thetaOpt["FP_beta"], col="red", pch="x")
		#
		RgSeq <- seq(0,maxRg, length.out=80)
		predGPP <- lrcFitter$predictLRC( thetaOpt, Rg=RgSeq, VPD=0, Temp=NA)$GPP
		lines( predGPP ~ RgSeq, col="red")
	}
	### add modelled NEE
	dsRes$NEE_DT <- -(dsRes$GPP_DT - dsRes$Reco_DT)
	## save data frames 
	write.table(dsRes,file=file.path(path,"Results",scenConf$ouputPath,paste0(siteName,".txt")),row.names=F,col.names=T)
	#  
	## the _agg columns are used for aggregation in aggregate() function below
	julday_agg <- c(1,dfall$julday[1:(nrow(dsRes)-1)])
	Month_agg  <- c(1,dfall$Month[1:(nrow(dsRes)-1)])
	Year_agg   <- c(dfall$Year[1],dfall$Year[1:(nrow(dsRes)-1)])
	# Aggregation Monthly per site
	df.REddy.mm  <- aggregate(dsRes,by=list(Year_agg=Year_agg, Month_agg=Month_agg),mean,na.rm=T)
	# sd
	df.REddy.mm$Reco_DT_SD   <- aggregate(dsRes$Reco_DT_SD,by=list(Year_agg=Year_agg, Month_agg=Month_agg),aggregate.sd)[,2]
	df.REddy.mm$GPP_DT_SD    <- aggregate(dsRes$GPP_DT_SD,by=list(Year_agg=Year_agg, Month_agg=Month_agg),aggregate.sd)[,2]
	df.REddy.mm  <- cbind(siteName,df.REddy.mm)
	# Aggregation Annual
	df.REddy.yy  <- aggregate(dsRes,by=list(Year_agg=Year_agg),mean,na.rm=T)
	# sd
	df.REddy.yy$Reco_DT_SD   <- aggregate(dsRes$Reco_DT_SD,by=list(Year_agg=Year_agg),aggregate.sd)[,2]
	df.REddy.yy$GPP_DT_SD    <- aggregate(dsRes$GPP_DT_SD,by=list(Year_agg=Year_agg),aggregate.sd)[,2]
	df.REddy.yy  <- cbind(siteName,df.REddy.yy)
	#
	list(
			mm = df.REddy.mm
			,yy = df.REddy.yy  
	)
}


#ansList <- lapply( iProcSites, function(s){
ansList <- foreach (s=iProcSites, .errorhandling="pass", .packages=c("REddyProc","plyr","mlegp","logitnorm")) %dopar% {
	siteName 	   <- sites[s] 
	fname        <- flist[s]
	message("-------- starting site ",siteName)
	computeSite(siteName,fname, scenConf)
}# end of site loop
#) 

str(ansList,  max.level =2)

REddy.mms <- lapply(ansList, "[[" , 1L)
REddy.mm.all <- do.call( rbind, REddy.mms)
REddy.yys <- lapply(ansList, "[[" , 2L)
REddy.yy.all <- do.call( rbind, REddy.yys)

## save as RData:
#save(NT_vs_DT_REddy,DT_REddy_vs_pvwave,file=file.path(path,"Results",scenConf$ouputPath,"/eval_metrics.RData")) # 1) evaluation metrics
save(REddy.mm.all,file=file.path(path,"Results",scenConf$ouputPath,"all_sites_monthly.RData"))   # 2) monthly aggregated results for all sites
save(REddy.yy.all,file=file.path(path,"Results",scenConf$ouputPath,"all_sites_annual.RData"))    # 3) annual aggregated results for all sites



.tmp.lookForHighPar <- function(){
	#duplated column names?
	#ds <- dfall_posix[,!(names(dfall) %in% c("Day","Hour"))]
	ds <- dfall_posix[,c("DateTime","Year","Month","julday","Rg","NEE_f","NEE_fs_unc","Tair","VPD","PotRad")]
	dailyMaxPar <- ds %>% group_by_(~julday) %>% summarize_(Rg=~max(Rg, na.rm=TRUE))
	arrange_(dailyMaxPar, ~desc(Rg))
	ggplot( dailyMaxPar, aes(julday, Rg)) + geom_point()
	# extract data of day 174
	dss <- subset(ds, julday %in% (142+(0:3)))
	dss$Temp <- dss$Tair
	dss$isDay <- (dss$PotRad > 20)
	dss$sdNEE <- dss$NEE_fs_unc
	dss$NEE <- dss$NEE_f
	isValidDayRecNoVPDConstraint <- !is.na(dss$isDay) & dss$isDay & !is.na(dss$NEE) & !is.na(dss$sdNEE) & !is.na(dss$Temp) & !is.na(dss$Rg)
	dsDay <- dss[isValidDayRecNoVPDConstraint,]  
	p1 <- ggplot( dsDay, aes(Rg,-NEE, col=Temp)) + geom_point(); p1
	
	ctrl <- partGLControl( isUsingLasslopQualityConstraints=TRUE )
	#ctrl <- partGLControl( )
	lrcFitter <- RectangularLRCFitter()
	resOpt <- resOpt0 <- lrcFitter$fitLRC(dsDay, E0=130, sdE0=50, RRefNight=1
			, controlGLPart=ctrl)
	(thetaOpt <- resOpt$thetaOpt)
	dsPred <- data.frame(Rg = seq(0,2000, length.out=80), Temp=median(dsDay$Temp))
	dsPred$GPP <- lrcFitter$predictLRC( thetaOpt, Rg=dsPred$Rg, VPD=0, Temp=dsPred$Temp)$GPP
	#ggplot( dsPred, aes(Rg, GPP)) + geom_line()
	p1 + geom_line(data=dsPred, aes(Rg, GPP))
	
	dsDay$NEP <- lrcFitter$predictLRC( thetaOpt, Rg=dsDay$Rg, VPD=dsDay$VPD, Temp=dsDay$Temp)$NEP
	p1 + geom_line(data=dsDay, aes(Rg, NEP))
	
	
}



