# copied from cluster form Mircos work_3/REddyProcRelease/RHvsNRH
#    example of reading LaThuille data


####------- Loading libraries -------------####
#library("ncdf")
#twutz: ncdf package is old and not any more supported for windows

#install.packages(c("ncdf4", "plyr"), repos="http://cran.r-project.org")

library(ncdf4)
open.ncdf <- nc_open
get.var.ncdf <- function(...){ as.vector(ncvar_get(...)) }
library(sirad)
library(chron)
bgiDir <- if( .Platform$OS.type=="windows") "M:" else "/Net/Groups/BGI"

#sourceDir <- function(path, trace = TRUE, ...) {
#  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
#    if(trace) cat(nm,":")
#    source(file.path(path, nm), ...)
#    if(trace) cat("\n")
#  }
#}

.tmp.f <- function(){
	#sourceDir('/Volumes/BGI/work_3/REddyProcRelease/RHvsNRH/source/sourceREddyProc/')
	#install.packages('mlegp')
	install.packages(file.path(bgiDir,'people/mmiglia/Rcodes/REddyProc-nonrectangular'), repos = NULL, type="source")
	#install.packages('mlegp')
	#install.packages('logitnorm')
	#install.packages('/Volumes/BGI/people/mmiglia/Rcodes/REddyProc-nonrectangular', repos = NULL, type="source")
	install.packages('ncdf4', repos = "http://cran.us.r-project.org")
}

if( !exists("partitionNEEGL") ) library(REddyProc)	# only load library if not sourced its already
library(plyr)
require(parallel)
require(SDMTools)
library(foreach)
#library(doMC)		#twutz: doMC is not supported for Windows any more, may swith to same functionality with doParallel	
library(doParallel)
#library(twDev)

#install.packages("mlegp")
#install.packages(c("mlegp", "lmodel2"), repos="http://cran.r-project.org")
#
library(lmodel2)

#Source REddyProc routine modified for Trevor's analysis

####------- Source Scripts and Utilities-------------####

#Set path for Berkeley database and codes

CLUSTER<- if( .Platform$OS.type=="windows") FALSE else TRUE
#CLUSTER<-FALSE

#registerDoMC(8) 
#doMC::registerDoMC(cores=48) # or however many cores you have access to
if( CLUSTER ){
 registerDoParallel(cores=32L)
} else {
 registerDoParallel(cores=2L)
}

# setting file paths
mainroot<-file.path(bgiDir,'work_3/REddyProcRelease/RHvsNRH')
setwd(mainroot)
# bsub -q BTM -n16 -R "rusage[mem=16384], span[hosts=1]" R32 CMD BATCH --no-save --no-restore source/forCluster/MAIN_Partitioning_DiffLRC_Markus_parallel_nRectBranch.R 
#| tee logCluster.txt
ipath <- ifelse(isLaThuile,
	  file.path(bgiDir,'data/DataStructureMDI/DATA/site/Fluxnet/halfhourly/level5_new_v2_newRpot_UncNew/Data/'), 
	  file.path(bgiDir,'data/DataStructureMDI/DATA/Incoming/Fluxnet/berkeley_012016/Data/HH/2016_11/'))
isAfrican<-FALSE
ipath <- ifelse(isAfrican,
                file.path(bgiDir,'work_3/REddyProcRelease/RHvsNRH/AfriSites'), 
                file.path(bgiDir,'data/DataStructureMDI/DATA/site/Fluxnet/halfhourly/level5_new_v2_newRpot_UncNew/Data/'))
  #ipath<-'/Net/Groups/BGI/data/DataStructureMDI/DATA/Incoming/Fluxnet/berkeley_012016/Data/HH/2016_07/'
  #ls /Volumes/BGI/people/uweber/_data/Fluxcom/data/AFRIFLUXCOM/sites/
  plotroot<-file.path(mainroot,'plots')

#sourceDir <- function(path, trace = TRUE, ...) {
#  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
#    if(trace) cat(nm,":")           
#    #if (nm != 'MAIN_Partitioning_DiffLRC_Markus_parallel.R' | nm != "PostProcessing_xMarkus.R") {
#    source(file.path(path, nm), ...)
#    if(trace) cat("\n")
#  }
#}

.tmp.overideREddyProc <- function(){
	sourceDir( file.path(mainroot,'source/sourceREddyProc') )
	source( file.path(mainroot,'source/sourceREddyProc/PartitioningLasslop10_RH_NRH.R')) #CHANGE THE PATH
	#source(paste(mainroot,'R/PartitioningLasslop10_RH_NRH.R', sep="")) #CHANGE THE PATH
	
	#source('/Users/mmiglia/Documents/Rcodes/REddyProc/R/PartitioningLasslop10_RH_NRH.R') #CHANGE THE PATH
}
source( file.path(mainroot,'source/sourceREddyProc/utilities.R'))  # from_umolm2s1_gCm2hh

scenario <- "twutz_debug"
#outRoot <- 'Rdata'
outDir <- paste0('Rdata_',scenario) 
plotroot<-file.path(mainroot,paste0('plots_',scenario))
RHroot<-file.path(mainroot,outDir,'RHRF')
NRHroot<-file.path(mainroot,outDir,'NRHRF')
STATSroot<-file.path(mainroot,outDir)

flist<-list.files(path = ipath, pattern="*.nc")
sites <- substr(flist, 1,6)

sitesMircoUsed <- c("BR-Ma2","BR-Sa1","CA-Let","CA-NS7","CA-TP3","CH-Oe2","CN-HaM","DE-Hai","DK-Sor","ES-E1","ES-VDA","FI-Hyy","FI-Kaa","FR-Fon","FR-Gri","FR-Hes","FR-Lq1","FR-Lq2","FR-Pue","IE-Dri","IL-Yat","IT-Amp","IT-MBo","IT-Pia","IT-SRo","JP-Tef","PT-Esp","RU-Cok","SE-Nor","TH98_new","US-Ha1","US-MMS","US-SO2","US-Ton","VU-Coc")
sitesRestricted <- c("BR-Ma2","BR-Sa1","CA-Let","CN-HaM","JP-Tef","US-SO2")
%%siteMapping = list(TH98_new = "DE-Tha", "ES-E1"="ES-ES1", "IT-Pia"="IT-PT1")
siteMapping = list(TH98_new = "DE-Tha", "ES-E1"="ES-ES1")
#
sitesUsed <- setdiff( c( unlist(siteMapping), setdiff(sitesMircoUsed, sitesRestricted)), names(siteMapping))
which( !(sitesUsed %in% sites)) # all available
js <- match(sitesUsed, sites)
n<-length(js)

flist[js]
j <- js[1]


loadLaThuilleData <- function(
	fname
	, ipath=			##<< path to LaThuille datasets
			file.path(bgiDir,'data/DataStructureMDI/DATA/site/Fluxnet/halfhourly/level5_new_v2_newRpot_UncNew/Data/')			 
	, thrRgDaytime=4	##<< Threshold global radiation for day/nighttime Wm2
){
	print(paste('###################### Analyzing ', fname))
	nc <- open.ncdf( file.path(ipath,fname))
	#Data.F <- fLoadFluxNCIntoDataframe(c('year','hour','julday','Rg', 'NEE', 'VPD','Tair','NEE_fs','NEE_fsd','H','LE','H_f','LE_f','NEE_f','NEE_fwin','NEE_fqc',
	#                                     'Tair_f','Rg_f','VPD_f','Tair_fqc','Rg_fqc','VPD_fqc'), fname, path, 'RNetCDF')  
	#grep("^rH",names(nc$var), value=TRUE)	
	Data.F <- data.frame(SW_IN_F=get.var.ncdf(nc, "Rg_f"),
			SW_IN_F_QC=get.var.ncdf(nc, "Rg_fqcOK"),
			NEE=get.var.ncdf(nc, "NEE_f"),
			NEE_QC=get.var.ncdf(nc, "NEE_fqc"),
			NEE_SE=get.var.ncdf(nc, "NEE_fs_unc"),                         
			VPD_F=get.var.ncdf(nc, "VPD_f"),
			VPD_F_QC=get.var.ncdf(nc, "VPD_fqcOK"),
			TA_F=get.var.ncdf(nc, "Tair_f"),
			TA_F_QC=get.var.ncdf(nc, "Tair_fqcOK"),
			year=get.var.ncdf(nc, "year"),
			month=get.var.ncdf(nc, "month"),
			day=get.var.ncdf(nc, "day"),
			hour=get.var.ncdf(nc, "hour"),
			Rg_pot=get.var.ncdf(nc, "Rg_pot"),
			rH=get.var.ncdf(nc, "rH_f")
	)
	Data.F$DAY[Data.F$SW_IN_F > thrRgDaytime] <- 1 #Set 1 - DAY  
	Data.F$DAY[Data.F$SW_IN_F < thrRgDaytime] <- 0 #Set 0 - NIGHT
	nRecInDay.i=48
	if(fname == "US-Ha1.HH.1991.2012.nc" ) nRecInDay.i=24  #To be set to 24 for sites with hourly data 
	Data.F$VPD_F_QC<-1-Data.F$VPD_F_Q
	Data.F$TA_F_QC<-1-Data.F$TA_F_QC
	Data.F$SW_IN_F_QC<-1-Data.F$SW_IN_F_QC
	Data.F
}
	
resForeach <- foreach (j=js, .errorhandling="pass") %dopar% {  #THIS IS THE CORRECT FOR PARALLELIZATION use js only for  sites not used yet
#ans <- foreach (j=c(1:n)) %dopar% {  

#for(j in indexfiles[c(4:5)]){
#for(j in c(1:length(flist))){
  
  fname<-flist[j]
  Data.F <- loadLaThuilleData(fname)
  nRecInDay.i <- if(fname == "US-Ha1.HH.1991.2012.nc" ) 24 else 48
  Data.F$sdNEE <- Data.F$NEE_SE
  #Data.F$sdNEE[!is.finite(Data.F$sdNEE)] <- pmax(0.7, abs(Data.F$NEE[!is.finite(Data.F$sdNEE)] * 0.2))
  # with many missing VPD, fitting has not enough records, compute missings from rH and Tair
  iMissingVPD <- which(!is.finite(Data.F$VPD_F))
  Data.F$VPD_F[iMissingVPD] <- fCalcVPDfromRHandTair(Data.F$rH[iMissingVPD], Data.F$TA_F[iMissingVPD])		  
  
  #MODIFIED BY MIRCO (Gap filling VPD with MDS)
  if (VPDgapfilling == TRUE){
    #convert 
    tmp.df<-Data.F
    
    tmp.df<-fConvertTimeToPosix(tmp.df, 'YMDH', Year.s = 'year', Month.s='month', Day.s = 'day', Hour.s = 'hour')
    colnames(tmp.df)[which(names(tmp.df) == "TA_F")] <- "Tair"
    colnames(tmp.df)[which(names(tmp.df) == "SW_IN_F")] <- "Rg"
    colnames(tmp.df)[which(names(tmp.df) == "VPD_F")] <- "VPD"
    colnames(tmp.df)[which(names(tmp.df) == "NEE")] <- "NEE"

    EddyProc.C <- sEddyProc$new('test', tmp.df, c('VPD','NEE','Rg','Tair'))
    
    #+++ Fill gaps in variables with MDS gap filling algorithm
    EddyProc.C$sMDSGapFill(c('Tair'), FillAll.b=TRUE)
    EddyProc.C$sMDSGapFill(c('Rg'), FillAll.b=TRUE)
    EddyProc.C$sMDSGapFill(c('VPD'), FillAll.b=TRUE)
    
    Data.F$VPD_F<-EddyProc.C$sTEMP$VPD_f
    
  }
    
  # negative Rg causes big problems in fitting, remove those values
  Data.F$SW_IN_F[ Data.F$SW_IN_F < 0 ] <- 0 # MODIFIED BY MIRCO : because creates problems in high latitude sites
  dss <- Data.F						# may take a subset for debugging
  #dss <- Data.F[ Data.F$year == 2006,]			# all VPD missing
  #dss <- Data.F[ Data.F$year == 2005,]			
  
  #run year by year
  Data.F$year_shift<-c(Data.F$year[1],Data.F$year[1:(length(Data.F$year)-1)])
  yy<-unique(Data.F$year_shift)
  
  for (y in yy) {
    
    print(paste('....Selecting year', y))
    
    dsstmp <- Data.F[ Data.F$year_shift == y,]
  
      tempodf <- partitionNEEGL(dsstmp,NEEVar.s="NEE",QFNEEVar.s="NEE_QC",QFNEEValue.n = 0,NEESdVar.s="sdNEE",
                                       TempVar.s="TA_F",QFTempVar.s="TA_F_QC",VPDVar.s="VPD_F",QFVPDVar.s="VPD_F_QC",
                                       RadVar.s="SW_IN_F",PotRadVar.s="Rg_pot", QFVPDValue.n=0,QFTempValue.n=0, Suffix.s="", nRecInDay= nRecInDay.i
							   		   ,lrcFitter=NonrectangularLRCFitter()
                                       ,controlGLPart=partGLControl(nBootUncertainty=5L, isAssociateParmsToMeanOfValids=FALSE, 
                                                                   isLasslopPriorsApplied=TRUE, smoothTempSensEstimateAcrossTime = FALSE,
                                                                   isBoundLowerNEEUncertainty=FALSE,
                                                                   fixedTempSens=data.frame(E0=200, sdE0=40, RRef=50)))
  
    #,fixedTempSens=data.frame(E0=100, sdE0=80, RRef=50)
    
    ifelse(y == yy[1],  df.REddyNRHRFtmp <-tempodf, df.REddyNRHRFtmp <- rbind(df.REddyNRHRFtmp, tempodf) )
  
  }
  
  #MIRCO Added Bounded uncertainty FALSE
  #something like:
  #  
  #  processData <- function(dssub){
  #    # do something with data.frame ds (initialize REddyProc Class, ...)
  #    # The return value must be a data.frame or tibble
  #df.REddyNRHRFtmp <- partitionNEEGL(dssub,NEEVar.s="NEE",QFNEEVar.s="NEE_QC",QFNEEValue.n = 0,NEESdVar.s="sdNEE",
  #                                   TempVar.s="TA_F",QFTempVar.s="TA_F_QC",VPDVar.s="VPD_F",QFVPDVar.s="VPD_F_QC",
  #                                   RadVar.s="SW_IN_F",PotRadVar.s="Rg_pot", QFVPDValue.n=0,QFTempValue.n=0, Suffix.s="", nRecInDay= nRecInDay.i
  #                                   ,lrcFitter=NonrectangularLRCFitter()
  #                                   ,controlGLPart=partGLControl(nBootUncertainty=5L, isAssociateParmsToMeanOfValids=FALSE, 
  #                                                                isLasslopPriorsApplied=TRUE, smoothTempSensEstimateAcrossTime = FALSE))
  
  #    ans <- data.frame( a = 1:4, b=3:6 )
    #return(df.REddyNRHRFtmp)
   #}
  
  #dsResult <- dss %>% group_by(year) %>% do( processData )
  
  # first column will be the year
  
  df.REddyNRHRF<-cbind(df.REddyNRHRFtmp, dss)
  
  #-- Aggregating daily
  df.REddyNRHRF<-fConvertTimeToPosix(df.REddyNRHRF, 'YMDH', Year.s = 'year', Month.s='month', Day.s = 'day', Hour.s = 'hour')
  short.date = format(df.REddyNRHRF$DateTime, "%Y/%m/%d")
  df.REddyNRHRF$short.date<-short.date 
  if(!isLaThuile){
      df.REddyNRHRF.dd<-ddply(df.REddyNRHRF, "short.date", summarise, GPP_DT = sum(from_umolm2s1_gCm2hh(GPP_DT)),
                          Reco_DT = sum(from_umolm2s1_gCm2hh(Reco_DT)),
                          NEE = sum(from_umolm2s1_gCm2hh(NEE)),
						  FP_qc = max(FP_qc),
				          GPP_DT_VUT_MEAN_F15 = sum(from_umolm2s1_gCm2hh(GPP_DT_VUT_MEAN_F15)),
                          GPP_NT_VUT_MEAN_F15=sum(from_umolm2s1_gCm2hh(GPP_NT_VUT_MEAN_F15)),
                          RECO_DT_VUT_MEAN_F15=sum(from_umolm2s1_gCm2hh(RECO_DT_VUT_MEAN_F15)),
                          RECO_NT_VUT_MEAN_F15=sum(from_umolm2s1_gCm2hh(RECO_NT_VUT_MEAN_F15)))
  }
  if(isLaThuile){
    df.REddyNRHRF.dd<-ddply(df.REddyNRHRF, "short.date", summarise, GPP_DT = sum(from_umolm2s1_gCm2hh(GPP_DT)),
                            Reco_DT = sum(from_umolm2s1_gCm2hh(Reco_DT)),
                            NEE = sum(from_umolm2s1_gCm2hh(NEE)),
							FP_qc = max(FP_qc)
							#GPP_DT_VUT_MEAN_F15 = sum(from_umolm2s1_gCm2hh(GPP_DT_VUT_MEAN_F15)),
                            #GPP_NT_VUT_MEAN_F15=sum(from_umolm2s1_gCm2hh(GPP_NT_VUT_MEAN_F15)),
                            #RECO_DT_VUT_MEAN_F15=sum(from_umolm2s1_gCm2hh(RECO_DT_VUT_MEAN_F15)),
                            #RECO_NT_VUT_MEAN_F15=sum(from_umolm2s1_gCm2hh(RECO_NT_VUT_MEAN_F15))
							)    
  }
 
  print(paste("Saving ....",NRHroot,"hh/NRHRF.",fname,".Rdata",sep=""))
  
  save(df.REddyNRHRF   ,file=file.path(NRHroot,"/hh/",paste0("NRHRF.",fname,".Rdata")))
  save(df.REddyNRHRF.dd,file=file.path(NRHroot,"/dd/",paste0("NRHRF.",fname,".Rdata")))
  #load(file.path(NRHroot,"hh",paste0("NRHRF.",fname,".Rdata")))
  #load(file.path(NRHroot,"dd",paste0("NRHRF.",fname,".Rdata")))
  
  #plot(Data.F$VPD_F_QC)
  #plot(Data.F$TA_F_QC)
  #plot(Data.F$SW_IN_F_QC)
  
  #DOUBLECHECK!!
  
  #df.REddyRHRF <- partitionNEEGL_2LRC(Data.F,NEEVar.s="NEE",QFNEEVar.s="NEE_QC",QFNEEValue.n = 0,NEESdVar.s="NEE_SE",
  #                                    TempVar.s="TA_F",QFTempVar.s="TA_F_QC",VPDVar.s="VPD_F",QFVPDVar.s="VPD_F_QC",
  #                                    RadVar.s="SW_IN_F",PotRadVar.s="DAY",Suffix.s="", nRecInDay.i= nRecInDay.i,NRHRfunction=FALSE,
  #                                   controlGLPart.l=partGLControl(nBootUncertainty=0L, isAssociateParmsToMeanOfValids=FALSE, 
  #                                                                  isLasslopPriorsApplied=TRUE, smoothTempSensEstimateAcrossTime = FALSE))
  
  for (y in yy) {
    
    print(paste('....Selecting year', y))
    
    dsstmp <- Data.F[ Data.F$year_shift == y,]
    
    tempodf <- partitionNEEGL(dsstmp,NEEVar.s="NEE",QFNEEVar.s="NEE_QC",QFNEEValue.n = 0,NEESdVar.s="NEE_SE",
                                      TempVar.s="TA_F",QFTempVar.s="TA_F_QC",VPDVar.s="VPD_F",QFVPDVar.s="VPD_F_QC",
                                      RadVar.s="SW_IN_F",PotRadVar.s="Rg_pot", QFVPDValue.n=0,QFTempValue.n=0, Suffix.s="", nRecInDay= nRecInDay.i
                                      ,lrcFitter=RectangularLRCFitterCVersion()
                                      ,controlGLPart=partGLControl(nBootUncertainty=0L, isAssociateParmsToMeanOfValids=FALSE, 
                                                                   isLasslopPriorsApplied=TRUE, smoothTempSensEstimateAcrossTime = FALSE,
                                                                   isBoundLowerNEEUncertainty=FALSE,
                                                                   fixedTempSens=data.frame(E0=200, sdE0=40, RRef=50)))
    #MIRCO Added Bounded uncertainty FALSE
     
    ifelse(y == yy[1],  df.REddyRHRFtmp <-tempodf, df.REddyRHRFtmp <- rbind(df.REddyRHRFtmp, tempodf) )
    
  }
  
  df.REddyRHRF<-cbind(df.REddyRHRFtmp, dss)
  
  #-- Aggregating daily
  df.REddyRHRF<-fConvertTimeToPosix(df.REddyRHRF, 'YMDH', Year.s = 'year', Month.s='month', Day.s = 'day', Hour.s = 'hour')
  short.date = format(df.REddyRHRF$DateTime, "%Y/%m/%d")
  df.REddyRHRF$short.date<-short.date 
  
  if(!isLaThuile){
  df.REddyRHRF.dd<-ddply(df.REddyRHRF, "short.date", summarise, GPP_DT = sum(from_umolm2s1_gCm2hh(GPP_DT)),
                          Reco_DT = sum(from_umolm2s1_gCm2hh(Reco_DT)),
                          NEE = sum(from_umolm2s1_gCm2hh(NEE)),
						  FP_qc = max(FP_qc),
                          GPP_DT_VUT_MEAN_F15 = sum(from_umolm2s1_gCm2hh(GPP_DT_VUT_MEAN_F15)),
                          GPP_NT_VUT_MEAN_F15=sum(from_umolm2s1_gCm2hh(GPP_NT_VUT_MEAN_F15)),
                          RECO_DT_VUT_MEAN_F15=sum(from_umolm2s1_gCm2hh(RECO_DT_VUT_MEAN_F15)),
                          RECO_NT_VUT_MEAN_F15=sum(from_umolm2s1_gCm2hh(RECO_NT_VUT_MEAN_F15)))
  }
  
  if(isLaThuile){
    df.REddyRHRF.dd<-ddply(df.REddyRHRF, "short.date", summarise, GPP_DT = sum(from_umolm2s1_gCm2hh(GPP_DT)),
                           Reco_DT = sum(from_umolm2s1_gCm2hh(Reco_DT)),
                           NEE = sum(from_umolm2s1_gCm2hh(NEE)),
						   FP_qc = max(FP_qc)
				   )
  }
  
  outFBasename <- paste0("RHRF.",fname,".Rdata")
  print(paste0("Saving ....",outFBasename," to ",RHroot))
  
  save(df.REddyRHRF,file=file.path(RHroot,"/hh/",outFBasename))
  save(df.REddyRHRF.dd,file=file.path(RHroot,"/dd/",outFBasename))
  
  #only where next fitted parameter set is not too far away (quality flag < 2)
  iClose <- which((df.REddyRHRF$FP_qc < 2) & (df.REddyNRHRF$FP_qc < 2))
  resRect <- df.REddyRHRF[iClose, ,drop=FALSE] 
  resNonrect <- df.REddyNRHRF[iClose, ,drop=FALSE] 
  iCloseDay <- which((df.REddyRHRF.dd$FP_qc < 2) & (df.REddyNRHRF.dd$FP_qc < 2))
  resRectDay <- df.REddyRHRF.dd[iCloseDay, ,drop=FALSE] 
  resNonrectDay <- df.REddyNRHRF.dd[iCloseDay, ,drop=FALSE]
  pdf(file.path(plotroot, paste0("scatterplots.",fname,'.pdf')))
	  tmp <- suppressMessages(R.utils::captureOutput({
	  		scatter_1by1(x=resRect$GPP_DT, y=resNonrect$GPP_DT, main=paste('GPP', fname), xlab="GPPhh RHRF", ylab="GPPhh NRHRF",OnetoOne=TRUE,RMA=TRUE)
			scatter_1by1(x=resRect$Reco_DT, y=resNonrect$Reco_DT, main=paste('Reco', fname), xlab="Recohh RHRF", ylab="Recohh NRHRF",OnetoOne=TRUE,RMA=TRUE)
			try( scatter_1by1(x=resRect$FP_beta, y=resNonrect$FP_beta, main=paste('beta', fname), xlab="beta RHRF", ylab="beta NRHRF",OnetoOne=TRUE,RMA=TRUE), silent=TRUE)
			try( scatter_1by1(x=resRect$FP_RRef, y=resNonrect$FP_RRef, main=paste('RRef', fname), xlab="RRef RHRF", ylab="RRef NRHRF",OnetoOne=TRUE,RMA=TRUE), silent=TRUE)
			# try because daily aggregates might all be NA
			try(scatter_1by1(x=resRectDay$GPP_DT, y=resNonrectDay$GPP_DT, main=paste('GPP', fname), xlab="GPPhh RHRF", ylab="GPPhh NRHRF",OnetoOne=TRUE,RMA=TRUE), silent=TRUE)
			try(scatter_1by1(x=resRectDay$Reco_DT, y=resNonrectDay$Reco_DT, main=paste('Reco', fname), xlab="Recohh RHRF", ylab="Recohh NRHRF",OnetoOne=TRUE,RMA=TRUE), silent=TRUE)
		}))
  dev.off()
  
  stats.out<-rbind(modeval(resNonrect$GPP_DT, resRect$GPP_DT),
                    modeval(resNonrect$Reco_DT, resRect$Reco_DT),
                    modeval(resNonrect$FP_beta, resRect$FP_beta),
                    modeval(resNonrect$FP_RRef, resRect$FP_RRef))
  row.names(stats.out)<-c("GPP_DT","Reco_DT","FP_beta","FP_RRef")  
  
  print(paste("....Saving ",STATSroot,"stats_",fname,".Rdata",sep=""))
  save(stats.out, file=file.path(STATSroot,paste0("stats_",fname,".Rdata")))
  j	# return value
}

# write outputs to trace errors
names(resForeach) <- flist[js]
resForeachOutFilename <- file.path(STATSroot,paste0("resForeach.Rdata"))
print(paste("....Saving resForeach to ",resForeachOutFilename))
save(resForeach, file=resForeachOutFilename)

print("finished script.")

.tmp.f <- function(){
	load( resForeachOutFilename )
	(iFailed <- which(sapply( resForeach, inherits, "error")))
	resForeach[iFailed]
}

#.tmp.f <- function(){
#	drs <- subset(resRect, Rg_pot>580)
#	drs <- resRect[1:200,]
#	#summary(drs$GPP_DT)
#	plot( drs$GPP_DT, ylim=range(c(drs$GPP_DT+drs$GPP_DT_SD,drs$GPP_DT-drs$GPP_DT_SD)) )
#	lines( drs$GPP_DT+drs$GPP_DT_SD, col="blue", pch="-")
#	lines( drs$GPP_DT-drs$GPP_DT_SD, col="blue", pch="-")
#}

.tmp.testErrorInForEach <- function(){
	tmpJs <- c(1,2,5)
	ans <- foreach (j=tmpJs, .errorhandling="pass") %dopar% {
		if( j %in% c(2,5) ) stop("test throwing error")
		j
	}
	names(ans) <- tmpJs
}

#cd /Net/Groups/BGI/work_3/REddyProcRelease/RHvsNRH
#mkdir F15
#cp -R plots F15/
#cp -R Rdata F15/
#rm plots/*.*
#rm Rdata/NRHRF/*.*
#rm Rdata/RHRF/*.*
#rm Rdata/*.*
#rm Rdata/NRHRF/dd/*.*
#rm Rdata/NRHRF/hh/*.*
#rm Rdata/RHRF/hh/*.*
#rm Rdata/RHRF/dd/*.*