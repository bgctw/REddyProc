#Script for testing the new partitioning GL for REddyProc
library(REddyProc)
library(sirad)
library(scales) # for plotting (function alpha())

path  <- "M:/work_3/REddyProcRelease/Eval_GL_Partitioning/"
flist <- list.files(paste0(path,"MR_GL_partitioning/"), pattern="*DataSetafterFluxpart.txt")
sites <- substr(flist,1,6)

latLongSites <- rbind( 
		data.frame(site="DE-Tha", lat=51, long=11, timeOffset=-1	)
		,data.frame(site="IT-MBo", lat=45.0, long=1, timeOffset=0	)
)
tmp <- read.csv(file.path(path,"CommonAnc.csv"), colClasses=c(Site.ID="character", Latitude="numeric", Longitude="numeric", UTC="numeric"), na.strings="TBD")
latLongSites <- data.frame(site=tmp$Site.ID, lat=tmp$Latitude, long=tmp$Longitude, timeOffset=floor(-tmp$UTC))


## results arrays
# evaluation metrics
metrics  <- c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
              "intercept","EF","SD","CRM","MPE","AC","ACu","ACs")
aggregation   <- c("halfhourly","daily","monthly")
vars          <- c("GPP","Reco")
NT_vs_DT_REddy <- DT_REddy_vs_pvwave <- array(NA,dim=c(length(sites),length(metrics),length(aggregation),length(vars)),
                                              dimnames=list(sites,metrics,aggregation,vars))

check_quality <- TRUE   # plot halfhourly NEE time series as a quality check?

###########################
#---- Reading data

# NEE_orig already ustar filtered!! as in Papale_2006 most conservative threshold, 
# the seasons are computed in a different manner (not e.g. JFM) 
# gapfilling according to Reichstein_2005

## Plot options
transp <- 0.15  # transparency

#s <- grep("BR-Sa1",sites)[1]
for ( s in seq_along(sites)) {
  
  fname        <- flist[s]
  year         <- as.numeric(substr(fname, nchar(fname)-28, nchar(fname)-25))
  fname.PVwave <- paste(sites[s],'.',year,'.','DataSetafterFluxpartGL2010.txt', sep="")
  latLongSite  <- unlist(subset(latLongSites, site==sites[s])[1,2:4])
  
  #+++ Loading data from MR partitioning and data for running the partitioning
  dfall             <- fLoadTXTIntoDataframe(fname, paste0(path,"MR_GL_partitioning/"))
  dfall.Lass.PVwave <- read.table(paste(path,"MR_GL_partitioning/",fname.PVwave,sep=""),skip=2)
  title <- scan(paste(path,"MR_GL_partitioning/",fname.PVwave,sep=""), nlines = 1, sep = "", strip.white=TRUE,
                     what=list(rep('character',17))) 
  names(dfall.Lass.PVwave) <- title[[1]]
  
  #+++ Add time stamp in POSIX time format
  dfall$PotRad <- fCalcPotRadiation(dfall$julday,dfall$Hour,latLongSite["lat"],latLongSite["long"],latLongSite["timeOffset"])
  dfall$day    <- (1 - dfall$night)*100
  dfall_posix  <- fConvertTimeToPosix(dfall, 'YMDH', Year.s = 'Year', Month.s='Month', Day.s = 'Day', Hour.s = 'Hr')
  
  
  #+++ Initalize R5 reference class sEddyProc for processing of eddy data
  #+++ with all variables needed for processing later
  EddyProc.C <- sEddyProc$new(sites[s], dfall_posix, 
                              c('NEE', 'NEE_f', 'NEE_fqc', 'Rg', 'Rg_f', 'Rg_fqc','Tair','Tair_fqc','Tsoil', 
                                'VPD','VPD_f', 'VPD_fqc','Ustar', "night","day","PotRad"))
  # EddyProc.C$sDATA$night
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # START - RUN THE REddyProc DT partitioning 
  df.REddy <- partitionNEEGL(dfall,NEEVar.s="NEE_f",QFNEEVar.s="NEE_fqc",QFNEEValue.n = 0,NEESdVar.s="NEE_fs_unc",
                             TempVar.s="Tair_f",QFTempVar.s="Tair_fqc",QFTempValue.n=0,VPDVar.s="VPD_f",QFVPDVar.s="VPD_fqc",
						                 QFVPDValue.n=0,RadVar.s="Rg",PotRadVar.s="day",Suffix.s=""
								 		,controlGLPart.l=partGLControl(nBootUncertainty=0L, isAssociateParmsToMeanOfValids=FALSE, isLasslopPriorsApplied=TRUE))

  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Binding Data frame PVWave tool (all data frame and GL partitioning)
  df.Pvwave <- cbind(dfall, dfall.Lass.PVwave[,c(6:17)])
  
  ## save data frames resulting from Pvwave and df.REddy
  #save(df.REddy,file=paste0(path,"Results/",sites[s],"_df.REddy.RData"))  # RData
  write.table(df.Pvwave,file=paste0(path,"Results/",sites[s],"_df.Pvwave.txt"),row.names=F,col.names=T)
  write.table(df.REddy,file=paste0(path,"Results/",sites[s],"_df.REddy.txt"),row.names=F,col.names=T)
  
  #+++++++++++++++++++++
  # Evaluation HH values
  
  ### 1) Comparison DT method Pvwave vs. REddyProc
  DT_REddy_vs_pvwave[s,,"halfhourly","GPP"]  <- c(unlist(modeval(df.Pvwave$GPP_HBLR, df.REddy$GPP_DT)))
  DT_REddy_vs_pvwave[s,,"halfhourly","Reco"] <- c(unlist(modeval(df.Pvwave$Reco_HBLR, df.REddy$Reco_DT)))
  
  
  ### 2) nighttime vs. daytime in REddyProc
  ## nighttime REddyProc still missing!!!
#   NT_vs_DT_REddy[s,,"GPP"]  <- c(unlist(modeval(df.REddy$GPP_f, df.REddy$GPP_DT))) 
#   NT_vs_DT_REddy[s,,"Reco"] <- c(unlist(modeval(df.REddy$Reco_f, df.REddy$Reco_DT))) 
  

  #+++++++++++++++++++++++++++++
  ## add a few columns to the data frames for evaluation purposes 
  ## the _agg columns are used for aggregation in aggregate() funciton below
  df.Pvwave$julday_agg <- c(1,df.Pvwave$julday[1:(nrow(df.Pvwave)-1)])
  df.Pvwave$Month_agg  <- c(1,df.Pvwave$Month[1:(nrow(df.Pvwave)-1)])
  df.Pvwave$Year_agg   <- c(df.Pvwave$Year[1],df.Pvwave$Year[1:(nrow(df.Pvwave)-1)])


  # Aggregation daily
  if(nrow(df.REddy) != nrow(df.Pvwave)) stop("REddy vs Pvwave: row numbers do not match!")
  df.Pvwave.dd <- aggregate(df.Pvwave,by=list(df.Pvwave$julday_agg),mean,na.rm=T)
  df.REddy.dd  <- aggregate(df.REddy,by=list(df.Pvwave$julday_agg),mean,na.rm=T)

  DT_REddy_vs_pvwave[s,,"daily","GPP"]  <- c(unlist(modeval(df.Pvwave.dd$GPP_HBLR, df.REddy.dd$GPP_DT)))
  DT_REddy_vs_pvwave[s,,"daily","Reco"] <- c(unlist(modeval(df.Pvwave.dd$Reco_HBLR, df.REddy.dd$Reco_DT)))


  #+++++++++++++++++++++++++++++
  # Aggregation Monthly per site
  df.Pvwave.mm <- aggregate(df.Pvwave,by=list(df.Pvwave$Month_agg),mean,na.rm=T)
  df.REddy.mm  <- aggregate(df.REddy,by=list(df.Pvwave$Month_agg),mean,na.rm=T)

  df.Pvwave.mm <- cbind(sites[s],df.Pvwave.mm[,-1]); colnames(df.Pvwave.mm)[1] <- "Site"
  df.REddy.mm  <- cbind(sites[s],df.REddy.mm[,-1]); colnames(df.REddy.mm)[1] <- "Site"

  DT_REddy_vs_pvwave[s,,"monthly","GPP"]  <- c(unlist(modeval(df.Pvwave.mm$GPP_HBLR, df.REddy.mm$GPP_DT)))
  DT_REddy_vs_pvwave[s,,"monthly","Reco"] <- c(unlist(modeval(df.Pvwave.mm$Reco_HBLR, df.REddy.mm$Reco_DT)))

  if (s == 1){
    Pvwave.mm.all <- df.Pvwave.mm 
    REddy.mm.all  <- df.REddy.mm
  } else {
    Pvwave.mm.all <- rbind(Pvwave.mm.all,df.Pvwave.mm)
    REddy.mm.all  <- rbind(REddy.mm.all,df.REddy.mm)
  }


  #+++++++++++++++++++++++++++++
  # Aggregation Annual
  df.Pvwave.yy <- aggregate(df.Pvwave,by=list(df.Pvwave$Year_agg),mean,na.rm=T)
  df.REddy.yy  <- aggregate(df.REddy,by=list(df.Pvwave$Year_agg),mean,na.rm=T)

  df.Pvwave.yy <- cbind(sites[s],df.Pvwave.yy[,-1]); colnames(df.Pvwave.yy)[1] <- "Site"
  df.REddy.yy  <- cbind(sites[s],df.REddy.yy[,-1]); colnames(df.REddy.yy)[1] <- "Site"

  if (s == 1){
    Pvwave.yy.all <- df.Pvwave.yy
    REddy.yy.all  <- df.REddy.yy
  } else {
    Pvwave.yy.all <- rbind(Pvwave.yy.all,df.Pvwave.yy)
    REddy.yy.all  <- rbind(REddy.yy.all,df.REddy.yy)
  }


  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ### Plots
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1) data quality check
  if (check_quality){
    par(mfrow=c(2,2),oma=c(2,2,1,1),mar=c(4,4,1,1)) 
    cex <- 0.9
    plot(df.Pvwave$NEE_f,xlab="timestep",ylab="NEE_f (umol m-2 s-1)",las=1,pch=1,col="black",cex=cex)
    plot(df.Pvwave$Tair_f,xlab="timestep",ylab="Tair_f (degC)",las=1,pch=1,col="black",cex=cex)
    plot(df.Pvwave$Rg_f,xlab="timestep",ylab="Rg_f (W m-2)",las=1,pch=1,col="black",cex=cex)
    plot(df.Pvwave$VPD_f,xlab="timestep",ylab="VPD_f (hPa)",las=1,pch=1,col="black",cex=cex)
    
    dev.copy2pdf(file=paste0(path,"Plots/",sites[s],"_quality_check.pdf"),width=10,height=8,pointsize=11)
  }


  ## 2) Scatterplots
  par(mfrow=c(1,3))
  # halfhourly
  plot(df.Pvwave$GPP_HBLR ~ df.REddy$GPP_DT,xlab="GPP_DT_REddyProc",ylab="GPP_DT_PVwave",las=1,
       main="halfhourly",pch=20,col=alpha("black",transp),cex=1.2)
  legend("topleft",legend=paste0("R^2 = ",round(DT_REddy_vs_pvwave[s,"R2","halfhourly","GPP"],2)),bty="n",cex=0.9)
  curve(1*x,from=-20,to=100,col="red",add=T)
  

  # daily
  plot(df.Pvwave.dd$GPP_HBLR ~ df.REddy.dd$GPP_DT,xlab="GPP_DT_REddyProc",ylab="GPP_DT_PVwave",las=1,
       main="daily",pch=20,col=alpha("black",transp),cex=1.2)
  legend("topleft",legend=paste0("R^2 = ",round(DT_REddy_vs_pvwave[s,"R2","daily","GPP"],2)),bty="n",cex=0.9)
  curve(1*x,from=-20,to=100,col="red",add=T)
  
  
  # monthly
  plot(df.Pvwave.mm$GPP_HBLR ~ df.REddy.mm$GPP_DT,xlab="GPP_DT_REddyProc",ylab="GPP_DT_PVwave",las=1,
       main="monthly",pch=1,col="black")
  legend("topleft",legend=paste0("R^2 = ",round(DT_REddy_vs_pvwave[s,"R2","monthly","GPP"],2)),bty="n",cex=0.9)
  curve(1*x,from=-20,to=100,col="red",add=T)
  
  # write to file
  dev.copy2pdf(file=paste0(path,"Plots/",sites[s],"_GPP_DT_PVwave_REddyProc.pdf"),
               width=8,height=5,pointsize=11)

  
  ## 3) Timeseries of the parameters
  




} # end site loop

## time series of parameter!


#+++++++++++++++++++++++++++++
# Evaluation Monthly All
plot(Pvwave.mm.all$GPP_HBLR ~ REddy.mm.all$GPP_DT)



#++++++++++++++++++++++++++++
# Evaluation Annual
plot(Pvwave.yy.all$GPP_HBLR ~ REddy.yy.all$GPP_DT)



## save as RData:
save(NT_vs_DT_REddy,DT_REddy_vs_pvwave,file=paste0(path,"Results/eval_metrics.RData")) # 1) evaluation metrics
save(Pvwave.mm.all,REddy.mm.all,file=paste0(path,"Results/all_sites_monthly.RData"))   # 2) monthly aggregated results for all sites
save(Pvwave.yy.all,REddy.yy.all,file=paste0(path,"Results/all_sites_annual.RData"))    # 3) annual aggregated results for all sites



### analysis for CA-TP3
# par(mfrow=c(1,1))
# plot(df.REddy.dd$GPP_DT/df.Pvwave.dd$GPP_HBLR ~ c(1:365),ylim=c(0,10),xlab="DOY")
# abline(h=1,col="blue")
#plot(df.Pvwave$Reco_HBLR)
plot(df.REddy$Reco_DT)

