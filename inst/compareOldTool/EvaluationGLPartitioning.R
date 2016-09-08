#Script for testing the new partitioning GL for REddyProc
#library(sirad)
#library(REddyProc)

path <- "M:/work_3/REddyProcRelease/Eval_GL_Partitioning/MR_GL_partitioning/"

flist <- list.files(path, pattern="*DataSetafterFluxpart.txt")
n     <- length(flist)

latLongSites <- rbind( 
		data.frame(site="DE-Tha", lat=51, long=11, timeOffset=-1	)
		,data.frame(site="IT-MBo", lat=45.0, long=1, timeOffset=0	)
)
tmp <- read.csv(file.path(path,"../CommonAnc.csv"), colClasses=c(Site.ID="character", Latitude="numeric", Longitude="numeric", UTC="numeric"), na.strings="TBD")
latLongSites <- data.frame(site=tmp$Site.ID, lat=tmp$Latitude, long=tmp$Longitude, timeOffset=floor(-tmp$UTC))

###########################
#---- Reading data

# NEE_orig already ustar filtered!! as in Papale_2006 most conservative threshold, 
# the seasons are computed in a different manner (not e.g. JFM) 
# gapfilling according to Reichstein_2005

#fname <- flist[24]
for ( fname in flist ) {
  sitecode <- substr(fname, nchar(fname)-35, nchar(fname)-30)
  year     <- as.numeric(substr(fname, nchar(fname)-28, nchar(fname)-25))
  fname.PVwave <- paste(sitecode,'.',year,'.','DataSetafterFluxpartGL2010.txt', sep="")
  latLongSite <- unlist(subset(latLongSites, site==sitecode)[1,2:4])
  #+++ Loading data from MR partitioning and data for running the partitioning
  dfall <- fLoadTXTIntoDataframe(fname, path)
  dfall.Lass.PVwave <- read.table(paste(path,fname.PVwave,sep=""),skip=2)
  title <- scan(paste(path,fname.PVwave,sep=""), nlines = 1, sep = "", strip.white=TRUE,
                     what=list(rep('character',17))) 
  names(dfall.Lass.PVwave) <- title[[1]]
  #+++ Add time stamp in POSIX time format
  dfall$PotRad <- fCalcPotRadiation(dfall$julday,dfall$Hour,latLongSite["lat"],latLongSite["long"],latLongSite["timeOffset"])
  dfall$day <- 1 - dfall$night
  dfall_posix <- fConvertTimeToPosix(dfall, 'YMDH', Year.s = 'Year', Month.s='Month', Day.s = 'Day', Hour.s = 'Hr')
  #+++ Initalize R5 reference class sEddyProc for processing of eddy data
  #+++ with all variables needed for processing later
  ### 1- night (inverse of the night) 
  EddyProc.C <- sEddyProc$new(sitecode, dfall_posix, 
                              c('NEE', 'NEE_f', 'NEE_fqc', 'Rg', 'Rg_f', 'Rg_fqc','Tair','Tair_fqc','Tsoil', 
                                'VPD','VPD_f', 'VPD_fqc','Ustar', "night","day","PotRad"))
  # EddyProc.C$sDATA$night
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # START - RUN THE GF LASSLOP -- THOMAS ADD HERE THE CODE FOR THE PARTITIONING 
  test <- partitionNEEGL(dfall,NEEVar.s="NEE_f",QFNEEVar.s="NEE_fqc",QFNEEValue.n = 0,NEESdVar.s="NEE_fs_unc",
                         TempVar.s="Tair_f",QFTempVar.s="Tair_fqc",QFTempValue.n=0,VPDVar.s="VPD_f",QFVPDVar.s="VPD_fqc",
						 QFVPDValue.n=0,RadVar.s="Rg",PotRadVar.s="PotRad",Suffix.s="")
  #partGLControl()
  
  
  
  # END - TO BE REMOVED - 
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # FROM HERE ON TO BE DEVELOP WHEN WE HAVE THE FIRST RESULTS OF THE PARTITIONING
  # Binding Data frame PVWave tool (all data frame and GL partitioning)
  
  df.Eval.Pvwave<-cbind(dfall, dfall.Lass.PVwave[,c(6:17)])
  
  #+++++++++++++++++++++
  # Evaluation HH values
  
  if (i==1){ 
    site_id_array<-code_ancillary
    GPP_R_vs_pvwave.df<-c(unlist(modeval(df.Eval.Pvwave$GPP_f, df.Eval.Pvwave$GPP_HBLR)))
    Reco_R_vs_pvwave.df<-c(unlist(modeval(df.Eval.Pvwave$Reco, df.Eval.Pvwave$Reco_HBLR)))  
  }
  
  if (i>1){  
    print(site_id_array)
    site_id_array<-c(site_id_array,code_ancillary)
    print(paste("!!!!!!!!  ",site_id_array))
    GPP_R_vs_pvwave.df<-rbind(GPP_R_vs_pvwave.df, c(unlist(modeval(df.Eval.Pvwave$GPP_f, df.Eval.Pvwave$GPP_HBLR))))
    Reco_R_vs_pvwave.df<-rbind(Reco_R_vs_pvwave.df, c(unlist(modeval(df.Eval.Pvwave$Reco, df.Eval.Pvwave$Reco_HBLR))))
    #R_out_vs_fluxpart.df<-rbind(R_out_vs_fluxpart.df,unlist(modeval(R_out$Reco_DEF,fluxpart$Reco)))
    #pvwave_vs_fluxpart.df<-rbind(pvwave_vs_fluxpart.df,unlist(modeval(pvwave$Reco,fluxpart$Reco)))
  }
  
  #+++++++++++++++++
  #Scatterplot
  
  scatter1by1(df.Eval.Pvwave$GPP_f, df.Eval.Pvwave$GPP_HBLR)
  
  #+++++++++++++++++++++++++++++
  # Aggregation daily
  
  
  #+++++++++++++++++++++++++++++
  # Aggregation Monthly per site
  
  
  #+++++++++++++++++++++++++++++
  # Aggregation Annual


} # end site loop

#+++++++++++++++++++++++++++++
# Evaluation Monthly All


#++++++++++++++++++++++++++++
# Evaluation Annual





