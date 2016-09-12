#Script for testing the new partitioning GL for REddyProc
library(sirad)
library(REddyProc)

path     <- "M:/work_3/REddyProcRelease/Eval_GL_Partitioning/MR_GL_partitioning/"
path.out <- "M:/people/jknauer/REddyProc_Tests/"

flist  <- list.files(path, pattern="*DataSetafterFluxpart.txt")
sites  <- substr(flist,1,6)

###########################
#---- Reading data

# NEE_orig already ustar filtered!! as in Papale_2006 most conservative threshold, 
# the seasons are computed in a different manner (not e.g. JFM) 
# gapfilling according to Reichstein_2005

## results array
metrics  <- c("N","pearson","MBE","RMBE","MAE","RMAE","RMSE","RRMSE","R2","slope",
              "intercept","EF","SD","CRM","MPE","AC","ACu","ACs")
vars     <- c("GPP","Reco")
NT_vs_DT <- REddy_vs_pvwave <- array(NA,dim=c(length(sites),length(metrics),length(vars)),
                                     dimnames=list(sites,metrics,vars))

## Plot options
par(mfrow=c(2,3),oma=c(1,1,1,1))

counter <- 0
for (s in seq_along(sites)) {
  cat("starting site nr. ",s, "(",sites[s],")",fill=T)
  
  fname        <- flist[s]    
  year         <- as.numeric(substr(fname, nchar(fname)-28, nchar(fname)-25))
  fname.PVwave <- paste(sites[s],'.',year,'.','DataSetafterFluxpartGL2010.txt', sep="")
  
  #+++ Loading data from MR partitioning and data for running the partitioning
  
  dfall <- fLoadTXTIntoDataframe(fname, path)
  dfall.Lass.PVwave <- read.table(paste(path,fname.PVwave,sep=""),skip=2)

  
  title <- scan(paste(path,fname.PVwave,sep=""), nlines = 1, sep = "", strip.white=TRUE,
                     what=list(rep('character',17))) 
  
  names(dfall.Lass.PVwave) <- title[[1]]
  
  #+++ Add time stamp in POSIX time format
  #dfall$PotRad <- fCalcPotRadiation(dfall$julday,dfall$Hour,55,-60.21,-4)
  dfall$day    <- 1 - dfall$night
  dfall_posix  <- fConvertTimeToPosix(dfall, 'YMDH', Year.s = 'Year', Month.s='Month', Day.s = 'Day', Hour.s = 'Hr')
  
  #+++ Initalize R5 reference class sEddyProc for processing of eddy data
  #+++ with all variables needed for processing later
  
  
  ### 1- night (inverse of the night) 

  
  EddyProc.C <- sEddyProc$new(sites[s], dfall_posix, 
                              c('NEE', 'NEE_f', 'NEE_fqc', 'Rg', 'Rg_f', 'Rg_fqc','Tair','Tair_fqc','Tsoil', 
                                'VPD','VPD_f', 'VPD_fqc','Ustar', "night","day"))
  
  # EddyProc.C$sDATA$night
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # START - RUN THE GF LASSLOP -- THOMAS ADD HERE THE CODE FOR THE PARTITIONING 
  
  df.REddy <- partitionNEEGL(dfall,NEEVar.s="NEE_f",QFNEEVar.s="NEE_fqc",QFNEEValue.n = 0,NEESdVar.s="NEE_fs_unc",
                             TempVar.s="Tair_f",QFTempVar.s="Tair_fqc",QFTempValue.n=0,VPDVar.s="VPD_f",QFVPDVar.s="VPD_fqc",
                             QFVPDValue.n=0,RadVar.s="Rg",PotRadVar.s="day",Suffix.s="")
  
  
  
  #partGLControl()
  
  
  
  # END - TO BE REMOVED - 
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # FROM HERE ON TO BE DEVELOP WHEN WE HAVE THE FIRST RESULTS OF THE PARTITIONING
  # Binding Data frame PVWave tool (all data frame and GL partitioning)
  
  df.Pvwave <- cbind(dfall, dfall.Lass.PVwave[,c(6:17)])
  
  
  ## the next lines compare Lasslop with Reichstein partitioning. 
  ## Include Lasslop (old/pvwave) vs. Lasslop new (REddyProc)??
  
  
  #+++++++++++++++++++++
  # Evaluation HH values

  ### 1) Comparison DT method Pvwave vs. REddyProc
  REddy_vs_pvwave[s,,"GPP"]  <- c(unlist(modeval(df.Pvwave$GPP_HBLR, df.REddy$GPP_DT)))
  REddy_vs_pvwave[s,,"Reco"] <- c(unlist(modeval(df.Pvwave$Reco_HBLR, df.REddy$Reco_DT)))
  

  ### 2) nighttime vs. daytime in REddyProc
  ## nighttime REddyProc still missing!!!
  NT_vs_DT[s,,"GPP"]  <- c(unlist(modeval(df.REddy$GPP_f, df.REddy$GPP_DT))) 
  NT_vs_DT[s,,"Reco"] <- c(unlist(modeval(df.REddy$Reco_f, df.REddy$Reco_DT))) 
  
  
  
  #+++++++++++++++++
  #Scatterplot
  
  plot(df.Pvwave$GPP_HBLR ~ df.REddy$GPP_DT,xlab="GPP_DT_REddyProc",ylab="GPP_DT_PVwave",las=1,
       main=sites[s])
  legend("topleft",legend=paste0("R^2 = ",REddy_vs_pvwave[s,"R2","GPP"]),bty="n",cex=0.9)
  curve(1*x,from=-20,to=100,col="blue",add=T)
  
  if (s %% 6 == 0){
    counter <- counter + 1
    dev.copy2pdf(file=paste0(path.out,"Plots/GPP_DT_REddyProc_PVwave_",counter,".pdf"),
                 width=10,height=8,pointsize=11)
  }
  
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


save(NT_vs_DT,REddy_vs_pvwave,file=paste0(path.out,"Results.RData"))


