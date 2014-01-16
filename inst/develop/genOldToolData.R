#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Developers' R script +++
#+++ Generate input data for the old webtool (based on pv-wave) from BGI Fluxnet nc files
#+++ to be used in comparison compOldNewTool.R
#+++ (Main formatting differences old to new tool: single years only, (no year column), 'Day' -> 'DoY')
#+++ --> run from package parent directory
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM

# Source settings for R environment and standard functions
# (Cleans up workspace and set directory paths)
source('inst/develop/setREnvir.R') 
require('REddyProc')

DirFluxnet.s <- paste('~/Data/Fluxnet/level5_new_nc') #!!! Quick fix to work from home, comment out later...

#List of site and years to compare
CompList.V.s <- c('AT-Neu.2004','BR-Ma2.2005','BR-Sa1.2003','CA-Let.2000','CA-NS7.2004','CA-TP3.2005',
                   'CH-Oe1.2004','CH-Oe2.2004','CN-HaM.2003','DE-Hai.2002','DE-Tha.1998','DK-Sor.2006',
                   'ES-ES1.2000','ES-VDA.2005','FI-Hyy.1998','FI-Kaa.2001','FR-Fon.2006','FR-Gri.2006',
                   'FR-Hes.1998','FR-Lq1.2006','FR-Lq2.2006','FR-Pue.2003','IE-Dri.2004','IL-Yat.2005',
                   'IT-Amp.2004','IT-MBo.2005','IT-Pia.2004','IT-SRo.2001','JP-Tef.2005','PT-Esp.2004',
                   'RU-Cok.2004','SE-Deg.2003','SE-Nor.1997','US-Ha1.2000','US-MMS.2004','US-SO2.2006',
                   'US-Ton.2004','VU-Coc.2002','ZA-Kru.2002')
CompName.V.s <- fStripFileExtension(CompList.V.s)

# List of files in FLUXNET directory
FluxFile.V.s <- fInitFilesDir(DirFluxnet.s, 'hourly.nc')
FluxName.V.s <- fStripFileExtension(FluxFile.V.s)

for (CompSite.i in 11) {
#for (CompSite.i in 1:length(CompName.V.s)) {
  #CompSite.i=11
  FluxSite.i <- which(FluxName.V.s == CompName.V.s[CompSite.i])
  Year.i <- as.numeric(sub('.*[.]','',CompList.V.s[CompSite.i]))
  message(paste('Handling site file \"', FluxFile.V.s[FluxSite.i],'\" for the year ', Year.i, sep=''))
  
  #Load data from NetCDF
  # Variables needed for running pv-wave tool
  VarPV.V.s <- c('NEE', 'LE', 'H', 'Rg', 'Tair', 'Tsoil_f', 'rH', 'VPD', 'ustar', 'julday') #Attention: Rh and rH for different version of BGI processing
  VarPV.V.s <- c('NEE', 'LE', 'H', 'Rg', 'Tair', 'Tsoil_f', 'Rh', 'VPD', 'ustar', 'julday') #!!!Old Rh --> delete
  # Variables needed to also compare to Fluxnet (FX) version
  VarFX.V.s <- c('Tair_f', 'Tair_fqcOK', 'NEE_f', 'NEE_fqc', 'Rg_pot', 'Reco', 'GPP_f')
  #VarFX.V.s <- NULL
  VarFX_new.V.s <- c('FX_Tair_f', 'FX_Tair_fqcOK', 'FX_NEE_f', 'FX_NEE_fqc', 'FX_Rg_pot', 'FX_Reco', 'FX_GPP_f')
  EddyNCData.F <- NULL #Reset
  EddyNCData.F <- fLoadFluxNCIntoDataframe(c(VarPV.V.s,VarFX.V.s), FluxFile.V.s[FluxSite.i], DirFluxnet.s) #takes longer...
  
  # Set to single year
  SiteData.F <- EddyNCData.F
  DTS.n <- 48 #half-hourly data
  TimeShift.n <- (0.5 * 24/DTS.n * 60 * 60)  #half-period time offset in seconds to get correct years (usually internal...)
  SiteData.F <-SiteData.F[as.numeric(format(SiteData.F$DateTime-TimeShift.n,"%Y"))==Year.i,]
  
  # Format necessary column for input file
  # Rename Fluxnet columns
  names(SiteData.F)[names(SiteData.F) %in% VarFX.V.s] <- VarFX_new.V.s
  # Rename Tsoil_f to pretend it is unfilled (no fqc available)
  names(SiteData.F)[names(SiteData.F)=='Tsoil_f'] <- 'Tsoil'
  # Set PV-Wave time stamp at front
  SiteData.F <- cbind(qcNEE=ifelse(is.na(SiteData.F$NEE), 2, 1), SiteData.F) # Quality flag according to PV-Webtool
  SiteData.F <- cbind(Hour=as.numeric(format(SiteData.F$DateTime, '%H')) + as.numeric(format(SiteData.F$DateTime, '%M'))/60, SiteData.F)
  SiteData.F <- cbind(Day=SiteData.F$julday, SiteData.F) #With funny uncorrect formatting...
  #Otherwise it would be: cbind(Day=as.numeric(format(SiteData.F$DateTime, '%j')), SiteData.F) #Attention: Called day but is actually DoY
  # Remove DateTime stamp and Fluxnet time columns
  SiteData.F <- SiteData.F[,!names(SiteData.F) %in% c('DateTime','year','month','day','hour','julday')]
  
  #PV-Webtool: Variable naming NOT case sensitive oder can be slightly changed (not of time variables)
  
  FileName.s <- paste(CompList.V.s[CompSite.i], '.txt', sep='')
  fWriteDataframeToFile(SiteData.F, FileName.s,'out')
}
