#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Developers' R script +++
#+++ Compare flux partitioning for old PV-Webtool and new R-Code and PV-Fluxnet
#+++ It is important to perform the new algorithm on the same subset of data after filtering.
#+++ Therefore the new algorithm has to be performed AFTER the PV-Webtool algorithm.
#+++ --> run from package parent directory
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM


# Generate output data from PV-Wave webtool: https://www.bgc-jena.mpg.de/~MDIwork/eddyproc/
# 1. Generate files with data format of PV-Webtool using the script 'genOldToolData.R'
# 2. Load up data in online tool input form with the following settings:
#   Year=..., Time Format 'Julian day, hour', Delimter <TAB>, GF with Uncertainties? <YES>, u*-Filtering before GF? <YES>,
#   Standard FP? <YES>, GL2010 FP? <NO>, Temperature Variable used for FP? <Tair>, Latitude: ..., Email Address: ...
# 3. Save from web and rename output data e.g. to <DE-Tha.1998.DataSetafterFluxpart.txt
# 3. Load data from PV-Webtool output file into this script
# 4. Use this data to run new flux partitioning algorithm. This ensures that the algorithms are based on the SAME subset of (filtered) NEE data.
# 5. Compare results...

# Source settings for R environment and standard functions
# (Cleans up workspace and set directory paths)
source('inst/develop/setREnvir.R')
require('REddyProc')

# Result files with PV-Webtool output data
ResultsFile.V.s <- fInitFilesDir('out', 'DataSetafterFluxpart.txt')
ResultsName.V.s <- fStripFileExtension(ResultsFile.V.s) #!!! Programm to be only AT-Neu...

#Fluxnet files for site info
FluxFile.V.s <- fInitFilesDir(DirFluxnet.s, 'hourly.nc')
FluxName.V.s <- fStripFileExtension(FluxFile.V.s)

#DirFluxnet.s <- paste('~/Data/Fluxnet/level5_new_nc') #!!! Quick fix to work from home, comment out later...

for (Result.i in 1 :length(ResultsName.V.s)) {
  #Result.i <- 33
  message(paste('Handling site \'', ResultsName.V.s[Result.i],'\'', sep=''))
  FluxSite.i <- which(FluxName.V.s == ResultsName.V.s[Result.i])

  # Load PV-Webtool output data
#  if( sum(grepl('PVWebtoolData.F', ls(all=TRUE))) == 0 ) # Load only PVWebtoolData.F, if not yet loaded

    PVWebtoolData.F <- fLoadTXTIntoDataframe(ResultsFile.V.s[Result.i], 'out') #takes a minute or two

  # Convert data frame to comply with REddyProc format
  PVData.F <- fConvertTimeToPosix(PVWebtoolData.F, 'YMDHM', Year.s = 'Year', Month.s= 'Month', Day.s = 'Day', Hour.s = 'Hour', Min.s = 'Minute')

  # Rename complicated names from webtool
  PVData.F$R_ref <- PVData.F$Rrefopt_OrdE0_2_from
  PVData.F$E_0 <- PVData.F$E0_2_from_Tair

  #Load site information from BGC nc files (used to generate old tool input data)
  Info.L <- fLoadFluxNCInfo(FluxFile.V.s[FluxSite.i], DirFluxnet.s, 'RNetCDF')

  # Run new flux partitioning algorithm
  rm(EPSite.C)
  # Automatically uses the NEE_f, NEE_fqc, Tair_f, Tair_fqc, and Rg from the pv-wave output files
  # This ensures that the algorithms are based on the SAME subset of (filtered) NEE data
  EPSite.C <- sEddyProc$new(ResultsName.V.s[Result.i], PVData.F, c('NEE_f', 'NEE_fqc', 'Tair_f', 'Tair_fqc', 'Rg'))
  EPSite.C$setLocationInfo(LatDeg = Info.L$LAT, LongDeg = Info.L$LON, TimeZoneHour = Info.L$TZ)
  EPSite.C$sMRFluxPartition()
  rm(NewFPResults.F)
  NewFPResults.F <- EPSite.C$sExportResults()

  PVData1.F <- PVData.F[,-1:-8]

  CombinedData.F <- cbind(PVData1.F, NewFPResults.F)
  Filename.s <- paste(ResultsName.V.s[Result.i],'.', PVData.F$Year[1], '.txt', sep='')
  fWriteDataframeToFile(CombinedData.F, Filename.s, 'out2')

}

.tmp.f <- function(){
  # Very basic example plot of differences between new algorithm to PV Webtool and to BGI fluxnet version
  # Plot 1998 ...
  plot(PVData.F$FX_Reco ~ PVData.F$Reco, col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3, main='PV Fluxnet vs. PV Webtool')
  plot(NewFPResults.F$Reco ~ PVData.F$Reco, col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3, main='R code vs. PV Webtool')
  # Residuals R_eco
  plot(PVData.F$Reco-PVData.F$Reco, col='red', pch=20, cex=0.3, ylim=c(-3,3))
  points(PVData.F$FX_Reco-PVData.F$Reco, col='violet', pch=20, cex=0.3)
  points(NewFPResults.F$Reco-PVData.F$Reco, col='blue', pch=20, cex=0.3)
  legend("topright", legend=c('PV Webtool','PV Fluxnet','R Code'), pch=20, col=c('red','violet','blue'))
  # --> Larger differences of PV BGI Fluxnet with PV Webtool than R code :-) !
}
