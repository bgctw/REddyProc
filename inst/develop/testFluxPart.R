#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Developers' R script to test flux paritioning as stand-alone version +++
#+++ --> run from parent directory
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM

# Source settings for R environment and standard functions
# (Cleans up workspace and set directory paths)
source('inst/develop/setREnvir.R') 

# Tests for flux partitioning
source('inst/develop/EddyFluxPartitioning.R')


# Source file and data handling scripts
source("R/DataFunctions.R")
source("R/FileHandling.R")
# Source geo functions
source("R/GeoFunctions.R")

# Generate output data from PV-Wave webtool: http://www.bgc-jena.mpg.de/~MDIwork/eddyproc/
# Setting: partitioning with ustar filter and temperature 'Tair'
# Differences to new tool: single years only, (no year column), 'Day' -> 'DoY'
# Load output data
if( sum(grepl('TestData.F', ls(all=TRUE))) == 0 ) # Load only TestData.F, if not yet loaded
  TestData.F <- fLoadTXTIntoDataframe('Example_DETha98_PVWave_DataSetafterFluxpart.txt', 'inst/examples') #takes a minute or two

# Further files to provide (this script and data loaded below):
'testFluxPart.R'
'Example_DE-Tha.1996.1998.hourly.nc'

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 # Initialize pseudo data frames of sEddyProc R5 class

# Convert data frame to comply with REddyProc format
PVData.F <- fConvertTimeToPosix(TestData.F, 'YMDHM', Year.s = 'Year', Month.s= 'Month', Day.s = 'Day', Hour.s = 'Hour', Min.s = 'Minute')
PVData.F$sDateTime <- PVData.F$DateTime

# Rename complicated names from webtool
PVData.F$R_ref <- PVData.F$Rrefopt_OrdE0_2_from
PVData.F$E_0 <- PVData.F$E0_2_from_Tair

sDATA <- PVData.F
sINFO <- NULL
sINFO$DIMS <- nrow(PVData.F)
sINFO$DTS <- 48 #half-hourly
sTEMP <- NULL

#++++++++++++++++++

# Automated processing
sTEMP <- data.frame(sDataTime=PVData.F$sDateTime) #Reset sTEMP

# To compare algorithm to PVWave setting
Lat_deg.n <- 51.0; Long_deg.n <- 13.6; TimeZone_h.n <- 1.0
sMRFluxPartition(Lat_deg.n=Lat_deg.n, Long_deg.n=Long_deg.n, TimeZone_h.n=TimeZone_h.n)

TestWebtool.F <- sTEMP


#+++++++++++++++++++
# For testing sMRFluxPartition step by step
sTEMP <- data.frame(sDataTime=PVData.F$sDateTime) #Reset sTEMP

# To compare algorithm to PVWave setting
message('E_0 value from Webtool: ', sDATA$E0_2_from_Tair[1])  #133.21
sTEMP <- data.frame(sDataTime=PVData.F$sDateTime)
sTEMP$PV_NEENight <- sDATA$NEENight
sTEMP$PV_E_0 <- sDATA$E0_2_from_Tair
sTEMP$PV_R_ref <- PVData.F$Rrefopt_OrdE0_2_from
sTEMP$PV_Reco <- sDATA$Reco
sTEMP$PV_GPP_f <- sDATA$GPP_f

# Test the individual steps of sMRFluxPartition() to compare to PV-Wave webtool output
RadVar.s <- 'Rg'; Lat_deg.n <- 51.0; Long_deg.n <- 13.6; TimeZone_h.n <- 1.0; CallFunction.s='test'
FluxVar.s <- 'NEE_f'; QFFluxVar.s <- 'NEE_fqc'; QFFluxValue.n <- 0; TempVar.s <- 'Tair_f'; QFTempVar.s <- 'Tair_fqc'; QFTempValue.n <- 0
fCheckColNames(sDATA, c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sMRFluxPartition')
fCheckColNum(sDATA, c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sMRFluxPartition')
fCheckColPlausibility(sDATA, c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sMRFluxPartition')
Var.V.n <- fSetQF(sDATA, FluxVar.s, QFFluxVar.s, QFFluxValue.n, 'sMRFluxPartition')

# Calculate potential radiation
DoY.V.n <- as.numeric(format(sDATA$sDateTime, '%j'))
Hour.V.n <- as.numeric(format(sDATA$sDateTime, '%H')) + as.numeric(format(sDATA$sDateTime, '%M'))/60
sTEMP$NEW_PotRad <- fCalcPotRadiation(DoY.V.n, Hour.V.n, Lat_deg.n, Long_deg.n, TimeZone_h.n)
sTEMP$VAR_night <- ifelse(sDATA[,RadVar.s] > 10 | sTEMP$NEW_PotRad > 0, NA,  Var.V.n)
attr(sTEMP$VAR_night, 'varnames') <- paste(attr(Var.V.n, 'varnames'), '_night', sep='')
attr(sTEMP$VAR_night, 'units') <- attr(Var.V.n, 'units')
# Apply quality flag for temperature
sTEMP$NEW_TempFP <- fSetQF(sDATA, TempVar.s, QFTempVar.s, QFTempValue.n, 'sMRFluxPartition')

# Estimate E_0 and R_ref (results are saved in sTEMP)
sTEMP$NEW_E_0 <- sRegrE0fromShortTerm('VAR_night', 'NEW_TempFP', CallFunction.s='sMRFluxPartition')
sTEMP$NEW_E_0_PVNight <-sRegrE0fromShortTerm('PV_NEENight', 'NEW_TempFP', CallFunction.s='sMRFluxPartition')

# Reanalyse R_ref with E_0 fixed with R code results and PV-Wave results
sTEMP$NEW_R_ref <- sRegrRref('VAR_night', 'NEW_TempFP', 'NEW_E_0', CallFunction.s='sMRFluxPartition')
sTEMP$NEW_R_ref_PVNight <- sRegrRref('PV_NEENight', 'NEW_TempFP', 'NEW_E_0', CallFunction.s='sMRFluxPartition')
sTEMP$NEW_R_ref_PVNightE_0 <- sRegrRref('PV_NEENight', 'NEW_TempFP', 'PV_E_0', CallFunction.s='sMRFluxPartition')

# Calculate the ecosystem respiration R_eco
sTEMP$NEW_Reco <- fLloydTaylor(sTEMP$NEW_R_ref, sTEMP$NEW_E_0, fConvertCtoK(sDATA[,TempVar.s]), T_ref.n=273.15+15)
sTEMP$NEW_Reco_PVNight <- fLloydTaylor(sTEMP$NEW_R_ref_PVNight, sTEMP$NEW_E_0, fConvertCtoK(sDATA[,TempVar.s]), T_ref.n=273.15+15)
sTEMP$NEW_Reco_PVNightE_0 <- fLloydTaylor(sTEMP$NEW_R_ref_PVNightE_0, sTEMP$PV_E_0, fConvertCtoK(sDATA[,TempVar.s]), T_ref.n=273.15+15)
attr(sTEMP$NEW_Reco, 'varnames') <- 'R_eco'
attr(sTEMP$NEW_Reco, 'units') <- attr(Var.V.n, 'units')

# Calculate the gross primary production GPP_f
sTEMP$NEW_GPP_f <- -sDATA[,FluxVar.s] + sTEMP$NEW_Reco
sTEMP$NEW_GPP_fqc <- sDATA[,QFFluxVar.s]
attr(sTEMP$NEW_GPP_f, 'varnames') <- 'GPP_f'
attr(sTEMP$NEW_GPP_f, 'units') <- attr(Var.V.n, 'units')

# Rename new columns generated during flux partitioning
colnames(sTEMP) <- gsub('VAR_', 'NEE_', colnames(sTEMP))
colnames(sTEMP) <- gsub('NEW_', '', colnames(sTEMP))

TestSteps.F <- sTEMP

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sTEMP <- TestSteps.F
sTEMP <- TestWebtool.F

# Compare night time filter
tt.i <- (!is.na(sTEMP$NEE_night) & is.na(sDATA$NEENight)) | (is.na(sTEMP$NEE_night) & !is.na(sDATA$NEENight))
cbind(DateTime=sDATA[tt.i, c('DateTime')], PotRad=sTEMP[tt.i, c('PotRad')], d=sDATA[tt.i, c('Rg', 'NEENight')], 
                     VAR_night=sTEMP[tt.i, c('NEE_night')])

if( F ) { # Plot residuals R_ref --> almost same as residuals of R_eco ...
plot(PVData.F$R_ref-PVData.F$R_ref, type='l', col='red', lty=1, lwd=2, ylim=c(-2,2))
points(sTEMP$R_ref-PVData.F$R_ref, type='l', col='blue', lty=1, lwd=1)
points(sTEMP$R_ref_PVNight-PVData.F$R_ref, type='l', col='green', lty=1, lwd=1)
points(sTEMP$R_ref_PVNightE_0-PVData.F$R_ref, type='l', col='green', lty=2, lwd=1)
legend("topright", legend=c(paste('PV-Wave (', round(PVData.F$E_0[1],2), ')', sep=''), paste('R (trimmed, 3 days) (', sTEMP$E_0[1], ')', sep=''), 
                            paste('If avail.: R with PV night (', sTEMP$E_0[1], ')', sep=''), paste('If avail.: R with PV night+E_0 (', round(PVData.F$E_0[1],2), ')', sep='')), 
       pch=20, col=c('red','blue','green','green'), lty=c(1,1,1,2), cex=0.8)
}

#Plot R_ref and residuals Reco ...
MaxY.n <- max(PVData.F$R_ref) * 1.1
MinY.n <- min(sTEMP$Reco-PVData.F$Reco) * 1.1

plot(PVData.F$R_ref, type='l', col='red', lty=1, lwd=2, ylim=c(MinY.n,MaxY.n), main='R_ref and residuals R_eco')
points(sTEMP$R_ref, type='l', col='blue', lty=1, lwd=1)
points(sTEMP$R_ref_PVNight, type='l', col='green', lty=1, lwd=1)
points(sTEMP$R_ref_PVNightE_0, type='l', col='green', lty=2, lwd=1)

points(PVData.F$Reco-PVData.F$Reco, type='l', col='red', lty=1, lwd=1, ylim=c(-2,2))
points(sTEMP$Reco-PVData.F$Reco, type='l', col='blue', lty=1, lwd=1)
points(sTEMP$Reco_PVNight-PVData.F$Reco, type='l', col='green', lty=1, lwd=1)
points(sTEMP$Reco_PVNightE_0-PVData.F$Reco, type='l', col='green', lty=2, lwd=1)
legend("topright", legend=c(paste('PV-Wave (', round(PVData.F$E_0[1],2), ')', sep=''), paste('R (trimmed, 3 days) (', sTEMP$E_0[1], ')', sep=''), 
                            paste('If avail.: R with PV night (', sTEMP$E_0[1], ')', sep=''), paste('If avail.: R with PV night+E_0 (', round(PVData.F$E_0[1],2), ')', sep='')), 
       pch=20, col=c('red','blue','green','green'), lty=c(1,1,1,2), cex=0.8)

#Plot scatterplots Reco and GPP_f ...
plot(sTEMP$Reco ~ PVData.F$Reco, col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3)
plot(sTEMP$GPP_f ~ PVData.F$GPP_f, col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load standard example data from BGI fluxnet files

lVarPart.V.s <- c('NEE', 'Rg', 'Tair_f', 'Tair_fqcOK', 'Tsoil_f', 'NEE_f', 'NEE_fqc', 'Rg_pot', 'Reco', 'GPP_f')
EddyNCData.F <- fLoadFluxNCIntoDataframe(lVarPart.V.s, 'Example_DE-Tha.1996.1998.hourly.nc','inst/examples')
EddyNCData.F$Tair_fqc <- ifelse(EddyNCData.F$Tair_fqcOK == 1, 0, 4) #Pseudo fqc
EddyNCData.F$sDateTime <- EddyNCData.F$DateTime

sDATA <- EddyNCData.F
sTEMP <- data.frame(sDataTime=EddyNCData.F$sDateTime)
sINFO <- NULL
sINFO$DIMS <- nrow(EddyNCData.F)
sINFO$DTS <- 48 #half-hourly

Lat_deg.n <- 51.0; Long_deg.n <- 13.6; TimeZone_h.n <- 1.0;
sMRFluxPartition(Lat_deg.n=Lat_deg.n, Long_deg.n=Long_deg.n, TimeZone_h.n=TimeZone_h.n)

# Compare differences to BGI fluxnet version
# Plot 1998 ...
plot(EddyNCData.F$Reco[35089:52608] ~ PVData.F$Reco, col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3, main='PV Fluxnet vs. PV Webtool')
plot(TestWebtool.F$Reco ~ PVData.F$Reco, col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3, main='R code vs. PV Webtool')
# Residuals R_eco
plot(PVData.F$Reco-PVData.F$Reco, col='red', pch=20, cex=0.3, ylim=c(-3,3))
points(EddyNCData.F$Reco[35089:52608]-PVData.F$Reco, col='violet', pch=20, cex=0.3)
points(TestWebtool.F$Reco-PVData.F$Reco, col='blue', pch=20, cex=0.3)
legend("topright", legend=c('PV Webtool','PV Fluxnet','R Code'), pch=20, col=c('red','violet','blue'))
# --> Larger differences of PV BGI Fluxnet with PV Webtool than R code :-) !



