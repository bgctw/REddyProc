#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Developers' R script to test flux paritioning as stand-alone version +++
#+++ --> run from parent directory
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM

# Source settings for R environment and standard functions
# (Cleans up workspace and set directory paths)
# source('inst/develop/setREnvir.R') 

# Tests for flux partitioning
source('inst/develop/EddyFluxPartitioning.R')


# Source file and data handling scripts
source("R/DataFunctions.R")
source("R/FileHandling.R")
# Source geo functions
source("R/GeoFunctions.R")

# Load output data from PV-Wave webtool: partitioning with ustar filter and temperature Tair
TestData.F <- fLoadTXTIntoDataframe('Example_DETha98_PVWave_DataSetafterFluxpart.txt', 'inst/examples') #takes a minute or two
# Further files to provide (this script and data loaded below):
'testFluxPart.R'
'Example_DE-Tha.1996.1998.hourly.nc'

# Convert data frame to comply with REddyProc format
PVData.F <- fConvertTimeToPosix(TestData.F, 'YMDHM', Year.s = 'Year', Month.s= 'Month', Day.s = 'Day', Hour.s = 'Hour', Min.s = 'Minute')
PVData.F$sDateTime <- PVData.F$DateTime

sDATA <- PVData.F
sTEMP <- data.frame(sDataTime=PVData.F$sDateTime)
sINFO <- NULL
sINFO$DIMS <- nrow(PVData.F)
sINFO$DTS <- 48 #half-hourly

# Automated processing
Lat_deg.n <- 51.0; Long_deg.n <- 13.6; TimeZone_h.n <- 1.0;
sMRFluxPartition(Lat_deg.n=Lat_deg.n, Long_deg.n=Long_deg.n, TimeZone_h.n=TimeZone_h.n)

# Test the individual steps of sMRFluxPartition() to compare to PV-Wave webtool output
RadVar.s <- 'Rg'; Lat_deg.n <- 51.0; Long_deg.n <- 13.6; TimeZone_h.n <- 1.0; CallFunction.s='test'
FluxVar.s <- 'NEE_f'; QFFluxVar.s <- 'NEE_fqc'; QFFluxValue.n <- 0; TempVar.s <- 'Tair_f'; QFTempVar.s <- 'Tair_fqc'; QFTempValue.n <- 0

# Check if specified columns are numeric and plausible and apply quality flag
fCheckColNames(sDATA, c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sMRFluxPartition')
fCheckColNum(sDATA, c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sMRFluxPartition')
fCheckColPlausibility(sDATA, c(FluxVar.s, QFFluxVar.s, TempVar.s, QFTempVar.s, RadVar.s), 'sMRFluxPartition')
Var.V.n <- fSetQF(sDATA, FluxVar.s, QFFluxVar.s, QFFluxValue.n, 'sMRFluxPartition')

# Calculate potential radiation
#! New code: Local time and equation of time accounted for in potential radiation calculation
DoY.V.n <- as.numeric(format(sDATA$sDateTime, '%j'))
Hour.V.n <- as.numeric(format(sDATA$sDateTime, '%H')) + as.numeric(format(sDATA$sDateTime, '%M'))/60
sTEMP$NEW_PotRad <- fCalcPotRadiation(DoY.V.n, Hour.V.n, Lat_deg.n, Long_deg.n, TimeZone_h.n)

# Filter night time values only
#! Note: Rg <= 10 congruent with MR PV-Wave, in paper Rg <= 20
sTEMP$VAR_night <- ifelse(sDATA[,RadVar.s] < 10 | sTEMP$NEW_PotRad == 0, Var.V.n, NA)
attr(sTEMP$VAR_night, 'varnames') <- paste(attr(Var.V.n, 'varnames'), '_night', sep='')
attr(sTEMP$VAR_night, 'units') <- attr(Var.V.n, 'units')

# Compare night time filter
sum(is.na(sTEMP$VAR_night) & !is.na(sDATA$NEENight)) # no data points
sum(!is.na(sTEMP$VAR_night) & is.na(sDATA$NEENight)) # 474
tt.i <- which(!is.na(sTEMP$VAR_night) & is.na(sDATA$NEENight))
Comp_NEENight.F <- cbind(sDATA[tt.i, c('DateTime', 'Rg', 'NEENight')], sTEMP[tt.i, c('VAR_night', 'NEW_PotRad')])
# --> less data points due to offset of Rg_pot
#! new code includes ~10% more data points

# To test algorithm with PVWave setting
sTEMP$VAR_night_PV <- sDATA$NEENight

# Apply quality flag for temperature
sTEMP$NEW_TempFP <- fSetQF(sDATA, TempVar.s, QFTempVar.s, QFTempValue.n, 'sMRFluxPartition')

# Estimate E_0 and R_ref (results are saved in sTEMP)
sTEMP$NEW_E_0 <- sRegrE0fromShortTerm('VAR_night', 'NEW_TempFP', CallFunction.s='sMRFluxPartition') #163.66
sTEMP$NEW_E_0_MIX <-sRegrE0fromShortTerm('VAR_night_PV', 'NEW_TempFP', CallFunction.s='sMRFluxPartition') #178.9 Even higher with PV-Wave NEEnight

# To test algorithm with PVWave setting
sTEMP$NEW_E_0_PV <- sDATA$E0_2_from_Tair #133.21
sTEMP$NEW_R_ref_PV <- PVData.F$Rrefopt_OrdE0_2_from
sTEMP$NEW_Reco_PV <- sDATA$Reco
sTEMP$NEW_GPP_f_PV <- sDATA$GPP_f

# Reanalyse R_ref with E_0 fixed
# New R code
sTEMP$NEW_R_ref <- sRegrRref('VAR_night', 'NEW_TempFP', 'NEW_E_0', CallFunction.s='sMRFluxPartition')
# PVWave
sTEMP$NEW_R_ref_MIX <- sRegrRref('VAR_night_PV', 'NEW_TempFP', 'NEW_E_0_PV', WinDays.i=3, CallFunction.s='sMRFluxPartition')

# Plot...
plot(sTEMP$NEW_R_ref_PV, col='gray', pch=20, cex=0.3)
points(sTEMP$NEW_R_ref, col='red', pch=20, cex=0.3)
points(sTEMP$NEW_R_ref_MIX, col='blue', pch=20, cex=0.3)
legend("topright", legend=c('PV-Wave','New R', 'Mix'), pch=20, col=c('gray','red','blue'))

# Calculate the ecosystem respiration R_eco
sTEMP$NEW_Reco <- fLloydTaylor(sTEMP$NEW_R_ref, sTEMP$NEW_E_0, suppressMessages(fConvertCtoK(sDATA[,TempVar.s])), T_ref.n=273.15+15)
sTEMP$NEW_Reco_MIX <- fLloydTaylor(sTEMP$NEW_R_ref_PV, sTEMP$NEW_E_0_PV, suppressMessages(fConvertCtoK(sDATA[,TempVar.s])), T_ref.n=273.15+15)
attr(sTEMP$NEW_Reco, 'varnames') <- 'R_eco'
attr(sTEMP$NEW_Reco, 'units') <- attr(Var.V.n, 'units')

#Plot...
plot(sTEMP$NEW_Reco ~ sTEMP$NEW_Reco_PV, col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3)
plot(sTEMP$NEW_Reco_PV-sTEMP$NEW_Reco_PV, col='gray', pch=20, cex=0.3, ylim=c(-3,3))
points(sTEMP$NEW_Reco-sTEMP$NEW_Reco_PV, col='red', pch=20, cex=0.3)
points(sTEMP$NEW_Reco_MIX-sTEMP$NEW_Reco_PV, col='blue', pch=20, cex=0.3) #??? !!! systematic overestimate by fLloydTaylor because reference temperature was at 10 degC
                                                                          #??? Lloyd Taylor with 10 degrees default or without default ???
legend("topright", legend=c('PV-Wave','New R', 'Mix'), pch=20, col=c('gray','red','blue'))

# Calculate the gross primary production GPP_f
sTEMP$NEW_GPP_f <- -sDATA[,FluxVar.s] + sTEMP$NEW_Reco
sTEMP$NEW_GPP_fqc <- sDATA[,QFFluxVar.s]
attr(sTEMP$NEW_GPP_f, 'varnames') <- 'GPP_f'
attr(sTEMP$NEW_GPP_f, 'units') <- attr(Var.V.n, 'units')
sTEMP$NEW_GPP_f_MIX <- -sDATA[,FluxVar.s] + sTEMP$NEW_Reco_MIX

#Plot...
plot(sTEMP$NEW_GPP_f ~ sTEMP$NEW_GPP_f_PV, col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3)
plot(sTEMP$NEW_GPP_f_PV-sTEMP$NEW_GPP_f_PV, col='gray', pch=20, cex=0.3, ylim=c(-3,3))
points(sTEMP$NEW_GPP_f-sTEMP$NEW_GPP_f_PV, col='red', pch=20, cex=0.3)
points(sTEMP$NEW_GPP_f_MIX-sTEMP$NEW_GPP_f_PV, col='blue', pch=20, cex=0.3)
legend("topright", legend=c('PV-Wave','New R', 'Mix'), pch=20, col=c('gray','red','blue'))

# Rename new columns generated during flux partitioning
colnames(sTEMP) <- gsub('VAR', 'NEE', colnames(sTEMP))
colnames(sTEMP) <- gsub('NEW_', '', colnames(sTEMP))

NewCode.F <- sTEMP

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
# Plots ...
plot(EddyNCData.F$Reco[35089:52608] ~ PVData.F$Reco, col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3)
plot(NewCode.F$Reco ~ NewCode.F$Reco_PV, col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3)
plot(NewCode.F$Reco ~ EddyNCData.F$Reco[35089:52608], col=rgb(0.4,0.4,0.4,alpha=0.2), pch=20, cex=0.3)

plot(NewCode.F$Reco_PV-NewCode.F$Reco_PV, col='gray', pch=20, cex=0.3, ylim=c(-3,3))
points(EddyNCData.F$Reco[35089:52608]-NewCode.F$Reco_PV, col='black', pch=20, cex=0.3)
points(NewCode.F$Reco-NewCode.F$Reco_PV, col='red', pch=20, cex=0.3)
legend("topright", legend=c('Webtool PV-Wave','BGI-Fluxnet PV-Wave','New R'), pch=20, col=c('gray','black','red'))
# --> Larger differences of BGI fluxnet to webtool !
# deviations look similar, did you change anything specific between the two?
