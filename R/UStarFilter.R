controlUStarEst <- function(
		plateau = 5
){
  list(  
		plateau = plateau		  
  )			
}

controlUStarSubsetting <- function(
){
	list(
	)
	
}

estUStarThresholdSingle <- function(
	### estimate the UStar threshold for single subset		
	NEE.v				##<< vector with value of Net Ecosystem exchange
	,uStar.v 			##<< vector with u* (related to friction velocity (m2/s)
	,ctrlUStarEst.l = controlUStarEst()
){
	##references<< inspired by Papale 2006
	
}

estUStarThreshold <- function(
		### estimate the UStar threshold for by aggregating estimates for singe seasonal and temperature classes
		ds						    ##<< data.frame with columns
		,uStarColName = "uStar"		##<< ustar
		,NEEColName = "NEE"
		,tempColName = "airTemp"
		,seasonFactor.v = ds[,"season"]	##<< TODO calculate seasons by default to month
		,ctrlUStarEst.l = controlUStarEst()
){
	##references<< inspired by Papale 2006
	
	dsi <- subset(ds, seasonFactor == 1)
	estUStarThresholdSingle( dsi[,NEEColname], dsi[,uStarColName], ctrlUStarEst.l = ctrlUStarEst.l)
}

estUStarThresholdDistribution <- function(
		### Estimating the UStarTrhesholdDistribution by bootstrapping over data
		ds						    ##<< data.frame with columns
		,uStarColName = "uStar"		##<< ustar
		,NEEColName = "NEE"
		,tempColName = "airTemp"
		,seasonFactor.v = ds[,"season"]	##<< TODO calculate seasons by default to month
		,ctrlUStarEst.l = controlUStarEst()
		,nSample = 100
){
		uStar.l <- boot( ds, estUStarThreshold, nSample)
}



