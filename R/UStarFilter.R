controlUStarEst <- function(
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
		ds=NULL					    ##<< data.frame with columns
		,uStar.v = ds[,"uStar"]		##<< ustar
		,NEE.v = ds[,"NEE"]
		,temp.v = ds[,"airTemp"]
		,seasonFactor.v = ds[,"season"]	##<< TODO calculate seasons by default to month
		,ctrlUStarEst.l = controlUStarEst()
){
	##references<< inspired by Papale 2006
	
	
}

estUStarThresholdDistribution <- function(
		### Estimating the UStarTrhesholdDistribution by bootstrapping over data
){
	
}



