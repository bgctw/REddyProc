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
        ,probs = c(0.01,0.05,0.5,0.95,0.99)
){
		uStar.l <- boot( ds, estUStarThreshold, nSample)
        
        ##value<< a list with entries
        list(
                ustar = 3                           ##<< the estimate based on the original data              
                ,ustarQuantiles = probs*2           ##<< the quantiles of ustar for given probabilities \code{probs}
                ,probs = probs                      ##<< argument \code{probs}
        )
}

getUStarQuantile <- function(
        ds
        ,UstarVar.s='Ustar'    ##<< Friction velocity ustar (ms-1)
){
    NEE.V.n <- ds[,FluxVar.s]
    Ustar.V.n <- ds[,UstarVar.s]
    time.V <- ds[,timeVar.s]
    Year.V.h <- as.numeric(format(time, '%Y'))
    Month.V.h <- as.numeric(format(time, '%m'))
    
    
}



