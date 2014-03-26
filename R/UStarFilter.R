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

estUStarThresholdYear <- function(
		### estimate the UStar threshold for by aggregating estimates for singe seasonal and temperature classes
		dsYear					    ##<< data.frame with columns
		,uStarColName = "Ustar"		##<< ustar
		,NEEColName = "NEE"
		,tempColName = "Tair"
		,seasonColName = "season" 
		,ctrlUStarEst.l = controlUStarEst()
){
	##references<< inspired by Papale 2006
	
	##value<< a numeric scalar: the estimate of uStar 
	c(0.2)
}



estUStarThreshold <- function(
		### apply estUStarThresold for each year in ds
		ds						    ##<< data.frame with columns
		,...						##<< further arguments to \code{\link{estUStarThresholdYear}}
		,seasonFactor.v = (as.POSIXlt(ds$DateTime)$mon-1) %/% 3	##<< factor of seasons so split dsYear
		,yearFactor.v = as.POSIXlt(ds$DateTime)$year+1900	##<< factor vector (nrow(dsYear) of seasons so split dsYear 
){
	##references<< inspired by Papale 2006
	ds$season <- seasonFactor.v
	if( !length( yearFactor.v) ){
		c( '0' = estUStarThresholdYear(ds,...) )
	}else{
		ds$yearFac <- yearFactor.v
		daply( ds, .(yearFactor.v), estUStarThresholdYear, ... )
	}
	
	##value<< a vector with uStar Threshold estiamtes. Names correspond to the year of the estimate 
}
attr(estUStarThreshold ,"ex") <- function(){
	Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
	EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
	dss <- subset(EddyData.F, DoY >= 150 & DoY <= 250)
	dss2 <- dss; dss2$Year <- 1999
	EddyDataWithPosix.F <- ds <- fConvertTimeToPosix(rbind(dss,dss2), 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
	(res <- estUStarThreshold(ds))
}


estUStarThresholdDistribution <- function(
		### Estimating the UStarTrhesholdDistribution by bootstrapping over data
		ds					    ##<< data.frame with columns
		,...					##<< further arguments to \code{\link{estUStarThreshold}}
		,nSample = 100
        ,probs = c(0.05,0.5,0.95)
){
		uStar.l <- boot( ds, estUStarThreshold, nSample)
        
        ##value<< a matrix with with first column denoting the year and other columns correponsing to the quantiles of uStar estimate for given probabilities \code{probs}
		years = c(2001,2002)
		#years = c(2001)
		ustarQuantiles = matrix( probs, byrow=TRUE, ncol=length(probs), nrow=length(years), dimnames=list(NULL,probs))
        res <- 
				cbind( 	year=years, ustarQuantiles)             
}



