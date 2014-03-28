# TODO: Is the library(plyr) already loaded?
# needed for function arrange()

controlUstarEst <- function(
  #T Classes not needed here?
  # either?
  #,taClasses=7 # set number of ta classes   
  # or?
  #,ctrlUstarSubsetting.l = controlUstarSubsetting()
  #,taClasses=ctrlUstarSubsetting.l$taClasses  
  #
  #,percentile = 90 #percentile value... double check!
  #,percentile_check = TRUE #enable percentile check\n ... double check!
  ustPlateauFwd = 10 #number of subsequent thresholds to compare to in fwd mode
  ,ustPlateauBack = 6 #number of subsequent thresholds to compare to in back mode  
  ,plateauCrit = 0.95 #significant differences between a u* value and the mean of a "plateau"
  ,swThr = 10  # nighttime data threshold [Wm-2]
  ,corrCheck = 0.5 #threshold value for correlation between Tair and u* data
  ,seasons = 1 # switch for different seasonal modes #TODO: Update?!
  #(seasons or "groupby" may easily be extended to an input vector or matrix)
  ,bt = FALSE #flag for bootstrapping
  ,btTimes = 100 #number of bootstrap samples
  
  #other params?
  ,method.v = fun
  
  # 1.) ,selection parameter for which fwd and back modes? fwd2 as default... 
  # 2.) ,MIN_VALUE_PERIOD <<- 3000 # per whole data set... double check C code
  # 3.) ,MIN_VALUE_SEASON <<- 160 #if #number of data points in one any season are smaller than that, merge to one big season
  #define MIN_VALUE_PERIOD  			3000		/* min values for compute u* threshold */
  #define MIN_VALUE_SEASON				160			/* min for seasons */
  #define TA_CLASS_MIN_SAMPLE				100
  #TODO: what does the following param do?
  #define FIRST_Ustar_MEAN_CHECK  		0.2  
  # 4.) const int percentiles[PERCENTILES_COUNT] = { 5, 10, 25, 50, 75, 90, 95};
  
){
  ctrl <- list(  
    #taClasses=taClasses
    #,UstarClasses=UstarClasses  
    #percentile = percentile
    #percentile_check = percentile_check #enable percentile check\n ... double check!
    ustPlateauFwd = ustPlateauFwd #number of subsequent thresholds to compare to in fwd mode
    ,ustPlateauBack = ustPlateauBack #number of subsequent thresholds to compare to in back mode  
    ,plateauCrit = plateauCrit #significant differences between a u* value and the mean of a "plateau"
    ,swThr = swThr  # nighttime data threshold [Wm-2]
    ,corrCheck = corrCheck #threshold value for correlation between Tair and u* data
    ,seasons = seasons # switch for three different seasonal modes 
    #(seasons or "groupby" may easily extended to an input vector or matrix)
    ,bt = bt #flag for bootstrapping
    ,btTimes = btTimes #number of bootstrap samples
  )
  #display warning message for the following variables that we advise not to be changed
  if (swThr != 10) warning("WARNING: parameter swThr set to non default value!")
  if (corrCheck != 0.5) warning("WARNING: parameter corrCheck set to non default value!")
  ctrl
}
attr(controlUstarEst,"ex") <- function(){
	controlUstarEst()
}

controlUstarSubsetting <- function(
  	taClasses=7 		##<< set number of air temperature classes 
	,UstarClasses=20 	##<< set number of Ustar classes 	
){  
  ctrl <- list(
    taClasses=taClasses
	,UstarClasses= UstarClasses
  )	
  if (taClasses != 7) warning("WARNING: parameter taClasses set to non default value!")	
  if (UstarClasses != 20) warning("WARNING: parameter UstarClasses set to non default value!")
  ctrl
}
attr(controlUstarSubsetting,"ex") <- function(){
	controlUstarSubsetting()
}

binUstar <- function(
	# bin the 
	NEE.v				##<< vector with value of Net Ecosystem exchange
	,Ustar.v 			##<< vector with u* (friction velocity (m2/s)
	,UstarClasses
){
	ds.f <- data.frame(NEE=NEE.v,Ustar=Ustar.v)
	
	#within data frame sort values by Ustar
	ds.f <- arrange(ds.f,ds.f[,2])
	
	N_T <- length(NEE.v) #number of observations(rows) in a T class
	Ust_bin_size <- round(N_T/UstarClasses)
	
	#set up data frame for bin averages (by Ustar)
	Ust_bins.m <- matrix(NA,UstarClasses,2)
	Ust_bins.f <- data.frame(Ust_bins.m)
	names(Ust_bins.f)[1]="Ust_avg"; names(Ust_bins.f)[2]="NEE_avg";
	#names(Ust_bins.f)[3]="N";
	
	#calculate u* bin averages
	for (u in 1:UstarClasses){
		#/om:this part only implemented for checking C code compatibility...
		#ust_class_start = ust_class_end
		#ust_class_end = ust_class_start + Ust_bin_size-1
		#dataUstclass <- dataTclass[ust_class_start:ust_class_end,]
		#/eom    
		if (u==UstarClasses){ 
			# use end index of vector for slightly smaller last bin (due to rounding) 
			dsUstClass.f <- ds.f[((u-1)*Ust_bin_size+1):N_T,]
		}
		else {
			dsUstClass.f <- ds.f[((u-1)*Ust_bin_size+1):((u)*Ust_bin_size),]
		}
		#TODO: merge two following lines to one
		Ust_bins.f$NEE_avg[u] <- mean(dsUstClass.f[,1], na.rm=T) #mean of NEE over Ustar bins
		Ust_bins.f$Ust_avg[u] <- mean(dsUstClass.f[,2], na.rm=T) #mean of Ustar bins
		#Ust_bins.f$N[u] <- sum(!is.na(dataUstclass$nee))
	}
	Ust_bins.f
	
}
attr(binUstar,"ex") <- function(){
	Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
	EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
	dss <- subset(EddyData.F, DoY >= 150 & DoY <= 250)
	(res <- binUstar(dss$NEE, dss$Ustar))
	(resFW1 <- estUstarThresholdSingleFw1Binned(res))
	(resFW2 <- estUstarThresholdSingleFw1Binned(res))
}


estUstarThresholdSingleFw1Binned <- function(
		### estimate the Ustar threshold for single subset, using FW1 algorithm, relying on binned NEE and Ustar
		Ust_bins.f			##<< data.frame with column s NEE_avg and Ust_avg, of NEE by Ustar bins by \code{\link{binUstar}}
		,ctrlUstarEst.l = controlUstarEst()
){
	##references<< inspired by Papale 2006
	
	# algorithm to check when plateau is reached
	flag <- FALSE
	#for every u* bin compare to avg of subsequent UST_PLATEAU, until found
	u <- 1
	#TODO: change to for loop 1:ustClasses and then break
	# in order to avoid infinite loop in case of error
	# optimize with Thomas?
	while (!flag){ #only stop if threshold is found
		if (!flag & (Ust_bins.f$NEE_avg[u] >= (ctrlUstarEst.l$plateauCrit*mean(Ust_bins.f$NEE_avg[(u+1):(u+ctrlUstarEst.l$ustPlateauFwd)],na.rm=T)))){ #na.rm=T to exclude NAs out of bounds..
			#   NEE_i >= .95*avg(i,i+1,...,i+10)  [FW]    
			UstarThSingle <- Ust_bins.f$Ust_avg[u]        
			flag <- TRUE #set flag for threshold found in this mode
		}          
		#case that no threshold could be found by plateau method, use maximum u* in that T_class...
		if (u==(nrow(Ust_bins.f)-1)){ #FW1: -1 ; FW2: 
			UstarThSingle <- Ust_bins.f$Ust_avg[u+1]        
			break;      
		}
		u <- u+1 #increase index by 1
	}  
	return(UstarThSingle)
}




.depr.estUstarThresholdSingleFw1 <- function(
  ### estimate the Ustar threshold for single subset, using FW1 algorithm		
  NEE.v				##<< vector with value of Net Ecosystem exchange
  ,Ustar.v 			##<< vector with u* (friction velocity (m2/s)
  ,ctrlUstarEst.l = controlUstarEst()
){
  ##references<< inspired by Papale 2006
  ds.f <- data.frame(NEE.v,Ustar.v)
  
  #within data frame sort values by Ustar
  ds.f <- arrange(ds.f,ds.f[,2])
  
  N_T <- length(NEE.v) #number of observations(rows) in a T class
  Ust_bin_size <- round(N_T/ctrlUstarEst.l$UstarClasses)
  
  #set up data frame for bin averages (by Ustar)
  Ust_bins.f <- matrix(NA,ctrlUstarEst.l$UstarClasses,2)
  Ust_bins.f <- data.frame(Ust_bins)
  names(Ust_bins.f)[1]="Ust_avg"; names(Ust_bins.f)[2]="NEE_avg";
  #names(Ust_bins.f)[3]="N";
  
  #calculate u* bin averages
  for (u in 1:ctrlUstarEst.l$UstarClasses){
    #/om:this part only implemented for checking C code compatibility...
    #ust_class_start = ust_class_end
    #ust_class_end = ust_class_start + Ust_bin_size-1
    #dataUstclass <- dataTclass[ust_class_start:ust_class_end,]
    #/eom    
    if (u==ctrlUstarEst.l$UstarClasses){ 
      # use end index of vector for slightly smaller last bin (due to rounding) 
      dsUstClass.f <- ds.f[((u-1)*Ust_bin_size+1):N_T,]
    }
    else {
      dsUstClass.f <- ds.f[((u-1)*Ust_bin_size+1):((u)*Ust_bin_size),]
    }
    #TODO: merge two following lines to one
    Ust_bins.f$NEE_avg[u] <- mean(dsUstClass.f[,1], na.rm=T) #mean of NEE over Ustar bins
    Ust_bins.f$Ust_avg[u] <- mean(dsUstClass.f[,2], na.rm=T) #mean of Ustar bins
    #Ust_bins.f$N[u] <- sum(!is.na(dataUstclass$nee))
  }
  
  # algorithm to check when plateau is reached
  flag <- FALSE
  #for every u* bin compare to avg of subsequent UST_PLATEAU, until found
  u <- 1
  #TODO: change to for loop 1:ustClasses and then break
  # in order to avoid infinite loop in case of error
  # optimize with Thomas?
  while (!flag){ #only stop if threshold is found
    if (!flag & (Ust_bins.f$NEE_avg[u] >= (ctrlUstarEst.l$plateauCrit*mean(Ust_bins.f$NEE_avg[(u+1):(u+ctrlUstarEst.l$ustPlateauFwd)],na.rm=T)))){ #na.rm=T to exclude NAs out of bounds..
      #   NEE_i >= .95*avg(i,i+1,...,i+10)  [FW]    
      UstarThSingle <- Ust_bins.f$Ust_avg[u]        
      flag <- TRUE #set flag for threshold found in this mode
    }          
    #case that no threshold could be found by plateau method, use maximum u* in that T_class...
    if (u==(ctrlUstarEst.l$UstarClasses-1)){ #FW1: -1 ; FW2: 
      UstarThSingle <- Ust_bins.f$Ust_avg[u+1]        
      break;      
    }
    u <- u+1 #increase index by 1
  }  
  return(UstarThSingle)
}


# TODO: restructure? may not be the most efficient cause Ustar bin averages have to be computed twice (or more depending on how many methods).
# TODO: wrap rest of Ustar code from fw1 around the while loop... (if necessary, depending on structure)
estUstarThresholdSingleFw2Binned <- function(
  ### estimate the Ustar threshold for single subset, using FW2 algorithm  	
  Ust_bins.f			##<< data.frame with column s NEE_avg and Ust_avg, of NEE by Ustar bins by \code{\link{binUstar}}
  ,ctrlUstarEst.l = controlUstarEst()
){  
  # algorithm to check when plateau is reached
  flag <- FALSE
  #for every u* bin compare to avg of subsequent UST_PLATEAU, until found
  u <- 1
  while (!flag){ #only stop if threshold is found
    if (!flag & (Ust_bins.f$NEE_avg[u] >= (ctrlUstarEst.l$plateauCrit*mean(Ust_bins.f$NEE_avg[(u+1):(u+ctrlUstarEst.l$ustPlateauFwd)],na.rm=T))) 
        & (Ust_bins.f$NEE_avg[u+1] >= (ctrlUstarEst.l$plateauCrit*mean(Ust_bins.f$NEE_avg[(u+1+1):(u+ctrlUstarEst.l$ustPlateauFwd+1)],na.rm=T)))){ 
      UstarThSingle <- Ust_bins.f$Ust_avg[u]        
      flag <- TRUE #set flag for threshold found in this mode

    }    
    #case that no threshold could be found by plateau method, use maximum u* in that T_class...
    if (u==(nrow(Ust_bins.f)-2)){ #FW1: -1 ; FW2: 
      UstarThSingle <- Ust_bins.f$Ust_avg[u+1]        
      break;      
    }
    u <- u+1 #increase index by 1
  }   
  return(UstarThSingle)    
}

estUstarThresholdYear <- function(
  ### estimate the Ustar threshold by aggregating estimates for singe seasonal and temperature classes
  ds						    ##<< data.frame with columns
  ,UstarColName = "Ustar"		##<< Ustar
  ,NEEColName = "NEE"
  ,tempColName = "Tair"
  ,seasonFactor.v = ds[,"season"]	##<< [from Thomas] TODO calculate seasons by default to month
  ,ctrlUstarEst.l = controlUstarEst()
  ,ctrlUstarSub.l = controlUstarSubsetting()
  ,fEstimateUStarBinned = estUstarThresholdSingleFw2Binned
){
  ##references<< Ustar filtering following the idea in Papale 2006	
  # subset for first season
  # TODO: implement seasonal functionality
  #dsi <- subset(ds, seasonFactor.v == 1)
  ds$seasonFactor <- seasonFactor.v
  uStarSeasons <- daply(ds, .(seasonFactor), function(dsi){
    dsiSort <- arrange(dsi, dsi[,tempColName]) #sort values in a season by air temperature
    N <- length(dsi[,NEEColName]) #number of observations (rows) total, probably can get from elsewhere..
    T_bin_size <- round(N/ctrlUstarSub.l$taClasses) #set T_bin size so that every bin has equal #values
    
    #set up vector that contains Ustar values for temperature classes
    UstarTh.v = vector(length=ctrlUstarSub.l$taClasses)
    
    for (k in 1:ctrlUstarSub.l$taClasses){
      #original Dario's C version...
      #ta_class_start = 0;
      #ta_class_end = season_start_index;
      #/* set start & end indexes */
      #  ta_class_start = ta_class_end;
      #ta_class_end = season_start_index + (ta_samples_count*(i+1)-1);
      
      #/om:this part only implemented for checking C code compatibility...
      #ta_class_start = ta_class_end
      #ta_class_end = ta_class_start + T_bin_size-1
      #/eom
      
      #subset into Ta classes
      if (k==ctrlUstarSub.l$taClasses){ # use end index of vector for slightly smaller last bin (due to rounding) 
        dsiSortTclass <- dsiSort[((k-1)*T_bin_size+1):N,]
      }
      else {
        dsiSortTclass <- dsiSort[((k-1)*T_bin_size+1):((k)*T_bin_size),]
      }
      
      #constraint: u* threshold only accepted if T and u* are not or only weakly correlated..
      Cor1 = abs(cor(dsiSortTclass[,UstarColName],dsiSortTclass[,tempColName]))
      # TODO: check more correlations here? [check C code]
      #      Cor2 = abs(cor(dataMthTsort$Ustar,dataMthTsort$nee))
      #      Cor3 = abs(cor(dataMthTsort$tair,dataMthTsort$nee))
      if (Cor1 < ctrlUstarEst.l$corrCheck){ #& Cor2 < CORR_CHECK & Cor3 < CORR_CHECK){
        dsiBinnedUstar <- binUstar(dsiSortTclass[,NEEColName],dsiSortTclass[,UstarName],ctrlUstarEst.l$ustClasses) 
        UstarTh.v[k]=fEstimateUStarBinned(  dsiBinnedUstar, ctrlUstarEst.l = ctrlUstarEst.l)
      }
      else { #correlation between T and u* too high
        #fill respective cell with NA
        UstarTh.v[k] = NA
        #TODO: should a message be printed here to the user??
      }
    }
    #OUTPUT:
    # This function returns one single Ustar threshold value
    # i.e., the median over taClasses
    UStarTh # uStar for temperature classes
    #return(median(UstarTh.v))
  }) # daply over seasons  matrix (nTmep x nSeason)
  uStarAggr <- max( apply( uStarSeasons, 1, median, na.rm=TRUE), na.rm=TRUE)
  list(
    UstarAggr=uStarAggr
    ,UstarTempSeasons=uStarSeasons
    )
}

estUstarThreshold <- function(
		### apply estUstarThresold for each year in ds
		ds						    ##<< data.frame with columns
		,...						##<< further arguments to \code{\link{estUstarThresholdYear}}
		,seasonFactor.v = (as.POSIXlt(ds$DateTime)$mon-1) %/% 3	##<< factor of seasons so split dsYear
		,yearFactor.v = as.POSIXlt(ds$DateTime)$year+1900	##<< factor vector (nrow(dsYear) of seasons so split dsYear 
){
	##references<< inspired by Papale 2006
	ds$season <- seasonFactor.v
	if( !length( yearFactor.v) ){
		c( '0' = estUstarThresholdYear(ds,...) )
	}else{
		ds$yearFac <- yearFactor.v
		daply( ds, .(yearFactor.v), function(dsYear){ 
      uStar.l <- estUstarThresholdYear(dsYear, ... )
      uStar.l$UstarAggr
		})
	}
	
	##value<< a vector with Ustar Threshold estimates. Names correspond to the year of the estimate 
}
attr(estUstarThreshold ,"ex") <- function(){
	Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
	EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
  dss <- subset(EddyData.F, DoY >= 150 & DoY <= 250)
	dss2 <- dss; dss2$Year <- 1999
	EddyDataWithPosix.F <- ds <- fConvertTimeToPosix(rbind(dss,dss2), 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
	(res <- estUstarThreshold(ds))
}


estUstarThresholdDistribution <- function(
		### Estimating the UstarTrhesholdDistribution by bootstrapping over data
		ds					    ##<< data.frame with columns
		,...					##<< further arguments to \code{\link{estUstarThreshold}}
		,nSample = 100
        ,probs = c(0.05,0.5,0.95)
){
		Ustar.l <- boot( ds, estUstarThreshold, nSample)
        
        ##value<< a matrix with with first column denoting the year and other columns correponsing to the quantiles of Ustar estimate for given probabilities \code{probs}
		years = c(2001,2002)
		#years = c(2001)
		UstarQuantiles = matrix( probs, byrow=TRUE, ncol=length(probs), nrow=length(years), dimnames=list(NULL,probs))
        res <- 
				cbind( 	year=years, UstarQuantiles)             
}



