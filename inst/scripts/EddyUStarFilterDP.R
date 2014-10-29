#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc methods for ustar filtering +++
#+++ Ustar filtering adapted after the idea in Papale, D. et al. (2006) +++
#+++ Dependencies: Eddy.R
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sMDSwithUStarDP = structure(function(
    ##title<< 
    ## sEddyProc$sMDSwithUStarDP - MDS gap filling with u* filtering
    ##description<<
    ## Calling \code{\link{sMDSGapFill}} for several filters of friction velocity u*
    Var.s                    ##<< Variable to be filled
    ,ustar.m   = .self$sEstUstarThresholdDistribution() # quantile( sDATA[,UstarVar.s], probs=c(0.9,0.7,0.95), na.rm=T)       
    ### Numeric matrix(nYear x nUStar): Each row is a vector of ustar thresholds to apply before gap filling for one year.
    ### Default is output of \code{\link{sEstUstarThresholdDistribution}}.
    ### Otherwise provide matrix with one row given for each year in the dataset to gap-fill.
    ### If only one row is provided, i.e. a vector, then this is used for each year.
    ,suffix.v = c("Ustar","U05","U50","U95")  ##<< String vector of suffixes for each ustar.m column  
    ## to distinguish result columns for different ustar values.
    ## Hence length must correspond to column numbers in ustar.m.
    ## Default corresponds to returned default columns of function \code{\link{sEstUstarThresholdDistribution}} 
    ## with estimates on original series, 5% of bootstrap, median of bootstrap, 95% of bootstrap. 
    ,...                  	 ##<< Other arguments passed to \code{\link{sMDSGapFill}}
    ,UstarVar.s='Ustar'   	 ##<< Column name of friction velocity u* (ms-1), default 'Ustar'
    ,isFlagEntryAfterLowTurbulence=TRUE  ##<< Default TRUE for flagging the first entry after low turbulance as bad condition.
  )
  ##author<< TW
{
    ##details<< 
    ## The u* threshold for filtering the conditions of low turbulence are by default
    ## estimated with the algorithm \code{\link{sEstUstarThresholdDistribution}}
    ## or can be provided by the user as matrix ustar.m with column suffixes in suffix.v.
    ## The data is then filtered for u* and gapfilled.
    
    ##seealso<< 
    ## \code{\link{sEstUstarThresholdDistribution}}
    
    year.v <- as.POSIXlt(sDATA$sDateTime)$year + 1900
    uYear.v <- unique(year.v)
    if( !is.matrix(ustar.m) ){
      if( length(suffix.v) != length(ustar.m)) stop("sMDSwithUStarDP: number of suffixes must correspond to number of uStar-thresholds")
      ustar.m <- matrix( ustar.m, nrow=length(uYear.v), dimnames=list(uYear.v,suffix.v))
    }
    nUStar <- ncol(ustar.m)
    if( length(suffix.v) != nUStar ) stop("sMDSwithUStarDP: number of suffixes must correspond to number of uStar-thresholds")
    # possibly parallelize, but difficult with R5 Classes
    # iCol <- 1
    lapply( seq(1:nUStar), function(iCol){
      QF.V.b <- rep( TRUE, nrow(sDATA) )
      #iYear <- 1
      for( iYear in seq_along(uYear.v) ){
        QF.V.b[ (year.v==uYear.v[iYear]) & is.finite(ustar.m[iYear, iCol]) & (sDATA[,UstarVar.s] < ustar.m[iYear, iCol]) ] <- FALSE  
      }
      if( isTRUE(isFlagEntryAfterLowTurbulence) ){
        ##details<< 
        ## With \code{isFlagEntryAfterLowTurbulence set to TRUE}, to be more conservative, in addition
        ## to the data acquired when u* is below the threshold,
        ## the first half hour measured with good turbulence conditions
        ## after a period with low turbulence is also removed (Papaple et al. 2006).
        #QF.V.b <- c( TRUE, TRUE, FALSE, TRUE)
        QF.V.b[ which(diff(QF.V.b) == 1)+1 ] <- FALSE
      }
      message('Using Ustar threshold of ',paste(signif(ustar.m[, iCol],2), collapse=","),
              ' introduced ',(1-signif(sum(QF.V.b)/length(QF.V.b),2))*100,'% gaps'  )
      .self$sMDSGapFill( Var.s, ..., QF.V.b=QF.V.b, suffix = suffix.v[iCol] )
    } )
    return(invisible(NULL))
    ##value<< 
    ## Gap filling results in sTEMP data frame (with renamed columns), that can be retrieved by \code{\link{sExportResults}}.
    ## Each of the columns is calculated for several u*r-estimates and distinguished by a suffix after the variable. 
    ## By default NEE for best UStar estimate is given in column NEE_UStar_f, 
    ## and NEE based on lower and upper 90% confidence interval estimates of Ustar threshold 
    ## are returned in columns NEE_U05_f and NEE_U95_f respectively.
  }, ex=function(){
    if( FALSE ) { #Do not always execute example code (e.g. on package installation)
      # Load the data from text file
      Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
      EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
      # Create TimeStamp column
      EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
      
      #+++ Default use case
      EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))   
      EddyProc.C$sMDSwithUStarDP('NEE' )     # calls sEstUstarThresholdDistribution 
      dsf <- EddyProc.C$sExportResults()
      colnames(dsf)		# note the different output columns corresponding to different Ustar estimates, best estimate with suffix "Ustar"
      #plot( NEE_U05_f ~ NEE_U95_f, dsf)	# differences between gapFilling using differing Ustar thresholds
      
      #+++ Only using one Ustar estimate (omitting output columns for range of Ustar estimates)
      EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
      Ustar <- EddyProc.C$sEstUstarThreshold()$UstarAggr
      EddyProc.C$sMDSwithUStarDP('NEE', ustar.m=Ustar, suffix.v="Ustar")      
      colnames(EddyProc.C$sExportResults())
      
      #+++ The advanced user can specify his own estimates of Ustar 
      # modifying arguements to sEstUstarThresholdDistribution, e.g quantiles to inspect
      EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))   
      ustar.m <- EddyProc.C$sEstUstarThresholdDistribution(EddyProc.C$sExportData(), nSample=10, probs=c(0.25,0.5,0.75))
      ustar.m		# note that the first entry corresponds to the non-bootstrapped Ustar estimate 
      EddyProc.C$sMDSwithUStarDP('NEE', ustar.m=ustar.m, suffix.v=c("Ustar","U25","U50","U75") )
      colnames(EddyProc.C$sExportResults())
      #
      # specify thresholds directly
      EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
      ustar.m <- c(0.38, 0.44)	# used for all years
      ustar.m = matrix(c(0.38,0.42), byrow=TRUE, ncol=2, nrow=2, dimnames=list(years=c(1998,1999),probs=c("05","95") )) # possible different thresholds for different years  
      EddyProc.C$sMDSwithUStarDP('NEE', ustar.m=ustar.m, suffix.v=c("U38","U44") )
      colnames(EddyProc.C$sExportResults())
    }
  }))

sEddyProc$methods(
	sEstUstarThreshold = structure(function(
	  ##title<<
	  ## sEddyProc$sEstUstarThreshold - Estimating ustar threshold
	  ##description<<
	  ## Estimate the Ustar threshold by aggregating the estimates for seasonal and temperature subsets.
	  ##author<<
	  ## OM, TW
		ds = sDATA					##<< data.frame with columns named by the following arguments
		,UstarColName = "Ustar"		##<< collumn name for UStar
		,NEEColName = "NEE"			##<< collumn name for NEE
		,TempColName = "Tair"		##<< collumn name for air temperature
		,RgColName = "Rg"			##<< collumn name for solar radiation for omitting night time data
		,seasonFactor.v = createSeasonFactorMonth(ds$sDateTime) ##<< factor for subsetting times see details 
		,ctrlUstarEst.l = controlUstarEst()			##<< control parameters for estimating uStar on a single binned series, see \code{\link{controlUstarEst}}
		,ctrlUstarSub.l = controlUstarSubsetting()	##<< control parameters for subsetting time series (number of temperature and Ustar classes \ldots), see \code{\link{controlUstarSubsetting}} 
		,fEstimateUStarBinned = estUstarThresholdSingleFw2Binned	##<< function to estimate UStar on a single binned series, see \code{\link{estUstarThresholdSingleFw2Binned}}
		,isCleaned=FALSE			##<< set to TRUE to avoid call to \code{\link{cleanUStarSeries}}.
){
	##references<< 
  ## Ustar filtering following the idea in Papale, D. et al. (2006)  
  ## Towards a standardized processing of net ecosystem exchange measured with eddy covariance technique: algorithms and uncertainty estimation.
  ## Biogeosciences 3(4): 571-583.
    
	# subset for first season
	# TODO: implement seasonal functionality
	#dsi <- subset(ds, seasonFactor.v == 1)
	#					
	##details<<
	## The threshold for sufficiently turbulent conditions u* (Ustar) 
	## is estimated for different subsets of the time series.
	## From the estimates for each season (each value in \code{seasonFactor.v}) 
	## the maximum is reported as global estimate.
	## Within each season the time series is split by temperature classes. 
	## Among these Ustar estimates, the median is reported as season value.
	##
	## In order to split the seasons, the uses has provide a vector with argument \code{seasonFactor.v}
	## where rows with the same value belong to
	## the same season. It is conveniently generated by one of the following functions:
	## \itemize{
	## 	\item{ \code{\link{createSeasonFactorMonth}} (default DJF-MAM-JJA-SON) }
	## 	\item{ \code{\link{createSeasonFactorYday}} }
	## } 
	##
	## The estimation of Ustar on a single binned series can be selected argument \code{fEstimateUStarBinned}.
	## \itemize{
	## 	\item{ \code{\link{estUstarThresholdSingleFw1Binned}} }
	## 	\item{ \code{\link{estUstarThresholdSingleFw2Binned}} (default) }
	## } 
	##
	## This function is usually called by
	## \itemize{
	## \item{ \code{\link{sEstUstarThresholdYears}} that applies this function to subsets of each year. }
	## \item{ \code{\link{sEstUstarThresholdDistribution}} which additionally estimates median and confidence intervals for each year by bootstrapping the original data.}
	## } 
	
	dsF <- cbind(ds,season=seasonFactor.v)
	dsc <- if( isCleaned) dsF else cleanUStarSeries( dsF, UstarColName, NEEColName, TempColName, RgColName, ctrlUstarSub.l$swThr)
	## TODO check number of records
	if( nrow(dsc) < ctrlUstarSub.l$minRecordsWithinYear){
		stop("sEstUstarThreshold: too few finite records within one year " 
				,if(length(dsc$DateTime)) as.POSIXlt(dsc$DateTime[1])$year else if(length(dsc$Year)) dsc$Year[1] 
				,"(n=",nrow(dsc)
				,"). Need at least n=",ctrlUstarSub.l$minRecordsWithinYear
		)
	}
	dsi <- subset(dsc, season == 5)
	UstarSeasons <- daply(dsc, .(season), function(dsi){
		if( nrow(dsi) < ctrlUstarSub.l$minRecordsWithinSeason){
			warning("sEstUstarThreshold: too few finite records within season (n=",nrow(dsi),"). Need at least n=",ctrlUstarSub.l$minRecordsWithinSeason,". Returning NA for this Season." )
			return( rep(NA_real_, ctrlUstarSub.l$taClasses))
		}
		if( nrow(dsi)/ctrlUstarSub.l$taClasses < ctrlUstarSub.l$minRecordsWithinTemp ){
			warning("sEstUstarThreshold: too few finite records within season (n=",nrow(dsi),") for ",ctrlUstarSub.l$taClasses
					," temperature classes. Need at least n=",ctrlUstarSub.l$minRecordsWithinTemp*ctrlUstarSub.l$taClasses
					,". Returning NA for this Season." )
		}
		#cat(dsi$season[1], as.POSIXlt(dsi$DateTime[1])$mon, ",")
		dsiSort <- arrange(dsi, dsi[,TempColName]) 	#sort values in a season by air temperature and then by UStar
		N <- nrow(dsi ) #number of observations (rows) total, probably can get from elsewhere..
		T_bin_size <- round(N/ctrlUstarSub.l$taClasses) #set T_bin size so that every bin has equal #values
		#set up vector that contains Ustar values for temperature classes
		UstarTh.v = vector(length=ctrlUstarSub.l$taClasses)
		#k<-1
		for (k in 1:ctrlUstarSub.l$taClasses){
			# minimum number of records within temp checked above					
			#print(k)
			#if( k == 4 ) recover()
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
			} else {
				dsiSortTclass <- dsiSort[((k-1)*T_bin_size+1):((k)*T_bin_size),]
			}
			#constraint: u* threshold only accepted if T and u* are not or only weakly correlated..
			Cor1 = suppressWarnings( abs(cor(dsiSortTclass[,UstarColName],dsiSortTclass[,TempColName])) ) # maybe too few or degenerate cases
			# TODO: check more correlations here? [check C code]
			#      Cor2 = abs(cor(dataMthTsort$Ustar,dataMthTsort$nee))
			#      Cor3 = abs(cor(dataMthTsort$tair,dataMthTsort$nee))
			if( (is.finite(Cor1)) && (Cor1 < ctrlUstarEst.l$corrCheck)){ #& Cor2 < CORR_CHECK & Cor3 < CORR_CHECK){
				dsiBinnedUstar <- binUstar(dsiSortTclass[,NEEColName],dsiSortTclass[,UstarColName],ctrlUstarSub.l$UstarClasses)
				if( any(!is.finite(dsiBinnedUstar[,2])) ){
					recover()
					stop("Encountered non-finite average NEE for a UStar bin.",
							"You need to provide data with non-finite collumns uStar and NEE for UStar Threshold detection.")
				}
				UstarTh.v[k]=fEstimateUStarBinned(  dsiBinnedUstar, ctrlUstarEst.l = ctrlUstarEst.l)
			} else { #correlation between T and u* too high
				#fill respective cell with NA
				UstarTh.v[k] = NA
				#TODO: should a message be printed here to the user??
			}
		}
		UstarTh.v # vector of uStar for temperature classes
	},.drop_o = FALSE) # daply over seasons  matrix (nTemp x nSeason)
	uStarAggr <- max( apply( UstarSeasons, 1, median, na.rm=TRUE), na.rm=TRUE)
	message(paste("Estimated UStar threshold of: ", signif(uStarAggr,2)
					,"by using controls:\n", paste(capture.output(unlist(ctrlUstarSub.l)),collapse="\n")
			))
	##value<< A list with entries
	list(
			UstarAggr=uStarAggr				##<< numeric scalar: Ustar threshold estimate:  max_Seasons( median_TempClasses )
			,UstarSeasonTemp=UstarSeasons	##<< numeric matrix (nSeason x nTemp): estimates for each subset
	)
},ex = function(){
  if( FALSE ) { #Do not always execute example code (e.g. on package installation)
    Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
    EddyData.F <- ds <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
    EddyDataWithPosix.F <- ds <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))   
    #ds <- head(ds,2000)
    (Result.L <- EddyProc.C$sEstUstarThreshold())
  }
}))


controlUstarEst <- function(
  ### Default list of parameters for determining UStar of a single binned series	
  #T Classes not needed here?
  # either?
  #,taClasses=7 # set number of ta classes   
  # or?
  #,ctrlUstarSubsetting.l = controlUstarSubsetting()
  #,taClasses=ctrlUstarSubsetting.l$taClasses  
  #
  #,percentile = 90 #percentile value... double check!
  #,percentile_check = TRUE #enable percentile check\n ... double check!
  ustPlateauFwd = 10 	##<< number of subsequent thresholds to compare to in fwd mode
  ,ustPlateauBack = 6	##<< number of subsequent thresholds to compare to in back mode  
  ,plateauCrit = 0.95	##<< significant differences between a u* value and the mean of a "plateau"
  ,corrCheck = 0.5 		##<< threshold value for correlation between Tair and u* data
  #,bt = FALSE 			##<< flag for bootstrapping
  #,btTimes = 100 		##<< number of bootstrap samples
  
  #,method.v = function... #fw2 by default..
  
  #TODO: what does the following param do?
  #define FIRST_Ustar_MEAN_CHECK  		0.2  
  # 4.) const int percentiles[PERCENTILES_COUNT] = { 5, 10, 25, 50, 75, 90, 95};
  
){
  ##seealso<< \code{\link{estUstarThresholdSingleFw2Binned}}, \code{\link{controlUstarSubsetting}} 
  ctrl <- list(  
    #taClasses=taClasses
    #,UstarClasses=UstarClasses  
    #percentile = percentile
    #percentile_check = percentile_check #enable percentile check\n ... double check!
    ustPlateauFwd = ustPlateauFwd    #number of subsequent thresholds to compare to in fwd mode
    ,ustPlateauBack = ustPlateauBack #number of subsequent thresholds to compare to in back mode  
    ,plateauCrit = plateauCrit #significant differences between a u* value and the mean of a "plateau"
    ,corrCheck = corrCheck #threshold value for correlation between Tair and u* data
    #,seasons = seasons # switch for three different seasonal modes 
    #(seasons or "groupby" may easily extended to an input vector or matrix)
    #,bt = bt #flag for bootstrapping
    #,btTimes = btTimes #number of bootstrap samples
  )
  #display warning message for the following variables that we advise not to be changed
  if (corrCheck != 0.5) warning("WARNING: parameter corrCheck set to non default value!")
  ctrl
}
attr(controlUstarEst,"ex") <- function(){
	controlUstarEst()
}

controlUstarSubsetting <- function(
	### Default list of parameters for determining UStar of a single binned series	
	taClasses=7 		##<< set number of air temperature classes 
	,UstarClasses=20 	##<< set number of Ustar classes 	
	# seasons param deprecated
  # TODO: add seasons handling to documentation
  #,seasons = 1 # switch for different seasonal modes #TODO: Update?!
	,swThr = 10  		##<< nighttime data threshold for solar radion [Wm-2]
	,minRecordsWithinTemp = 100		##<< integer scalar: the minimum number of Records within one Temperature-class
	,minRecordsWithinSeason = 160	##<< integer scalar: the minimum number of Records within one season
	,minRecordsWithinYear	= 3000	##<< integer scalar: the minimum number of Records within one year
	# 1.) ,selection parameter for which fwd and back modes? fwd2 as default... 
	# 2.) ,MIN_VALUE_PERIOD <<- 3000 # per whole data set... double check C code
	# 3.) ,MIN_VALUE_SEASON <<- 160 #if #number of data points in one any season are smaller than that, merge to one big season
	#define MIN_VALUE_PERIOD    		3000		/* min values for compute u* threshold */
	#define MIN_VALUE_SEASON				160			/* min for seasons */
	#define TA_CLASS_MIN_SAMPLE				100
  ){  
	##seealso<< \code{\link{estUstarThresholdSingleFw2Binned}}, \code{\link{controlUstarSubsetting}} 
	ctrl <- list(
    	taClasses=taClasses
		,UstarClasses= UstarClasses
  		#,seasons
  		,swThr = swThr
		,minRecordsWithinTemp = minRecordsWithinTemp
		,minRecordsWithinSeason = minRecordsWithinSeason
		,minRecordsWithinYear = minRecordsWithinYear
)	
  if (ctrl$swThr != 10) warning("WARNING: parameter swThr set to non default value!")
  if (ctrl$taClasses != 7) warning("WARNING: parameter taClasses set to non default value!")	
  if (ctrl$UstarClasses != 20) warning("WARNING: parameter UstarClasses set to non default value!")
  if (ctrl$minRecordsWithinTemp != 100) warning("WARNING: parameter minRecordsWithinTemp set to non default value!")
  if (ctrl$minRecordsWithinSeason != 160) warning("WARNING: parameter minRecordsWithinSeason set to non default value!")
  if (ctrl$minRecordsWithinYear != 3000) warning("WARNING: parameter minRecordsWithinYear set to non default value!")
  ctrl
}
attr(controlUstarSubsetting,"ex") <- function(){
	controlUstarSubsetting()
}

createSeasonFactorMonth <- function(
	### calculate factors to denote the season for uStar-Filtering by specifying starting months
  	dates							##<< POSIXct vector of length of the data set to be filled				
  	, month=as.POSIXlt(dates)$mon   ##<< integer (0-11) vector of length of the data set to be filled, specifying the month for each record  
	, startMonth=c(12,3,6,9)-1		##<< integer vector specifying the starting month for each season, counting from zero
){
  ##seealso<< \code{\link{createSeasonFactorYday}}
  startMonth <- sort(unique(startMonth))
  boLastPeriod <- month < startMonth[1] 		# translate month before the first specified beginning month to be after last specified month
  month[ boLastPeriod ] <- month[ boLastPeriod] +12
  startMonthAdd <- c(startMonth, startMonth[1]+12)
  seasonFac <- rep(as.integer(1), length(month) )
  # i <- 2
  for( i in 2:length(startMonth) ){
	  seasonFac[ month >= startMonthAdd[i] & month < startMonthAdd[i+1] ] <- i
  }
  #plot( seasonFac ~ months )
  seasonFac 	
  ##value<<
  ## Integer vector length(dates), with each unique value representing one season
}
attr(createSeasonFactorMonth,"ex") <- function(){
	Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
	EddyData.F <- dss <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
	EddyDataWithPosix.F <- ds <- fConvertTimeToPosix(dss, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
	(res <- createSeasonFactorMonth(ds$DateTime))
	(res2 <- createSeasonFactorYday(ds$DateTime)) # default days are chosen to correspond to start of Febr, June, Sept, and Dec
}


createSeasonFactorYday <- function(
	### calculate factors to denote the season for uStar-Filtering by specifying starting day of years
	dates							##<< POSIXct vector of length of the data set to be filled				
	, yday=as.POSIXlt(dates)$yday  ##<< integer (0-11) vector of length of the data set to be filled, specifying the month for each record  
	, startYday=c(335,60,152,244)-1	##<< integer vector (0-366) specifying the starting month for each season
){
	startYday <- sort(unique(startYday))
	boLastPeriod <- (yday < startYday[1])	# days before the first starting day will be in last period 
	yday[ boLastPeriod ] <- yday[ boLastPeriod] +366  # translate day to be after last specified startday 
	startYdayAdd <- c(startYday, startYday[1]+366)  # 
	seasonFac <- rep(as.integer(1), length(yday) )
	# i <- 2
	for( i in 2:length(startYday) ){
		seasonFac[ yday >= startYdayAdd[i] & yday < startYdayAdd[i+1] ] <- i
	}
	#plot( monthsFac ~ months )
	seasonFac 	
	##value<<
	## Integer vector of nrow ds, each unique class representing one season
}


binUstar <- function(
	### Bin the NEE for a number of classes of UStar classes
	NEE.v				##<< vector with value of Net Ecosystem exchange
	,Ustar.v 			##<< vector with u* (friction velocity (m2/s)
	,UstarClasses=controlUstarSubsetting()$UstarClasses	##<< the number of binning classes
	,isUStarSorted=FALSE	##<< set to TRUE, if NEE and Ustar are already sorted by increasin Ustar values (performance gain)

){
	ds.f <- data.frame(NEE=NEE.v,Ustar=Ustar.v)
	
	#within data frame sort values by Ustar
	if( !isTRUE(isUStarSorted))	ds.f <- arrange(ds.f,ds.f[,2])
	
	N_T <- length(NEE.v) #number of observations(rows) 
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
	#Ust_bins.f[,2][ !is.finite(Ust_bins.f[,2]) ] <- NA	# only allow finite values (no NaN)
	Ust_bins.f
}
attr(binUstar,"ex") <- function(){
	Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
	EddyData.F <- ds <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
	EddyDataWithPosix.F <- ds <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
	dss <- subset(EddyDataWithPosix.F, DoY >= 150 & DoY <= 250)
	(res <- binUstar(dss$NEE, dss$Ustar))
	(resFW1 <- estUstarThresholdSingleFw1Binned(res))
	(resFW2 <- estUstarThresholdSingleFw2Binned(res))
}


estUstarThresholdSingleFw1Binned <- function(
		### estimate the Ustar threshold for single subset, using FW1 algorithm, relying on binned NEE and Ustar
		Ust_bins.f			##<< data.frame with columns NEE_avg and Ust_avg, of NEE by Ustar bins by \code{\link{binUstar}}
		,ctrlUstarEst.l = controlUstarEst() ##<< parameter list, see \code{\link{controlUstarEst}} for defaults and description
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

# TODO: restructure? may not be the most efficient cause Ustar bin averages have to be computed twice (or more depending on how many methods).
# TODO: wrap rest of Ustar code from fw1 around the while loop... (if necessary, depending on structure)
estUstarThresholdSingleFw2Binned <- function(
  ### estimate the Ustar threshold for single subset, using FW2 algorithm  	
  Ust_bins.f							##<< data.frame with column s NEE_avg and Ust_avg, of NEE by Ustar bins by \code{\link{binUstar}}
  ,ctrlUstarEst.l = controlUstarEst()	##<< parameter list, see \code{\link{controlUstarEst}} for defaults and description 
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

cleanUStarSeries <- function(
	### remove non-finite cases and omit night time data.
	ds						    ##<< data.frame with columns
	,UstarColName = "Ustar"  
	,NEEColName = "NEE"
	,TempColName = "Tair"
	,RgColName = "Rg"
	,swThr = controlUstarSubsetting()$swThr
){
	ds <- subset(ds, is.finite(ds[,NEEColName]) & is.finite(ds[,TempColName]) & is.finite(ds[,UstarColName]) & is.finite(ds[,RgColName]) ) 
	#night time data selection
	ds <- ds[ ds[,RgColName] < swThr, ]
	##value<< ds with non-finite cases and cases with radiation < swThr removed.
	ds
}

sEddyProc$methods(
		sEstUstarThresholdYears = structure(function(
		### apply sEstUstarThresholdYear for each year in dataset
		ds=sDATA					##<< data.frame with columns
		,boot=1:nrow(ds)			##<< indices for calling by \code{link{boot}}
		,...						##<< further arguments to \code{\link{sEstUstarThreshold}}
		,seasonFactor.v = createSeasonFactorMonth(ds$sDateTime)  ##<< factor of seasons to split
    	,yearFactor.v = as.POSIXlt(ds$sDateTime)$year+1900	  	##<< factor vector (nrow(dsYear) of seasons to split
		,UstarColName = "Ustar"		##<< collumn name for UStar
		,NEEColName = "NEE"			##<< collumn name for NEE
		,TempColName = "Tair"		##<< collumn name for air temperature
		,RgColName = "Rg"			##<< collumn name for solar radiation for omitting night time data
		,ctrlUstarSub.l = controlUstarSubsetting() ##<< control parameters for subsetting time series (number of temperature and Ustar classes \ldots), see \code{\link{controlUstarSubsetting}}
		,isCleaned=FALSE			##<< set to TRUE to avoid call to \code{\link{cleanUStarSeries}}.
){
	##details<< 
	## Due to change of surface roughness, the threshold of u* can change over years.
	## This methods estimates u* threshold for each subset by year.
	
	##seealso<< \code{\link{sEstUstarThreshold}}
	
	##references<< inspired by Papale 2006
	ds$seasonFactor <- seasonFactor.v
	ds$yearFactor <- yearFactor.v
	#remove any no data records for NEE, Tair, Ustar, and Rg
	dsBoot <- ds[boot,]
	dsc <- if( isTRUE(isCleaned) ) dsBoot else cleanUStarSeries( dsBoot, UstarColName, NEEColName, TempColName, RgColName, ctrlUstarSub.l$swThr)
	
	if( !length( yearFactor.v) ){
		c( '0' = .self$sEstUstarThreshold(dsc,...,ctrlUstarSub.l =ctrlUstarSub.l, isCleaned=TRUE )$UstarAggr )
	}else{
		#dsYear <- subset(dsc, yearFactor == dsc$yearFactor[1] )
		res <- daply( dsc, .(yearFactor), function(dsYear){
					if( nrow(dsYear) < ctrlUstarSub.l$minRecordsWithinYear){
						warning("sEstUstarThreshold: too few finite records within one year " 
							,if(length(dsc$DateTime)) as.POSIXlt(dsc$DateTime[1])$year else if(length(dsc$Year)) dsc$Year[1] 
							,"(n=",nrow(dsYear)
							,"). Need at least n=",ctrlUstarSub.l$minRecordsWithinYear
							,". Returning NA and continueing with next year."
							)
							return( NA )
						}
					uStar.l <- .self$sEstUstarThreshold(dsYear, ... ,UstarColName = UstarColName, NEEColName = NEEColName, TempColName = TempColName, ctrlUstarSub.l =ctrlUstarSub.l, isCleaned=TRUE  )
					uStar.l$UstarAggr
				}, .inform = TRUE, .drop_o = FALSE)
				#}, .inform = FALSE)
	}
	##value<< a vector with Ustar Threshold estimates. Names correspond to the year of the estimate 
}))

sEddyProc$methods(
		sEstUstarThresholdDistribution = structure(function(
		### Estimating the distribution of u* threshold by bootstrapping over data
		#ds					    ##<< data.frame with columns see \code{\link{sEstUstarThresholdYears}}
		ctrlUstarEst.l = controlUstarEst()			##<< control parameters for estimating uStar on a single binned series, see \code{\link{controlUstarEst}}
		,ctrlUstarSub.l = controlUstarSubsetting()	##<< control parameters for subsetting time series (number of temperature and Ustar classes \ldots), see \code{\link{controlUstarSubsetting}} 
		,...						##<< further arguments to \code{\link{sEstUstarThresholdYears}}
		,seasonFactor.v = as.factor(createSeasonFactorMonth(sDATA$sDateTime))   ##<< factor of seasons to split
		,yearFactor.v = as.factor(as.POSIXlt(sDATA$sDateTime)$year+1900)	  	##<< factor vector (nrow(dsYear) of seasons so split dsYear 
		,nSample = 30				##<< the number of repetitions in the bootstrap
        ,probs = c(0.05,0.5,0.95)	##<< the quantiles of the bootstrap sample to return. Default is the 5%, median and 95% of the bootstrap
){
		##details<< 
		## The choice of the criterion for sufficiently turbulent conditions (u* > choosen threshold)
		## introduces large uncertainties in calculations based on gap-filled Eddy data.
	  ## Hence, it is good practice to compare derived quantities based on gap-filled data using different u* threshold values.
		  
		##
		## This method explores the probability density of the threshold by repeating its estimation
		## on a bootstrapped sample.
		## By default it returns the 90% confidence interval (arguement \code{probs}). 
		## For larger intervals the sample number need to be increased (arguement \code{probs}). 
		
		##seealso<< \code{\link{sEstUstarThreshold}}, \code{\link{sEstUstarThresholdYears}}, \code{\link{sMDSwithUStarDP}}
		ds <- sDATA
		Ustar.l <- suppressMessages( 
				boot( ds, .self$sEstUstarThresholdYears, nSample, seasonFactor.v=seasonFactor.v, yearFactor.v=yearFactor.v
					,ctrlUstarEst.l =ctrlUstarEst.l, ctrlUstarSub.l=ctrlUstarSub.l
				) # only evaluate once
		)
        res <- cbind(  Ustar=Ustar.l$t0, t(apply( Ustar.l$t, 2, quantile, probs=probs, na.rm=TRUE )))
		rownames(res) <- as.numeric(rownames(Ustar.l$t0)) # years
		message(paste("Estimated UStar distribution of:\n", paste(capture.output(res),collapse="\n")
			,"\nby using ",nSample,"bootstrap samples and controls:\n", paste(capture.output(unlist(ctrlUstarSub.l)),collapse="\n")
		))
		res
        ##value<< 
		## a matrix (nYear x (1+nProbs): first two column is the original estimate 
		## the other columns correponsing to the quantiles of Ustar estimate 
		## for given probabilities (argument \code{probs} ).
		## Rownames hold the corresonding years.
},ex = function(){
	if( FALSE ){	# takes long, so do not execute on each install or check
		# load the data and generate DateTime column
		Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
		EddyData.F <- ds <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
		EddyDataWithPosix.F <- ds <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
		EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
		# initialize parallel setup and do the bootstrap, may omit
		#sfInit(parallel=TRUE,cpus=2)
		#options("boot.parallel" = "snow")
		#getOption("boot.parallel")
		#options("boot.parallel" = NULL)
		(res <- EddyProc.C$sEstUstarThresholdDistribution(nSample=10))
		#(res <- sEstUstarThresholdDistribution(subset(ds, as.POSIXlt(ds$DateTime)$year==98 ), nSample=20))
	}
}) )



