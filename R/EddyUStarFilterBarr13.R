fitSeg1 <- function(
		### fit a segmented relationship according to eq 1b of Barr13
		x
		,y
		,n=length(x)
){
	xr <- -x
	lm1 <- lm(y~1)
	seg1 <- segmented(lm1, seg.Z=~xr, psi= xr[n%/%2]
			#, seg.control(toll = 0.005) 	# precision of 0.01/2 is sufficient
			#, seg.control(toll = 0.005, n.boot=0) 	# precision of 0.01/2 is sufficient
			, seg.control(toll = 0.005, n.boot=3) 	# precision of 0.01/2 is sufficient
	)
	cf <- as.numeric(coef(seg1))
	##value<< numeric vector with entries
	c( 
			b0=cf[1]			##<< intercept of second part
			, b1=-cf[2]		##<< first slope (second slope is fixed to zero)
			, cp=-seg1$psi[2]	##<< estimated breakpoint
			, sdCp=seg1$psi[3]	##<< estimated standard error of cp
			, p=anova(lm(y~xr), seg1, test="LRT")[[5]][2]	##<< probability of F test that segmented model is better than a linear model
	)
}

.tmp.f <- function(){
	plot( y ~ xr)
	abline(lm1)
	lines(seg1)
}


fitSeg2 <- function(
		### fit a segmented relationship according to eq 1a of Barr13
		x
		,y
		,n=length(x)
){
	xr <- -x
	lm1 <- lm(y~xr)
	seg1 <- segmented(lm1, seg.Z=~xr, psi= xr[n%/%2])
	cf <- as.numeric(coef(seg1))
	##value<< numeric vector with entries
	c( a0=cf[1]			##<< intercept of second part
		, a1=-cf[3]		##<< first slope
		, a2=-cf[2]		##<< second slope
		, cp=-seg1$psi[2]	##<< estimated breakpoint
		, sdCp=seg1$psi[3]	##<< estimated standard error of cp
		, p=anova(lm1, seg1)[[6]][2]	##<< probability of F test that segmented model is better than linear model (with slope and intercept)
	)
}
attr(fitSeg2,"ex") <- function(){
	n <- 11L
	x <- seq(0L,1L,length.out=n)
	noise <- rnorm(n, sd=0.1)
	y1 <- y2 <- y3 <- rep(1,n) + noise
	iSlope <- 1:(n/2L) 
	y2[iSlope] <- 0.5 + x[iSlope] + noise[iSlope]  
	y3[iSlope] <- 0.2 + (0.8/0.5)*x[iSlope] + noise[iSlope]
	y <- y2
	plot( y ~ x)
	cf2 <- fitSeg2(x,y)
	cf1 <- fitSeg1(x,y)
}

.tmp.f <- function(){
	trace(fitSeg1, recover)	#untrace(fitSeg1)
	tmp <- fitSeg1(dsiSortTclass[,UstarColName], dsiSortTclass[,NEEColName])
}


.estimateUStarSeasonCPTSeveralT <- function(dsi, ctrlUstarSub.l, ctrlUstarEst.l, fEstimateUStarBinned
		,UstarColName 		##<< collumn name for UStar
		,NEEColName 		##<< collumn name for NEE
		,TempColName 		##<< collumn name for air temperature
		,RgColName 			##<< collumn name for solar radiation for omitting night time data
){
	if( isTRUE(ctrlUstarEst.l$isUsingCPTSeveralT) ){
		return(.estimateUStarSeasonCPT(dsi, ctrlUstarSub.l, ctrlUstarEst.l, fEstimateUStarBinned, UstarColName, NEEColName, TempColName, RgColName))
	}
	if( nrow(dsi) < ctrlUstarSub.l$minRecordsWithinSeason){
		warning("sEstUstarThreshold: too few finite records within season (n=",nrow(dsi),"). Need at least n=",ctrlUstarSub.l$minRecordsWithinSeason,". Returning NA for this Season." )
		return( rep(NA_real_, ctrlUstarSub.l$taClasses))
	}
	if( nrow(dsi)/ctrlUstarSub.l$taClasses < ctrlUstarSub.l$minRecordsWithinTemp ){
		warning("sEstUstarThreshold: too few finite records within season (n=",nrow(dsi),") for ",ctrlUstarSub.l$taClasses
				," temperature classes. Need at least n=",ctrlUstarSub.l$minRecordsWithinTemp*ctrlUstarSub.l$taClasses
				,". Returning NA for this Season." )
		return( rep(NA_real_, ctrlUstarSub.l$taClasses))
	}
	# if( as.POSIXlt(dsi$sDateTime[1])$year+1900==2002 & dsi$season[1]==2L ) recover()	
	#cat(dsi$season[1], as.POSIXlt(dsi$DateTime[1])$mon, ",")
	dsiSort <- arrange(dsi, dsi[,TempColName]) 	#sort values in a season by air temperature (later in class by ustar)
	#N <- nrow(dsi ) #number of observations (rows) total, probably can get from elsewhere..
	#T_bin_size <- round(N/ctrlUstarSub.l$taClasses) #set T_bin size so that every bin has equal #values
	#set up vector that contains Ustar values for temperature classes
	UstarTh.v = vector(length=ctrlUstarSub.l$taClasses)
	# twutz 1505: changed temperature binning of records to put equals temperatures into the same bin (compatibility with C code)
	TId <- .binWithEqualValues(dsiSort[,TempColName], ctrlUstarSub.l$taClasses)
	#k<-1L
	for (k in 1:ctrlUstarSub.l$taClasses){	# k temperature class
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
#		if (k==ctrlUstarSub.l$taClasses){ # use end index of vector for slightly smaller last bin (due to rounding) 
#			dsiSortTclass <- dsiSort[((k-1)*T_bin_size+1):N,]
#		} else {
#			dsiSortTclass <- dsiSort[((k-1)*T_bin_size+1):((k)*T_bin_size),]
#		}
		dsiSortTclass <- dsiSort[TId == k,]
		#constraint: u* threshold only accepted if T and u* are not or only weakly correlated..
		Cor1 = suppressWarnings( abs(cor(dsiSortTclass[,UstarColName],dsiSortTclass[,TempColName])) ) # maybe too few or degenerate cases
		# TODO: check more correlations here? [check C code]
		#      Cor2 = abs(cor(dataMthTsort$Ustar,dataMthTsort$nee))
		#      Cor3 = abs(cor(dataMthTsort$tair,dataMthTsort$nee))
		if( (is.finite(Cor1)) && (Cor1 < ctrlUstarEst.l$corrCheck)){ #& Cor2 < CORR_CHECK & Cor3 < CORR_CHECK){
			if( isTRUE(ctrlUstarEst.l$isUsingCPT) ){
				resCPT <- try( fitSeg1(dsiSortTclass[,UstarColName], dsiSortTclass[,NEEColName]), silent=TRUE )
				UstarTh.v[k] <- if( inherits(resCPT,"try-error") || !is.finite(resCPT["p"]) || resCPT["p"] > 0.05) NA else resCPT["cp"]
			} else {
				dsiBinnedUstar <- binUstar(dsiSortTclass[,NEEColName],dsiSortTclass[,UstarColName],ctrlUstarSub.l$UstarClasses)
				#plot( NEE_avg ~ Ust_avg, dsiBinnedUstar)
				if( any(!is.finite(dsiBinnedUstar[,2])) ){
					stop("Encountered non-finite average NEE for a UStar bin.",
							"You need to provide data with non-finite collumns uStar and NEE for UStar Threshold detection.")
				}
				UstarTh.v[k]=fEstimateUStarBinned(  dsiBinnedUstar, ctrlUstarEst.l = ctrlUstarEst.l)
			}
		} else { #correlation between T and u* too high
			#fill respective cell with NA
			UstarTh.v[k] = NA
			#TODO: should a message be printed here to the user??
		}
	}
	UstarTh.v # vector of uStar for temperature classes
}



.estimateUStarSeasonCPTSeveralT <- function(dsi, ctrlUstarSub.l, ctrlUstarEst.l, fEstimateUStarBinned
		,UstarColName 		##<< collumn name for UStar
		,NEEColName 		##<< collumn name for NEE
		,TempColName 		##<< collumn name for air temperature
		,RgColName 			##<< collumn name for solar radiation for omitting night time data
){
	nTaClasses <- 3L*ctrlUstarSub.l$taClasses - 3L
	if( nrow(dsi) < ctrlUstarSub.l$minRecordsWithinSeason){
		warning("sEstUstarThreshold: too few finite records within season (n=",nrow(dsi),"). Need at least n=",ctrlUstarSub.l$minRecordsWithinSeason,". Returning NA for this Season." )
		return( rep(NA_real_, nTaClasses))
	}
	if( nrow(dsi)/ctrlUstarSub.l$taClasses < ctrlUstarSub.l$minRecordsWithinTemp ){
		warning("sEstUstarThreshold: too few finite records within season (n=",nrow(dsi),") for ",ctrlUstarSub.l$taClasses
				," temperature classes. Need at least n=",ctrlUstarSub.l$minRecordsWithinTemp*ctrlUstarSub.l$taClasses
				,". Returning NA for this Season." )
		return( rep(NA_real_, nTaClasses))
	}
	#if( as.POSIXlt(dsi$sDateTime[1])$year+1900==2002 & dsi$season[1]==2L ) recover()	
	#cat(dsi$season[1], as.POSIXlt(dsi$DateTime[1])$mon, ",")
	dsiSort <- arrange(dsi, dsi[,TempColName]) 	#sort values in a season by air temperature (later in class by ustar)
	##details<< 
	## In order for robustness, bin temperatue by several bin widths: 
	## In addition wo width ctrlUstarSub.l$taClasses, width reduced by 1 and 2
	## providing 7+6+5=18 classes for the median
	# taClasses <- 7L
	thresholdsTList <- lapply( ctrlUstarSub.l$taClasses - (0:2) , function(taClasses){
		TId <- .binWithEqualValues(dsiSort[,TempColName], taClasses)
		#k <- 1L
		thresholds <- vapply( 1:taClasses, function(k){
					dsiSortTclass <- dsiSort[TId == k,]
					##details<< 
					## Temperature classes, where NEE is still correlated to temperature 
					## are not used for uStar threshold estimation.
					Cor1 = suppressWarnings( abs(cor(dsiSortTclass[,UstarColName],dsiSortTclass[,TempColName])) ) # maybe too few or degenerate cases
					# TODO: check more correlations here? [check C code]
					#      Cor2 = abs(cor(dataMthTsort$Ustar,dataMthTsort$nee))
					#      Cor3 = abs(cor(dataMthTsort$tair,dataMthTsort$nee))
					if( (!is.finite(Cor1)) || (Cor1 > ctrlUstarEst.l$corrCheck)) return(NA_real_)
					resCPT <- try( suppressWarnings(fitSeg1(dsiSortTclass[,UstarColName], dsiSortTclass[,NEEColName])), silent=TRUE )
					threshold <- if( inherits(resCPT,"try-error") || !is.finite(resCPT["p"]) || resCPT["p"] > 0.05) 
								#c(NA_real_,NA_real_) else resCPT[c("cp","sdCp")]	# testing weighted mean, no improment, simplify again 
								c(NA_real_) else resCPT[c("cp")]
					return(threshold)
				}, FUN.VALUE=numeric(1L), USE.NAMES = FALSE)
	})
#	UstarTh.l <- data.frame(
#		UstarTh.v  = do.call( c, lapply(thresholdsTList,"[",1,TRUE)) # vector of uStar for temperature classes
#		,sdUstarTh.v = do.call( c, lapply(thresholdsTList,"[",2,TRUE)) # vector of uStar for temperature classes
#	)	
	UstarTh.v  = do.call( c, thresholdsTList )
}

.tmp.f <- function(){
	tmp <- UstarTh.l
	#tmp <- UstarAndSdSeasonsTemp
	ggplot(tmp, aes(x=1:length(UstarTh.v), y=UstarTh.v, color=as.factor(season))) + 
			geom_errorbar(aes(ymin=UstarTh.v-sdUstarTh.v, ymax=UstarTh.v+sdUstarTh.v), width=.1) +
			#geom_line() +
			geom_point()
}

.tmp.f <- function(){
	tmp <- UstarTh.l
	#tmp <- UstarAndSdSeasonsTemp
	ggplot(dsiSortTclass, aes(x=ustar_level4, y=NEEorig_level4)) + 
			geom_point() + geom_smooth()
}
