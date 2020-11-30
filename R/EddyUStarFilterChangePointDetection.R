.fitSeg1 <- function(
		### fit a segmented relationship according to eq 1b of Barr13
		x
		, y
		, n = length(x)
		, nBootSegmented = 3
) {
  # check upstream once instead of in each loop
  # if (!requireNamespace("segmented")) stop(
  #   "functionality is only supported if segmented package is installed.")
	xr <- -x
	lm1 <- lm(y~1)
	seg1 <- segmented::segmented(lm1, seg.Z = ~xr, psi = xr[n %/% 2]
	    # precision of 0.01 / 2 is sufficient
			#, segmented::seg.control(toll = 0.005)
			#, segmented::seg.control(toll = 0.005, n.boot = 0)
			, segmented::seg.control(tol = 0.005, n.boot = nBootSegmented)
	)
	cf <- as.numeric(coef(seg1))
	##value<< numeric vector with entries
	c(
			b0 = cf[1]			##<< intercept of second part
			, b1 = -cf[2]		##<< first slope (second slope is fixed to zero)
			, cp = -seg1$psi[2]	  ##<< estimated breakpoint
			, sdCp = seg1$psi[3]	##<< estimated standard error of cp
			, p = anova(lm(y~xr), seg1, test = "LRT")[[5]][2]	##<< probability of
			## F test that segmented model is better than a linear model
	)
}

.tmp.f <- function() {
	plot(y ~ xr)
	abline(lm1)
	lines(seg1)
}


.fitSeg2 <- function(
		### fit a segmented relationship according to eq 1a of Barr13
		x
		, y
		, n = length(x)
) {
	xr <- -x
	lm1 <- lm(y~xr)
	seg1 <- segmented::segmented(lm1, seg.Z =~xr, psi = xr[n%/%2])
	cf <- as.numeric(coef(seg1))
	##value<< numeric vector with entries
	c(a0 = cf[1]			##<< intercept of second part
		, a1 =-cf[3]		##<< first slope
		, a2 =-cf[2]		##<< second slope
		, cp =-seg1$psi[2]	##<< estimated breakpoint
		, sdCp = seg1$psi[3]	##<< estimated standard error of cp
		, p = anova(lm1, seg1)[[6]][2]	##<< probability of F test that segmented model is better than linear model (with slope and intercept)
	)
}
attr(.fitSeg2, "ex") <- function() {
	n <- 11L
	x <- seq(0L, 1L, length.out = n)
	noise <- rnorm(n, sd = 0.1)
	y1 <- y2 <- y3 <- rep(1, n) + noise
	iSlope <- 1:(n / 2L)
	y2[iSlope] <- 0.5 + x[iSlope] + noise[iSlope]
	y3[iSlope] <- 0.2 + (0.8 / 0.5) * x[iSlope] + noise[iSlope]
	y <- y2
	plot(y ~ x)
	cf2 <- .fitSeg2(x, y)
	cf1 <- .fitSeg1(x, y)
}

.tmp.f <- function() {
	trace(.fitSeg1, recover)	#untrace(.fitSeg1)
	tmp <- .fitSeg1(dsiSortTclass[, UstarColName], dsiSortTclass[, "NEE"])
}





.estimateUStarSeasonCPTSeveralT <- function(
		### similar to .estimateUStarSeason but with extended temperature classification
		dsi
		, ctrlUstarSub
		, ctrlUstarEst
		, fEstimateUStarBinned
		, nBootSegmented = 3L ##<< number of bootstrap samples used in segmented.
		## may set to zero, if called already within a bootstrap
) {
  if (!requireNamespace("segmented", quietly = TRUE)) stop(
    "uStar threshold change point detection is only supported "
    ,"if segmented package is installed.")
	nTaClasses <- 3L * ctrlUstarSub$taClasses - 3L	# number of temperature classes expanded
	resNA <- 	list(
			UstarTh.v = rep(NA_real_, nTaClasses)	##<< vector of uStar for temperature classes
			, bins.F = data.frame(tempBin = rep(NA_integer_, nrow(dsi))
					, uStarBin = rep(NA_integer_, nrow(dsi)) )			##<< data.frame with columns tempBin, uStarBin for each row in dsi
	)
	if (nrow(dsi) < ctrlUstarSub$minRecordsWithinSeason) {
		warning("sEstUstarThreshold: too few finite records within season (n = ", nrow(dsi), "). Need at least n = ", ctrlUstarSub$minRecordsWithinSeason, ". Returning NA for this Season.")
		return(resNA)
	}
	if (nrow(dsi) / ctrlUstarSub$taClasses < ctrlUstarSub$minRecordsWithinTemp) {
		warning("sEstUstarThreshold: too few finite records within season (n = ", nrow(dsi), ") for ", ctrlUstarSub$taClasses
				, " temperature classes. Need at least n = ", ctrlUstarSub$minRecordsWithinTemp * ctrlUstarSub$taClasses
				, ". Returning NA for this Season.")
		return(resNA)
	}
	orderTemp <- order(dsi[, "Tair"])
	uStarBinSortedT <- integer(nrow(dsi))		# default value for methods that do not bin uStar
	dsiSort <- dsi[orderTemp, , drop = FALSE] 	#sort values in a season by air temperature (later in class by ustar)
	##details<<
	## In order for robustness, bin temperature by several bin widths:
	## In addition wo width ctrlUstarSub$taClasses, width reduced by 1 and 2
	## providing 7 + 6 + 5 = 18 classes for the median
	# taClasses <- 7L
	#
	# for mosted detailed temperature classing, report classes with results
	TId0 <- .binWithEqualValuesBalanced(dsiSort[, "Tair"], ctrlUstarSub$taClasses)
	TIdUnsorted <- uStarBinUnsortedT <- integer(length(orderTemp)); 	# 0L
	TIdUnsorted[orderTemp] <- TId0
	# plot(TIdUnsorted ~ dsi$Tair) # for checking Temperature binning
	#
	thresholdsTList <- lapply(
	  ctrlUstarSub$taClasses - (0L:min(2L, ctrlUstarSub$taClasses - 1L)),
	  function(taClasses) {
	    TId <- if (taClasses ==  ctrlUstarSub$taClasses) TId0 else
	      .binWithEqualValuesBalanced(dsiSort[, "Tair"], taClasses)
	    #k <- 1L
	    thresholds <- vapply(1:taClasses, function(k) {
	      dsiSortTclass <- dsiSort[TId == k, ]
	      ##details<<
	      ## Temperature classes, where NEE is still correlated to temperature
	      ## are not used for uStar threshold estimation.
	      # maybe too few or degenerate cases
	      Cor1 = suppressWarnings(
	        abs(cor(dsiSortTclass[, "Ustar"], dsiSortTclass[, "Tair"])) )
	      # TODO: check more correlations here? [check C code]
	      #      Cor2 = abs(cor(dataMthTsort$Ustar, dataMthTsort$nee))
	      #      Cor3 = abs(cor(dataMthTsort$tair, dataMthTsort$nee))
	      if ((!is.finite(Cor1)) || (Cor1 > ctrlUstarEst$corrCheck)) return(NA_real_)
	      resCPT <- try(suppressWarnings(
	        .fitSeg1(dsiSortTclass$Ustar, dsiSortTclass$NEE,
	                 nBootSegmented = nBootSegmented)),
	        silent = TRUE)
	      threshold <- if (inherits(resCPT, "try-error") ||
	                       !is.finite(resCPT["p"]) ||
	                       resCPT["p"] > 0.05)
	        # testing weighted mean, no improment, simplify again
	        #c(NA_real_, NA_real_) else resCPT[c("cp", "sdCp")]
	        c(NA_real_) else resCPT[c("cp")]
	      return(threshold)
	    }, FUN.VALUE = numeric(1L), USE.NAMES = FALSE)
	  }  )
	##value<< and list with entries (invisible, because might be large)
	invisible(list(
			UstarTh.v = do.call(c, thresholdsTList) 	##<< vector of uStar for temperature classes
			, bins.F = data.frame(tempBin = TIdUnsorted, uStarBin = 0L) ##<< data.frame with columns tempBin, uStarBin for each row in dsi.
				## Temperature bins are reported for binning according to ctrlUstarSub$taClasses.
				## There is only a single uStar bin with value 0L.
	))
}

.tmp.f <- function() {
	tmp <- UstarTh.l
	#tmp <- UstarAndSdSeasonsTemp
	ggplot(tmp, aes(x = 1:length(UstarTh.v), y = UstarTh.v, color = as.factor(season))) +
			geom_errorbar(aes(ymin = UstarTh.v-sdUstarTh.v, ymax = UstarTh.v + sdUstarTh.v), width = .1) +
			#geom_line() +
			geom_point()
}

.tmp.f <- function() {
	tmp <- UstarTh.l
	#tmp <- UstarAndSdSeasonsTemp
	ggplot(dsiSortTclass, aes(x = ustar_level4, y = NEEorig_level4)) +
			geom_point() + geom_smooth()
}
