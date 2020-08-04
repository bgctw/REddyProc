# Convenence functions to read standard datasets
# Will reduce support work with people redoing this.

#' @export
fLoadEuroFlux16 <- function(
	### reads a sequence of annual files in the format of europe-fluxdata 2016
	siteName		##<< scalar string: the name of the site, i.e. start of the filename before _<year>_
	, dirName = ''	##<< scalar string: the directory where the files reside
	, additionalColumnNames = character(0)	##<< character vector: column names to read in addition to c("Month", "Day", "Hour", "NEE_st", "qf_NEE_st", "ustar", "Ta", 'Rg')
) {
	##author<< TW
	##details<< The filenames should correspond to the pattern <sitename>_<YYYY>_. * .txt
	## And hold columns c("Month", "Day", "Hour", "NEE_st", "qf_NEE_st", "ustar", "Ta", 'Rg').
	## By default only those columns are read and reported only
	## c("DateTime", "NEE", "Ustar", "Tair", "Rg", "qf_NEE_st" (Note the renaming).
	## NEE is set to NA for all values with "qf_NEE_st != 0.
	## Values  of -9999.0 are replaced by NA
	#
	filenamePattern <- paste(siteName, ". +\\.txt$", sep = "")
	fileNames <- dir(dirName, filenamePattern)
	fileName <- fileNames[1]
	colNames <- union(c("Month", "Day", "Hour", "NEE_st", "qf_NEE_st", "ustar", "Ta", 'Rh', 'Rg'), additionalColumnNames)
	# by settting colClasses at a given position to NULL the column is skipped
	header <- as.character(read.csv(file.path(dirName, fileName), header = FALSE, nrows = 1, stringsAsFactors = F))
	iCols <- match(colNames, header)
	if (length(iNACols <- which(is.na(iCols))) ) stop("unknown columns ", colNames[iNACols], " in file ", fileName)
	colClasses <- rep("NULL", length(header))
	colClasses[iCols] <- NA
	ds <- do.call(rbind, lapply(fileNames, function(fileName) {
					year <- as.integer(sub(". * _(\\d\\d\\d\\d)_. * ", "\\1", fileName))
					tmp <- read.csv(file.path(dirName, fileName), colClasses = colClasses)
					#doy <- as.POSIXlt(ISOdate(year, tmp$Month, tmp$Day))$yday + 1L
					tmp$Year <- year
					nRow <- nrow(tmp)
					if (tmp$Month[nRow] == 1L)
						tmp$Year[nRow] <- year + 1L
					tmp
				}))
	dsTime <- fConvertTimeToPosix(
	  ds, 'YMDH', Year = 'Year', Month = 'Month', Day = 'Day', Hour = 'Hour')
	colnames(dsTime)[match(c("NEE_st", "ustar", "Ta", "Rh"), colnames(dsTime))] <-  c("NEE", "Ustar", "Tair", "rH")
	dsTime$NEE[(dsTime$qf_NEE_st != 0)] <- NA
	ans <- dsTime[, c("DateTime", "NEE", "Ustar", "Tair", "rH", "Rg", "qf_NEE_st", additionalColumnNames)]
	ans <- fConvertGapsToNA(ans)
	ans
}


#' extract processing results with columns corresponding to Fluxnet15 release
#'
#' extract processing results with columns corresponding to Fluxnet15 release
#'
#' @param EProc sEddyProc class with uncertainty also in meteo variables and
#' both nighttime and daytime partitioning columns present
#'
#' @return data.frame with columns names of Fluxnet15. Timestamps are
#'   in ISO string format \code{\link{POSIXctToBerkeleyJulianDate}}
#' @export
extract_FN15 <- function(EProc) {
  input <- EProc$sTEMP
  replace_patterns <- tribble(
    ~pattern, ~replacement, ~flux, ~method,
    "^NEE_U(\\d\\d)_f$", "NEE_VUT_\\1", "NEE", "",
    "^GPP_U(\\d\\d)_f$", "GPP_NT_VUT_\\1", "GPP", "NT",
    "^GPP_DT_U(\\d\\d)$", "GPP_DT_VUT_\\1", "GPP", "DT",
    "^Reco_U(\\d\\d)$", "RECO_NT_VUT_\\1", "GPP", "NT",
    "^Reco_DT_U(\\d\\d)$", "RECO_DT_VUT_\\1", "GPP", "DT",
    "^Ustar_Thresh_U(\\d\\d)$", "USTAR_THRESHOLD_VUT_\\1", "", ""
  )
  # do not import stringr for dependencies
  str_replace <- function(x,pattern,replacement) gsub(pattern, replacement, x)
  replaceFun <- function(pattern, replacement,...){
    tmp <- input %>% select(matches(pattern))
    names(tmp) <- names(tmp) %>% str_replace(pattern, replacement)
    tmp
  }
  output_ustar <- replace_patterns %>% pmap(replaceFun) %>% bind_cols()
  #summary(output_ustar)
  #
  output_add <- cbind(EProc$sDATA, EProc$sTEMP[-1]) %>% select(
    NEE_VUT_USTAR50_QC = .data$NEE_U50_fqc,
    NEE_VUT_USTAR50_RANDUNC	= .data$NEE_U50_fsd,
    NEE_VUT_USTAR50_RANDUNC_N	= .data$NEE_U50_fnum,
    NIGHT	= .data$night,
    SW_IN_F_MDS	= .data$Rg_f,
    SW_IN_F_MDS_QC = .data$Rg_fqc,
    SW_IN_POT = .data$PotRad_NEW,
    TA_F_MDS = .data$Tair_f,
    TA_F_MDS_QC = .data$Tair_fqc,
    USTAR = .data$Ustar,
    VPD_F_MDS = .data$VPD_f,
    VPD_F_MDS_QC = .data$VPD_fqc,
    GPP_DT_U50_QC = .data$FP_qc		##<< quality flag: 0: good parameter fit,
    ## 1: some parameters out of range, required refit,
    ## 2: next parameter estimate is more than two weeks away

    #USTAR_THRESHOLD = .data$Ustar_uStar_Thres
  )
  #summary(output_add)
  #
  time <- EProc$sDATA$sDateTime
  timestep <- difftime(time[2],time[1], units = "hours")
  output_time <- tibble(
    TIMESTAMP_START = POSIXctToBerkeleyJulianDate(time - timestep/2),
    TIMESTAMP_END = POSIXctToBerkeleyJulianDate(time + timestep/2)
  )
  bind_cols(output_time, output_ustar, output_add)
}



