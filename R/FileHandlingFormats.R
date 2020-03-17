# Convenence functions to read standard datasets
# Will reduce support work with people redoing this.

#' @export
fLoadEuroFlux16 <- function(
	### reads a sequence of annual files in the format of Europe-fluxdata 2016
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
