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
  ##seealso<< \code{\link{help_export}}
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
	# by setting colClasses at a given position to NULL the column is skipped
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


#' Read a file in the format of Fluxnet 2015 release
#'
#' Assigns default units to the columns and keeps variable name attributes
#' as in original file.
#'
#' @param file_path scalar string: the path to the csv file
#' @param additional_columns character vector of columns to
#'   read in addition of standard columns of \code{\link{read_from_fluxnet15}}.
#'   Can be a character vector or a object return by \code{\link{cols}}
#' @param colname_NEE name (scalar string) of column that reports NEE observations
#' @param ... further arguments to \code{\link{read_csv}}
#'
#' @examples
#'   ds_fn15 <- Example_DETha98 %>%
#'      fConvertTimeToPosix('YDH',Year = 'Year',Day = 'DoY', Hour = 'Hour') %>%
#'      dplyr::mutate(
#'         TIMESTAMP_END = POSIXctToBerkeleyJulianDate(DateTime),
#'         season = factor(199801)
#'      ) %>%
#'      dplyr::rename(SW_IN = "Rg", TA = "Tair", USTAR = "Ustar") %>%
#'      dplyr::select(dplyr::one_of(c(
#'        "TIMESTAMP_END","NEE","SW_IN","TA","VPD","USTAR","season")))
#'   head(ds_fn15)
#'   fname <- tempfile()
#'   readr::write_csv(ds_fn15, fname)
#'
#'   # standard columns are renamed to REddyProc defaults
#'   ds_eproc <- fLoadFluxnet15(fname)
#'   head(ds_eproc)
#'   EProc <- sEddyProc$new("DE-Tha", ds_eproc)
#'   head(EProc$sExportData())
#'
#'   # Additional columns can be specified, e.g. factor column season
#'   ds_eproc <- fLoadFluxnet15(fname,
#'     additional_columns = readr::cols(season = readr::col_factor()))
#'   head(ds_eproc)
#'   EProc <- sEddyProc$new("DE-Tha", ds_eproc,
#'     c("NEE", "Rg", "Tair", "VPD", "Ustar","season"),
#'     ColNamesNonNumeric = "season"
#'     )
#'   head(EProc$sExportData())
#' @export
fLoadFluxnet15 <- function(file_path, additional_columns = character(0),
                           colname_NEE = "NEE", ...) {
  col <- cols_only(
    TIMESTAMP_END = col_character(),
    NEE = col_double(),
    LE = col_double(),
    H = col_double(),
    SW_IN = col_double(),
    TA = col_double(),
    TS = col_double(),
    USTAR = col_double(),
    VPD = col_double()
  )
  names(col$cols)[names(col$cols) == "NEE"] <- colname_NEE
  df_units <- tribble(
    ~varname, ~unit,
    colname_NEE, "umolm-2s-1",
    "LE", "Wm-2",
    "H", "Wm-2",
    "SW_IN", "Wm-2",
    "TA", "degC",
    "TS", "degC",
    "USTAR", "ms-1",
    "VPD", "hPa",
  )
  colsInFile <- read_lines(file_path, n_max = 1L) %>% strsplit(",") %>%  "[["(1)
  col$cols <- col$cols[names(col$cols) %in% colsInFile]
  if (length(additional_columns)) {
    col_add <- if (inherits(additional_columns,"col_spec")) {
      additional_columns
    } else {
      col_add <- cols(rep(col_guess(),length(additional_columns)))
      names(col_add$cols) <- additional_columns
      col_add
    }
    col$cols <- c(col$cols, col_add$cols)
  }
  # df_fn15 <- read_csv(file_path, ...)
  # df_fn15 <- read_csv(file_path, col_types = col_standard, ...)
  df_fn15 <- read_csv(file_path, col_types = col, ...)
  df_unitsin <- df_units %>% filter(.data$varname %in% names(df_fn15))
  df_fn15 <- df_fn15 %>% as.data.frame() %>%
    set_varunit_attributes(df_unitsin$varname, df_unitsin$unit)
  read_from_fluxnet15(df_fn15, colname_NEE = colname_NEE)
}

.tmp.f <- function(){
  ds <- Example_DETha98 %>%
    fConvertTimeToPosix('YDH',Year = 'Year',Day = 'DoY', Hour = 'Hour') %>%
    rename(SW_IN = .data$Rg, TA = .data$Tair, USTAR = .data$Ustar) %>%
    mutate(TIMESTAMP_END = POSIXctToBerkeleyJulianDate(.data$DateTime)) %>%
    select(one_of(c("TIMESTAMP_END","NEE","SW_IN","TA","VPD","USTAR")))
  fname <- tempfile()
  write_csv(ds, fname)
  ds_fn15 <- fLoadFluxnet15(fname)
  head(ds_fn15)
  EProc <- sEddyProc$new("DE-Tha", ds_fn15)
  head(EProc$sExportData())
}

#' extract REddyProc input columns from data.frame in Fluxnet15 format
#'
#' Column format as described at
#' https://fluxnet.org/data/fluxnet2015-dataset/fullset-data-product/
#'
#' @details If input has numeric column USTAR_QC then USTAR of records
#' with USTAR_QC > 2 are set to NA.
#'
#' @param ds data.frame with columns TIMESTAMP_END (Time YYYYMMDDHHMM),
#' NEE, LE, H, USTAR, TA, TS, VPD, SW_IN and optionally USTAR_QC
#' @param colname_NEE name (scalar string) of column that reports NEE observations
#' @return data.frame with additional columns 'DateTime', 'NEE','Ustar' and
#'   'Rg','Tair','Tsoil' if columns 'SW_IN','TA', or 'TS' are present respectively
#' @export
read_from_fluxnet15 <- function(ds, colname_NEE = "NEE"){
  ustar_qc <- if (
    ("USTAR_QC" %in% names(ds)) && is.numeric(ds$USTAR_QC)) ds$USTAR_QC else
      rep(1L, nrow(ds))
  ds_eproc <- ds %>% mutate(
    DateTime = BerkeleyJulianDateToPOSIXct(.data$TIMESTAMP_END),
    NEE = .data[[colname_NEE]],
    Ustar = ifelse(ustar_qc <= 2L, .data$USTAR, NA_real_)
  )

  if ("TA" %in% names(ds_eproc)) ds_eproc <- ds_eproc %>% mutate( Tair = .data$TA)
  if ("TS" %in% names(ds_eproc)) ds_eproc <- ds_eproc %>% mutate( Tsoil = .data$TS)
  if ("SW_IN" %in% names(ds_eproc)) ds_eproc <- ds_eproc %>% mutate( Rg = .data$SW_IN)
  ds_eproc
}



#' extract processing results with columns corresponding to Fluxnet15 release
#'
#' extract processing results with columns corresponding to Fluxnet15 release
#'
#' @param EProc sEddyProc class with uncertainty also in meteo variables and
#' both nighttime and daytime partitioning columns present
#' @param keep_other_cols set to TRUE to report also other columns
#' @param is_export_nonfilled set to FALSE to not export columns before gapfilling
#'
#' @return data.frame with columns names of Fluxnet15. Timestamps are
#'   in ISO string format \code{\link{POSIXctToBerkeleyJulianDate}}
#' @export
extract_FN15 <- function(EProc = .self, is_export_nonfilled = TRUE, keep_other_cols = FALSE) {
  input <- bind_cols(EProc$sDATA, select(EProc$sTEMP, -1L))
  time <- EProc$sDATA$sDateTime
  timestep <- difftime(time[2],time[1], units = "hours")
  output_time <- tibble(
    TIMESTAMP_START = POSIXctToBerkeleyJulianDate(time - timestep/2),
    TIMESTAMP_END = POSIXctToBerkeleyJulianDate(time + timestep/2)
  )
  # do not import stringr for dependencies
  str_replace <- function(x,pattern,replacement) gsub(pattern, replacement, x)
  replaceFun <- function(pattern, replacement,...){
    tmp <- input %>% select(matches(pattern))
    names(tmp) <- names(tmp) %>% str_replace(pattern, replacement)
    tmp
  }
  replace_patterns_uStar <- tribble(
    ~pattern, ~replacement, ~variable, ~method,
    "^NEE_U(\\d\\d)_f$", "NEE_VUT_\\1", "NEE", "",
    "^GPP_U(\\d\\d)_f$", "GPP_NT_VUT_\\1", "GPP", "NT",
    "^GPP_DT_U(\\d\\d)$", "GPP_DT_VUT_\\1", "GPP", "DT",
    "^Reco_U(\\d\\d)$", "RECO_NT_VUT_\\1", "GPP", "NT",
    "^Reco_DT_U(\\d\\d)$", "RECO_DT_VUT_\\1", "GPP", "DT",
    "^Ustar_Thresh_U(\\d\\d)$", "USTAR_THRESHOLD_VUT_\\1", "", "",
    "^NEE_U(\\d\\d)_fqc$", "NEE_VUT_USTAR\\1_QC", "NEE", "",
    # the following extract only one column
    # by putting them as a pattern it works also if the column does not exist
    #better add fqc for all ustar "^NEE_U50_fqc$", "NEE_VUT_USTAR50_QC", "NEE", "",
    "^NEE_U50_fsd$", "NEE_VUT_USTAR50_RANDUNC", "NEE", "",
    "^NEE_U50_fnum$", "NEE_VUT_USTAR50_RANDUNC_N", "NEE", "",
    "^FP_qc$", "GPP_DT_U50_QC", "GPP", ""		##<< quality flag: 0: good parameter fit,
    ## 1: some parameters out of range, required refit,
    ## 2: next parameter estimate is more than two weeks away
  )
  output_ustar <- replace_patterns_uStar %>% pmap(replaceFun) %>% bind_cols()
  if (!ncol(output_ustar)) output_ustar <- NULL
  replace_patterns_filled <- tribble(
    ~pattern, ~replacement, ~variable, ~method,
    "^night$", "NIGHT", "", "",
    "^Rg_f$", "SW_IN_F_MDS", "Rg", "",
    "^Rg_fqc$", "SW_IN_F_MDS_QC", "Rg", "",
    "^PotRad_NEW$", "SW_IN_POT", "Rg", "",
    "^Tair_f$", "TA_F_MDS", "", "",
    "^Tair_fqc$", "TA_F_MDS_QC", "", "",
    "^VPD_f$", "VPD_F_MDS", "", "",
    "^VPD_fqc$", "VPD_F_MDS_QC", "", ""
  )
  output_filled <- replace_patterns_filled %>% pmap(replaceFun) %>% bind_cols()
  if (!ncol(output_filled)) output_filled <- NULL
  replace_patterns_orig <- tribble(
    ~pattern, ~replacement, ~variable, ~method,
    "^NEE$", "NEE_ORIG", "NEE", "",
    "^Rg$", "SW_IN", "Rg", "",
    "^Tair$", "TA", "", "Tair",
    "^Ustar$", "USTAR", "uStar", "",
    "^VPD$", "VPD", "VPD", ""
  )
  output_orig <- if (isTRUE(is_export_nonfilled)){
    replace_patterns_orig %>% pmap(replaceFun) %>% bind_cols()
  } else NULL
  if (!ncol(output_orig)) output_orig <- NULL
  #
  output <- bind_cols(output_time, output_orig, output_filled, output_ustar)
  if (isTRUE(keep_other_cols)) output <- bind_cols(
    output, input[,setdiff(names(input),names(output)),drop = FALSE])
  output
}

#' Read basic variables from Ameriflux standard (as of 2022) files
#'
#' Reads Variables from file into data.frame from file and passes
#' it to \code{\link{read_from_ameriflux22}}.
#'
#' @param file_path scalar string: the path to the csv file
#' @param ... further arguments to \code{\link{read_csv}}
#'
#' @seealso \code{\link{read_from_ameriflux22}} \code{\link{help_export}}
#'
#' @export
fLoadAmeriflux22 <- function(file_path, ...) {
  col <- col_standard <- cols_only(
    TIMESTAMP_END = col_character(),
    FC = col_double(),
    LE = col_double(),
    H = col_double(),
    SW_IN = col_double(),
    TA = col_double(),
    #TS = col_double(),
    USTAR = col_double(),
    #VPD = col_double()
    RH = col_double()
  )
  df <- read_csv(file_path, col_types = col, na =  c("-9999","","NA"), comment="#", ...)
  read_from_ameriflux22(df)
}

#' Extract basic variables from Ameriflux standard (as of 2022) data.frames
#'
#' NEE is read from FC, Rg from SW_in, VPD is computed from RH and Tair.
#' Non-storage corrected LE and H are read.
#'
#' @param df data.frame: with columns FC, SW_IN, RH, TA, USTAR, L and E
#'
#' @return Data.Frame with columns
#'   DateTime, NEE,	Rg,	Tair,	rH,	VPD, Ustar, LE, H
#'
#' @export
read_from_ameriflux22 <- function(df){
  ds_eproc <- df %>% mutate(
    DateTime = BerkeleyJulianDateToPOSIXct(.data$TIMESTAMP_END),
    RH = ifelse(between(.data$RH,100.0,105.0),100.0, .data$RH),
    VPD = fCalcVPDfromRHandTair(.data$RH, .data$TA)
  ) %>%
    select("DateTime", NEE = "FC",	Rg = "SW_IN",	Tair="TA",	rH="RH",
           .data$VPD, Ustar = "USTAR", .data$LE, .data$H )
  varnames = names(ds_eproc)[-1] # all except DateTime
  units = REddyProc_defaultunits(varnames)
  ds_eproc <- ds_eproc %>%
    set_varunit_attributes(varnames, units)
}

#' @export
fWriteFrench23 <- function(
    ##description<<
  ## Write data frame to ASCII comma-separated text file
  data                ##<< Data frame to be exported, with unit attributes attached to columns
  , filename          ##<< (string)  name (including path) of the output file
  , isSplitDatetime = FALSE ##<< set to TRUE to create columns Year, DoY and Hour
  , digits = 5		  	##<< (integer) number of digits, i.e. precision, for numeric values
) {
  ##author<< TW
  ##seealso<< \code{\link{fWriteDataframeToFile}}
  ##details<<
  ## Writes data.frame as comma-separated file after two header rows.
  ##
  ## The first header row contains the column names, and the second units.
  ##
  ## Spaces in column names are replaced by underscore and % is replaced by
  ## the word percent.
  if (isTRUE(isSplitDatetime)) data <- fSplitDateTime(data)
  data <- fConvertNAsToGap(data)
  # Write header
  header <- vector(mode = 'character', length = 2)
  header[1] <- paste(colnames(data), collapse = ',')
  header[2] <- paste(as.character(lapply(
    data, attr, which = 'units')), collapse = ',')
  header <- gsub(' ', '_', header)
  header[2] <- gsub('NULL', '-', header[2])
  header[2] <- gsub('%', 'percent', header[2])
  write(header, file = filename, append = F)
  write_csv(
    format(data, digits = digits, drop0trailing = T, trim = T),
    filename, col_names=TRUE, append = TRUE)
  message('Wrote output in French23 format to textfile: ', filename)
}
attr(fWriteFrench23, 'ex') <- function() {
}


