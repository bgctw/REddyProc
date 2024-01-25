#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with functions for data input and output
#+++ Dependencies: DataFunctions.R
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
help_export <- function(
  ### Overview of functions helping with exporting Data and Results
){
  ##author<< TW
  ##details<<
  ## Functions helping with exporting data
  ## \itemize{
  ## \item Export Input data from REddyProc class:
  ##   \code{\link{sEddyProc_sExportData}}
  ## \item Export Computed results from REddyProc class:
  ##   \code{\link{sEddyProc_sExportResults}}
  ## \item Write data frame to ASCII tab-separated text file:
  ##   \code{\link{fWriteDataframeToFile}}
  ## \item Write data frame to ASCII comma-separated text file with units in header row:
  ##   \code{\link{fWriteFrench23}}
  ## }
  ##
  ## Writing a file that can be supplied to the REddyProc webservice at
  ## MPI-BGC Jena can be done by exporting data from REddyProc class \code{EProc}.
  ##
  ## \preformatted{
  ## df <- EProc$sExportData()
  ## fWriteDataframeToFile(df, "myfilename.txt", isSplitDatetime = TRUE)
  ## }
  ##
  ## For preparing minimal working examples also consider
  ## \itemize{
  ## \item Omit records before the start of the first full day and the end of
  ##   the last full day:
  ##   \code{df <- \link{filter_entire_days}(df)}
  ## \item Subset data.frame to one or two years:
  ##   \code{df <- \link{filter_years_eop}(df, c(1998))}
  ## }
  ##
  ## There are several functions that import from file of different formats.
  ## \itemize{
  ## \item Load text file with one header and one unit row into data frame:
  ##   \code{\link{fLoadTXTIntoDataframe}}
  ## \item Reads sequence of annual files in the format of Europe-fluxdata 2016:
  ##   \code{\link{fLoadEuroFlux16}}
  ## \item Read basic variables from Ameriflux standard (as of 2022) files:
  ##   \code{\link{fLoadAmeriflux22}}
  ## \item Read NetCDF files -> moved to separate package REddyProcNCDF
  ##   (https://github.com/bgctw/REddyProcNCDF)
  ## }
  ##
  ## Back to \link{REddyProc-package}.
  "type ?help_export"
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Load ascii format data
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fLoadTXTIntoDataframe <- function(
  ##title<<
  ## Load text file with one header and one unit row into data frame
  ##description<<
  ## If gaps with the flag -9999.0 exist, these are set to NA.
  ##seealso<<
  ##  \code{\link{help_export}}
  FileName  = FileName.s  ##<< File name as a character string
  , Dir = if (!missing(Dir.s)) Dir.s else ''##<< Directory as a character string
  , FileName.s             ##<< deprecated
  , Dir.s = ''             ##<< deprecated way of specifying Dir
) {
  varNamesDepr <- c("FileName.s","Dir.s")
  varNamesNew <- c("FileName","Dir")
  iDepr = which(!c(missing(FileName.s),missing(Dir.s)))
  if (length(iDepr)) warning(
    "Argument names ",varNamesDepr[iDepr]," have been deprecated."
    ," Please, use instead ", varNamesNew[iDepr])

	##details<<
	## Function fLoadFluxNCIntoDataframe, which loads data from NetCDF-Files, has
	## been moved to add-on package REddyProcNCDF.
	## In addition, \code{\link{fLoadEuroFlux16}} loads data from several annual
	## files in format corresponding to Europe-fluxdata 2016.
	##details<<
	## For using only part of the records, use \code{fFilterAttr} to keep
	## units attributes.
  InputFile.s <- fSetFile(FileName, Dir, T, 'fLoadTXTIntoDataframe')
  # Read in header
  Header.V.s <- as.character(read.csv(
    InputFile.s, header = F, sep = '', dec = '.', nrows = 1, stringsAsFactors = F))
  Units.V.s <- as.character(read.csv(
    InputFile.s, header = F, sep = '', dec = '.', skip = 1, nrows = 1
    , stringsAsFactors = F))
  if (length(Header.V.s) != length(Units.V.s) ) stop(
    'fLoadTXTIntoDataframe::: Entries in header row and unit row are '
    , 'not the same length: \n'
    , length(Header.V.s), ': ', paste(Header.V.s, collapse = ' '), '\n'
    , length(Units.V.s), ': ', paste(Units.V.s, collapse = ' '))
  # Skip unit row and read in data
  Data.F <- read.csv(InputFile.s, header = F, skip = 2, sep = '', dec = '.')
  # Rename columns with header information
  names(Data.F) <- Header.V.s
  Data.F <- set_varunit_attributes(Data.F, Header.V.s, Units.V.s)
  message('Loaded file ', FileName, ' with the following variables (units):')
  message(' *** ', paste(colnames(Data.F), '(', as.character(lapply(
    Data.F, attr, which = 'units')), ')', collapse = ' ', sep = ''))
  # Convert gap flags to NA
  Data.F <- fConvertGapsToNA(Data.F)
  Data.F
  ##value<<
  ## Data frame with data from text file.
  ##examples<<
  ## \donttest{
  ## examplePath <- getExamplePath('Example_DETha98.txt', TRUE)
  ## EddyData.F <- fLoadTXTIntoDataframe(examplePath)
  ## }
}

#' Get the default units for given variables
#'
#' @param variable_names  string vector of variables to query units for
#' @return string vector with units, NA for non-standard variables.
#' @export
REddyProc_defaultunits <- function(variable_names) {
  df_all_units <-tribble(
    ~varname, ~unit,
    "NEE", "umolm-2s-1",
    "LE", "Wm-2",
    "H", "Wm-2",
    "Rg", "Wm-2",
    "Tair", "degC",
    "Tsoil", "degC",
    "Ustar", "ms-1",
    "VPD", "hPa",
    "rH", "%",
  )
  df_units <- data.frame(varname = variable_names) %>%
    left_join(df_all_units, by = "varname")
  df_units$unit
}


set_varunit_attributes <- function(df, varnames, units) {
  # Add units (and also varnames) as attribute
  if (length(units) != length(varnames)) stop(
    "Expected arguments varnames to be of same length but was ",
    length(varnames)," != ", length(units),".")
  for (i in 1:length(varnames)) {
    attr(df[[varnames[i] ]], 'varnames') <- varnames[i]
    attr(df[[varnames[i] ]], 'units') <- units[i]
  }
  df
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Write data to file
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fWriteDataframeToFile <- function(
  ##description<<
  ## Write data frame to ASCII tab-separated text file
  Data.F                ##<< Data frame
  , FileName = FileName.s           ##<< File base name as a string
  , Dir = if (!missing(Dir.s)) Dir.s else ''          ##<< Directory as a string
  , Digits = if (!missing(Digits.n)) Digits.n else 5			##<< (integer) number
  ## of digits, i.e. precision, for numeric values
  , isSplitDatetime = FALSE ##<< set to TRUE to create columns Year, DoY and Hour
  , FileName.s         ##<< deprecated
  , Dir.s              ##<< deprecated
  , Digits.n			     ##<< deprecated
) {
  ##author<<
  ## AMM, KS, TW
  ##seealso<< \code{\link{help_export}} \code{\link{fSplitDateTime}}
  ##details<<
  ## Missing values are flagged as -9999.0
  # TEST: Data.F <- EddyData.F; FileName = 'none'; Dir <- 'inst / examples';
  # Set file name
  OutputFile.s <- fSetFile(FileName, Dir, F, 'fWriteDataframeToFile')
  # Convert NAs to gap flag
  if (isTRUE(isSplitDatetime)) Data.F <- fSplitDateTime(Data.F)
  Data.F <- fConvertNAsToGap(Data.F)
  # Write tab delimited file
  Lines.V.s <- vector(mode = 'character', length = 2)
  Lines.V.s[1] <- paste(colnames(Data.F), collapse = '\t')
  #If POSIX column replace name
  Lines.V.s[1] <- gsub('DateTime', 'Date Time', Lines.V.s[1])
  Lines.V.s[2] <- paste(as.character(lapply(
    Data.F, attr, which = 'units')), collapse = '\t')
  Lines.V.s[2] <- gsub('NULL', '-', Lines.V.s[2])
  #if POSIX column replace unit
  Lines.V.s[2] <- gsub('DateTime', 'Date Time', Lines.V.s[2])
  write(Lines.V.s, file = OutputFile.s, append = F)
  write.table(format(
    Data.F, digits = Digits, drop0trailing = T, trim = T)
    , file = OutputFile.s, col.names = F, row.names = F
    , sep = '\t', quote = F, append = T)
  message('Wrote tab separated textfile: ', OutputFile.s)
  ##value<<
  ## Output of data frame written to file of specified type.
}
attr(fWriteDataframeToFile, 'ex') <- function() {
  (Dir <- tempdir())   # directory where output is written to
  fWriteDataframeToFile(Example_DETha98, 'OutputTest.txt', Dir = Dir)
}

#' @export
fSplitDateTime <- function(
  ### Replace Column DateTime by columns Year, DoY, and Hour in a data.frame
  df ##<< data.frame with column DateTime
) {
  ##seealso<< \code{\link{fWriteDataframeToFile}}
  ##details<<
  ## This function helps exporting to the format required by the
  ## REddyProc web interface with columns Year, DoY, and Hour
  df_ydh <- df %>% mutate(
    Year = as.POSIXlt(.data$DateTime)$year + 1900,
    DoY = as.POSIXlt(.data$DateTime)$yday + 1 ,
    Hour = as.POSIXlt(.data$DateTime)$hour + as.POSIXlt(.data$DateTime)$min/60
  ) %>%
    select(.data$Year, .data$DoY, .data$Hour, !.data$DateTime)
  df_ydh
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ File handling
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fInitFilesDir <- function(
  ##description<<
  ## Get all available files with specific file extension in directory
  Dir.s           ##<< Directory (string) to be searched for files
  , lFileExt.s     ##<< File extension (string) specification
  , fixed = TRUE	  ##<< set to FALSE, if using regular expressions
)
  ##author<<
  ## AMM
{
  # List files in path and grep files with specified file extension as string
  list.files(path = Dir.s)[
    grep(lFileExt.s, list.files(path = Dir.s), fixed = fixed)]
  ##value<<
  ## Character vector with names of all available site files.
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fStripFileExtension <- function(
  ##description<<
  ## Strip file extension
  lFiles.V.s     ##<< String vector with names of all available site files
)
  ##author<<
  ## AMM
{
  # RegExp: Search for first dot and replace rest of string with nothing
  sub('[.]. * ', '', lFiles.V.s)
  ##value<<
  ## Character vector containing the first part of file names
  ## (before first dot in file name).
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fSetFile <- function(
  ##description<<
  ## Set file name with path and check if directory and / or file exists
  FileName.s            ##<< File name as a string
  , Dir.s                ##<< Directory as a string
  , IO.b                 ##<< Input / output flag, TRUE for input, FALSE for output
  , CallFunction.s = ''    ##<< Name (string) of the caller function for warnings
)
  ##author<<
  ## AMM
  # TEST: Dir.s <- 'inst / examples'; FileName.s <- 'Example_DETha98.txt';
  # IO.b <- T; CallFunction.s <- 'test'
{
  # Check if string for directory provided
  Dir.b <- fCheckValString(Dir.s)

  # Check if directory exists
  if (IO.b && Dir.b && (file.access(Dir.s, mode = 4) != 0))
    stop(CallFunction.s, ':::fSetFile::: Directory does not exist: ', Dir.s)

  # Make directory if mode is output
  if ( !IO.b && Dir.b && (file.access(Dir.s, mode = 0) != 0) ) {
    dir.create(Dir.s)
    message(CallFunction.s, ':::fSetFile::: Directory created: ', Dir.s)
    if (file.access(Dir.s, mode = 2) != 0)
      stop(CallFunction.s, ':::fSetFile::: Directory could not be created: ', Dir.s)
  }

  # Set file name accordingly
  File.s <- if (Dir.b) file.path( Dir.s, FileName.s ) else FileName.s

  # If input file, check if file exists
  if (IO.b && (file.access(File.s, mode = 4) != 0) ) stop(
    CallFunction.s
    , ':::fSetFile::: File does not exist or has no read permission: ', File.s)

  File.s
  ##value<<
  ## Returns name of file with complete path.
}
