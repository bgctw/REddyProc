# sEddyProc.example moved to different vignettes
# see vignettes/*.Rmd

##+++++++++++++++++  Downloading and caching example files ++++++++++++++++++
#' @export
getExamplePath <- function(
    ### checks if example filename is existing and if not tries to download it.
    filename = "Example_DETha98.txt"  ##<< the name of the example file
    , isTryDownload = FALSE            ##<< scalar logical whether to try
         ## downloading the file to package or tmp directory.
        ## Because of CRAN checks, need to explicitly set to TRUE
    , exampleDir = getREddyProcExampleDir()    ##<< directory where examples are
        ## looked up and downloaded to
    , remoteDir = ""                   ##<< the URL do download from
) {
  ##details<<
  ## Example input text data files are not distributed with the package, because
  ## it exceeds allowed package size.
  ## Rather, the example files will be downloaded when required from github
  ## by this function.
  ##
  ## The remoteDir (github) must be reachable, and the writing directory
  ## must be writeable.
  # set default remoteDir inside function instead of argument default,
  # because it screws function signature
  if (!nzchar(remoteDir) ) remoteDir <-
      "https://raw.githubusercontent.com/bgctw/REddyProc/master/examples"
  fullname <- file.path(exampleDir, filename)
  if (file.exists(fullname) ) return(fullname)
  if (isTRUE(isTryDownload) ) {
    if (file.access(exampleDir, mode = 2) != 0) stop(
      "target example directory ", exampleDir, " is not writeable.")
    url <- file.path(remoteDir, filename)
    retCode <- suppressWarnings(try(download.file(url, fullname, quiet = TRUE)
                                    , silent = TRUE))
    if (!inherits(retCode, "try-error") && retCode == 0) return(fullname)
  }
  ##value<< the full path name to the example data or if not available
  ##an zero-length character.
  ## Allows to check for \code{if (length(getExamplePath()) ) ... }
  return(character(0) )
}

#' @export
getREddyProcExampleDir <- function(
  ### get the example directory inside temporary directory
  isPreferParentDir =        ##<< logical scalar, whether to prefer
    ## temp parent directory instead of the R-session temp-Directory.
    ## See details.
      identical(Sys.getenv("NOT_CRAN"), "true")
  , subDir = 'REddyProcExamples'  ##<< the name of the subdirectory inside the
    ## tmp directory, where examples are stored
) {
  ##seealso<< \code{\link{getExamplePath}}
  ##details<<
  ## If \code{isPreferParentDir = FALSE} (the default),
  ## the examples will be downloaded again for
  ## each new R-session in a session specific directory as given by
  ## \code{\link{tempdir}}. This corresponds to CRAN policy.
  ## IF TRUE, the parent of \code{\link{tempdir}} will be used, so that
  ## downloads of examples are preserved across R-sessions.
  ## This is the default if
  ##  environment variable "NOT_CRAN" is defined,
  ##   when running from testthat::\code{\link{check}}.
  tmpDir <- tempdir()
  if (!dir.exists(tmpDir) ) dir.create(tmpDir)
  # dirname on a directory returns the parent directory
  if (isPreferParentDir) tmpDir <- dirname(tmpDir)
  # If the directory inside packageDir is not yet existing, create it
  exampleDir <- file.path(tmpDir, subDir)
  if (!dir.exists(exampleDir) ) dir.create(exampleDir)
  exampleDir
}

.tmp.f <- function() {
  # do not put to example, because it creates dir in /tmp
  # R session specific
  getREddyProcExampleDir()
  # outside R-session specific
  getREddyProcExampleDir(TRUE)
}

#' @export
getFilledExampleDETha98Data <- function(
  ### Get or create the gapfilled version of the Example_DETha98 example data
  exampleDir = getREddyProcExampleDir()  ##<< the directory where the
    ## cached filled example data is stored
) {
  exampleBaseName <- "Example_DETha98_Filled.RData"
  examplePath <- getExamplePath(exampleBaseName, exampleDir = exampleDir)
  if (!length(examplePath) ) {
    # Example_DETha98 is a lazyData object of REddyProc
    # nee to prefix package name here, to satisfy R CMD CHECK
    Example_DETha98_Date <- fConvertTimeToPosix(REddyProc::Example_DETha98
                    , 'YDH', Year.s = 'Year', Day.s = 'DoY', Hour.s = 'Hour')
    Example_DETha98_sDate <- cbind(
      sDateTime = Example_DETha98_Date$DateTime - 15 * 60,  Example_DETha98_Date)
    EProc <- sEddyProc$new('DE-Tha', Example_DETha98_sDate
                                , c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
    EProc$sSetLocationInfo(Lat_deg.n = 51.0, Long_deg.n = 13.6, TimeZone_h.n = 1)
    EProc$sCalcPotRadiation()
    EProc$sMDSGapFill('NEE', FillAll.b = TRUE)
    EProc$sMDSGapFill('Rg', FillAll.b = FALSE)
    EProc$sMDSGapFill('Tair', FillAll.b = FALSE)
    EProc$sMDSGapFill('VPD', FillAll.b = FALSE)
    Example_DETha98_Filled <- cbind(Example_DETha98_sDate, EProc$sExportResults() )
    save(Example_DETha98_Filled, file = file.path(exampleDir, exampleBaseName))
    examplePath <- getExamplePath(exampleBaseName)
  }
  ##value<< example data.frame Example_DETha98 processed by gapfilling.
  ans <- local({load(examplePath); get(ls()[1])})
}

