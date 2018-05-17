#' @export
renameVariablesInDataframe <- function(
  ### Rename the column names of a data.frame according to a given mapping
  data.F                    ##<< data.frame whose columns should be renamed
  , mapping =               ##<< named character vector:
    ## specifying a renaming (name -> value)
    ## of the variables, see e.g.
    ## \code{\link{getAmerifluxToBGC05VariableNameMapping}}
    getBGC05ToAmerifluxVariableNameMapping()
) {
  ##author<< TW,
  if (length(names(mapping)) ) {
    iTarget <- match(colnames(data.F), names(mapping)  )
    iMatch <- which(!is.na(iTarget))
    colnames(data.F)[iMatch] <- mapping[iTarget[iMatch] ]
  }
  data.F
}

#' @export
getBGC05ToAmerifluxVariableNameMapping <- function(
  ### map REddyProc names the Berkeley 2016 release of the Fluxnet data
  map = character()  ##<< named character vector: additional mapping,
    ## that extends or overwrites defaults in \code{mapDefault}
  , mapDefault = c(  ##<< named character vector: default mapping
      Year = "YEAR"
      , DoY = 'DOY'
      , Rg = 'SW_IN'
      , Tair = 'TA'
      , Tsoil = 'TS'
      , rH = 'RH'
      , VPD = 'VPD'
      , Ustar = 'USTAR'
      , NEE_orig = 'NEE_PI'
      , H_orig = 'H_PI'
      , LE_orig = 'LE_PI'
      , NEE_f = 'NEE_F'
      , H_f = 'H_F'
      , LE_f = 'LE_F'
      , NEE_fqc = 'NEE_QC'
      , H_fqc = 'H_QC'
      , LE_fqc = 'LE_QC'
  )
) {
  ##author<< TW,
  ##details<< Get a mapping of variable names of REddyProc defaults to names
  ##of the Berkeley 2016 release of the Fluxnet data
  ##seealso<< \code{\link{renameVariablesInDataframe}}
  mapDefault[names(map) ] <- map
  mapDefault
}
attr(getBGC05ToAmerifluxVariableNameMapping, "ex") <- function() {
  # adding mapping of foo, and overwriting mapping of DoY
  getBGC05ToAmerifluxVariableNameMapping(c(foo = "FOO", DoY = "doy"))
}

#' @export
getAmerifluxToBGC05VariableNameMapping <- function(
  ### map Ameriflux variable names to REddyProc defaults to names
  map = character()  ##<< named character vector: additional mapping,
    ##that extends or overwrites defaults in \code{mapDefault}
  , mapDefault = c(  ##<< named character vector: default mapping
    YEAR = 'Year'
    , DOY = 'DoY'
    , NEE = 'NEE'
    , LE = 'LE'
    , H = 'H'
    , SW_IN = 'Rg'
    , TA = 'Tair'
    , TS = 'Tsoil'
    , RH = 'rH'
    , VPD = 'VPD'
    , USTAR = 'Ustar'
    , NEE_PI = 'NEE_orig'
    , H_PI = 'H_orig'
    , LE_PI = 'LE_orig'
    , NEE_F = 'NEE_f'
    , H_F = 'H_f'
    , LE_F = 'LE_f'
    , NEE_QC = 'NEE_fqc'
    , H_QC = 'H_fqc'
    , LE_QC = 'LE_fqc'
  )
) {
  ##author<< TW,
  ##details<< Get a mapping of variable names of Ameriflux
  ## (Berkley 2016 Fluxnet release)
  ## to of REddyProc defaults to names
  ##seealso<< \code{\link{renameVariablesInDataframe}}
  mapDefault[names(map) ] <- map
  mapDefault
}

#' @export
POSIXctToBerkeleyJulianDate <- function(
    ### convert POSIXct to JulianDate format used in Berkeley release
    sDateTime  ##<< POSIXct vector
) {
  ##author<< TW,
  ##seealso<< \code{\link{BerkeleyJulianDateToPOSIXct}}
  ##details<<
  ## In the Berkeley-Release of the fluxnet data, the time is stored as an number
  ## with base10-digits representing YYYYMMddhhmm
  tz <- getTZone(sDateTime)
  charRep <- strftime(sDateTime, format = "%Y%m%d%H%M", tz = tz)
  ans <- as.numeric(charRep)
  ans
}

#' @export
BerkeleyJulianDateToPOSIXct <- function(
  ### convert JulianDate format used in Berkeley release to POSIXct
  julianDate  ##<< numeric vector representing times (see details for format)
  , tz = "GMT"  ##<< time zone used to represent the dates
  , ...    ##<< further arguments to \code{\link{strptime}}, such as tz
) {
  ##author<< TW,
  ##seealso<< \code{\link{POSIXctToBerkeleyJulianDate}}
  ##details<<
  ## In the Berkeley-Release of the fluxnet data, the time is stored as an number
  ## with base10-digits representing YYYYMMddhhmm
  ans <- as.POSIXct(strptime(as.character(julianDate), "%Y%m%d%H%M", tz = tz, ...))
  ans
}


.tmp.f <- function() {
  bla.new <- strptime(as.character(new_ds$TIMESTAMP_START), "%Y%m%d%H%M")
  zzz <- strftime(bla.new, format = "%Y-%m-%d %H:%M:%S")
  new_ds$COMMTIME <- zzz
  new_ds_sub_9607 <- new_ds[
    c(which(new_ds$COMMTIME == "1996-01-01 00:00:00"):which(
      new_ds$COMMTIME == "2013-12-31 23:30:00")), ]
}

