#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with geo functions to calculate latent variables or convert units
#+++ Dependencies: fCheckOutsideRange() in DataFunctions.R
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit conversions
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fConvertCtoK <- function(
  ##description<<
  ## Convert degree Celsius to degree Kelvin
  Celsius = Celsius.V.n      ##<< Data vector in Celsius (degC)
  , Celsius.V.n            ##<< deprecated way of specifying Celsius
) {
  ##author<< AMM
  if (!missing(Celsius.V.n)) warning(
    "fConvertCtoK: argument name Celsius.V.n is deprecated, use instead Celsius.")
  fCheckOutsideRange(
    data.frame(Celsius = Celsius), 'Celsius', c('<', -273.15), 'fConvertCtoK')
  Kelvin <-  Celsius + 273.15
  attr(Kelvin, 'varnames') <- 'Temp_K'
  attr(Kelvin, 'units') <- 'degK'
  return(Kelvin)
  ##value<<
  ## Data vector in temperature Kelvin (Temp_K, degK)
}

#' @export
fConvertKtoC <- function(
  ##description<<
  ## Convert degree Kelvin to degree Celsius
  Kelvin = Kelvin.V.n            ##<< Data vector in Kelvin (degK)
  , Kelvin.V.n            ##<< deprecated, use Kelvin instead
  ##author<< AMM
) {
  if (!missing(Kelvin.V.n)) warning(
    "fConvertKtoC: argument name Kelvin.V.n is deprecated, use instead Kelvin.")
  fCheckOutsideRange(cbind(Kelvin = Kelvin), 'Kelvin', c('<', 0), 'fConvertKtoC')
  Celsius <-  Kelvin - 273.15
  attr(Celsius, 'varnames') <- 'Temp_C'
  attr(Celsius, 'units') <- 'degC'
  return(Celsius)
  ##value<<
  ## Data vector in temperature Celsius (Temp_C, degC)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fConvertVisibleWm2toPhotons <- function(
  ##description<<
  ## Convert units of visible radiation from irradiance to photons flux
  Wm2 = Wm2.V.n               ##<< Data vector in units of irradiance (W m-2)
  , Wm2.V.n               ##<< deprecated
) {
  if (!missing(Wm2.V.n)) warning(
    "fConvertVisibleWm2toPhotons: argument name Wm2.V.n is deprecated, use instead Wm2.")
  # With Planck's equation at 550 nm wavelength (see also FormelBackup.doc and
  # NormanWeiss1985 paper)
  Photons.V.n <- 4.6 * Wm2
  attr(Photons.V.n, 'varnames') <- 'PPFD'
  attr(Photons.V.n, 'units') <- 'umol_m-2_s-1'
  return(Photons.V.n)
  ##value<<
  ## Data vector in units of photons flux (PPFD, umol photons m-2 s-1)
}

#' @export
fConvertGlobalToVisible <- function(
  ##description<<
  ## Partition global (solar) radiation into only visible (the rest is UV and infrared)
  Global = Global.V.n            ##<< Data vector of global radiation (W m-2)
  , Global.V.n            ##<< deprecated
  ##author<< AMM
) {
  if (!missing(Global.V.n)) warning(
    "fConvertGlobalToVisible: argument name Global.V.n is deprecated, use instead Global")
  # Ratio of visible to total solar radiation, see Weiss-Norman 1985 paper
  Vis.V.n <- 0.5 * Global
  attr(Vis.V.n, 'varnames') <- 'VisRad'
  attr(Vis.V.n, 'units') <- 'W_m-2'
  return(Vis.V.n)
  ##value<<
  ## Data vector of visible part of solar radiation (VisRad, W m-2)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Latent variable calculations
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fCalcVPDfromRHandTair <- function(
  ### Calculate VPD from rH and Tair
  rH = RH.V.n         ##<< Data vector of relative humidity (rH, %)
  , Tair = Tair.V.n   ##<< Data vector of air temperature (Tair, degC)
  , RH.V.n                ##<< deprecated
  , Tair.V.n              ##<< deprecated
  ##author<< AMM
) {
  varNamesDepr <- c("RH.V.n","Tair.V.n")
  varNamesNew <- c("rh","Tair")
  iDepr = which(!c(missing(RH.V.n),missing(Tair.V.n)))
  if (length(iDepr)) warning(
    "Argument names ",varNamesDepr[iDepr]," have been deprecated."
    ," Please, use instead ", varNamesNew[iDepr])
  fCheckOutsideRange(
    data.frame(RelHumidity_Percent = rH), 'RelHumidity_Percent'
    , c('<', 0, '|', '>', 100), 'fCalcVPDfromRHandTair')
  fCheckOutsideRange(
    data.frame(AirTemp_degC = Tair), 'AirTemp_degC'
    , c('<', -70, '|', '>', 60), 'fCalcVPDfromRHandTair')
  # See Kolle Logger Tools Software 2012 (Magnus coefficients for
  # water between 0 and 100 degC)
  VPD.V.n <- 6.1078 * (1 - rH / 100) * exp(17.08085 * Tair / (234.175 + Tair))
  attr(VPD.V.n, 'varnames') <- 'VPD'
  attr(VPD.V.n, 'units') <- 'hPa'
  return(VPD.V.n)
  ##value<<
  ## Data vector of vapour pressure deficit (VPD, hPa (mbar))
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fCalcSVPfromTair <- function(
  ##description<<
  ## Calculate SVP (of water) from Tair
  Tair = Tair.V.n  ##<< Data vector of air temperature (Tair, degC)
  , Tair.V.n       ##<< deprecated
  ##author<< AMM
) {
  if (!missing(Tair.V.n)) warning(
    "fCalcSVPfromTair: argument name Tair.V.n is deprecated, use instead Tair.")
  # Approximation formula after CANVEG (Berkeley) code
  TZero.c <- 273.15  #Absolute zero
  SVP.V.n <- exp(54.8781919 - 6790.4985 / (Tair + TZero.c)
                 - 5.02808 * log(Tair + TZero.c))
  attr(SVP.V.n, 'varnames') <- 'SVP'
  attr(SVP.V.n, 'units') <- 'hPa'
  return(SVP.V.n)
  ##value<<
  ## Data vector of saturation vapor pressure (SVP, hPa (mbar))
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fCalcAVPfromVMFandPress <- function(
  ##description<<
  ## Calculate AVP from VMF and Press
  VMF = VMF.V.n           ##<< Vapor mole fraction (VMF, mol / mol)
  , Press = Press.V.n     ##<< Atmospheric pressure (Press, hPa)
  , VMF.V.n               ##<< deprecated
  , Press.V.n             ##<< deprecated
  ##author<< AMM
) {
  varNamesDepr <- c("VMF.V.n","Press.V.n")
  varNamesNew <- c("VMF","Press")
  iDepr = which(!c(missing(VMF.V.n),missing(Press.V.n)))
  if (length(iDepr)) warning(
    "Argument names ",varNamesDepr[iDepr]," have been deprecated."
    ," Please, use instead ", varNamesNew[iDepr])
  # Calculation of actual vapor pressure, also called vapor partial pressure
  AVP <- (VMF) * Press
  attr(AVP, 'varnames') <- 'AVP'
  attr(AVP, 'units') <- 'hPa'
  return(AVP)
  ##value<<
  ## Data vector of actual vapor pressure (AVP, hPa (mbar))
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fCalcRHfromAVPandTair <- function(
  ##description<<
  ## Calculate relative humidity from actual vapour pressure and air temperature
  AVP = AVP.V.n               ##<< Data vector of actual vapour pressure (AVP, hPa (mbar))
  , Tair = Tair.V.n             ##<< Data vector of air temperature (Tair, degC)
  , AVP.V.n               ##<< Data vector of actual vapour pressure (AVP, hPa (mbar))
  , Tair.V.n             ##<< Data vector of air temperature (Tair, degC)
  ##author<< AMM
) {
  varNamesDepr <- c("AVP.V.n","Tair.V.n")
  varNamesNew <- c("AVP","Tair")
  iDepr = which(!c(missing(AVP.V.n),missing(Tair.V.n)))
  if (length(iDepr)) warning(
    "Argument names ",varNamesDepr[iDepr]," have been deprecated."
    ," Please, use instead ", varNamesNew[iDepr])
  # Definition of relative humidity (ratio of AVP to SVP)
  SVP.V.n <- fCalcSVPfromTair(Tair)
  rH <- AVP / SVP.V.n * 100
  # Restrict to physically plausible range
  rH <- ifelse(rH >= 0, rH, 0)
  rH <- ifelse(rH <= 100, rH, 100)
  attr(rH, 'varnames') <- 'rH'
  attr(rH, 'units') <- '%'
  return(rH)
  ##value<<
  ## Data vector of relative humidity (rH, %)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fCalcETfromLE <- function(
  ##description<<
  ## Calculate ET from LE and Tair
  LE = LE.V.n             ##<< Data vector of latent heat (LE, W m-2)
  , Tair = Tair.V.n       ##<< Data vector of air temperature (Tair, degC)
  , LE.V.n                ##<< deprecated
  , Tair.V.n              ##<< deprecated
  ##author<< AMM
) {
  varNamesDepr <- c("LE.V.n","Tair.V.n")
  varNamesNew <- c("LE","Tair")
  iDepr = which(!c(missing(LE.V.n),missing(Tair.V.n)))
  if (length(iDepr)) warning(
    "Argument names ",varNamesDepr[iDepr]," have been deprecated."
    ," Please, use instead ", varNamesNew[iDepr])
  # Calculated from the water density and latent heat of vaporation,
  # see Moffat manuscript on WUE
  ET.V.n <- LE / ((2.501 - 0.00236 * Tair) * 18.016)
  attr(ET.V.n, 'varnames') <- 'ET'
  attr(ET.V.n, 'units') <- 'mmol_m-2_s-1'
  return(ET.V.n)
  ##value<<
  ## Data vector of evapotranspiration (ET, mmol H20 m-2 s-1)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fLloydTaylor <- function(
  ##title<<
  ## Temperature dependence of soil respiration
  ##description<<
  ## Temperature dependence of soil respiration after Equation 11 in Lloyd & Taylor (1994)
  RRef = R_ref.n          ##<< Respiration rate at reference temperature
  , E0 = E_0.n            ##<< Temperature sensitivity ("activation energy")
  ## in Kelvin (degK)
  , TSoil = Tsoil.n       ##<< Soil temperature in Kelvin (degK)
  , TRef = if (missing(T_ref.n)) 273.15 + 10 else T_ref.n ##<< Reference
  ## temperature of 10 degC in Kelvin (degK)
  , T0 = if (missing(T_0.n)) 227.13 else T_0.n        ##<< Regression
  ## temperature as fitted by LloydTaylor (1994) in Kelvin (degK)
  , R_ref.n    ##<< deprecated way to specify RRef
  , E_0.n      ##<< deprecated way to specify E0
  , Tsoil.n    ##<< deprecated way to specify Tsoil
  , T_ref.n    ##<< deprecated way to specify TRef
  , T_0.n      ##<< deprecated way to specify T0
  ##author<<
  ## AMM
  ##reference<<
  ## Lloyd J, Taylor JA (1994) On the temperature dependence of soil respiration.
  ## Functional Ecology, 8, 315-323.
) {
  varNamesDepr <- c(
    "R_ref.n","E_0.n","Tsoil.n","T_ref.n","T_0.n")
  varNamesNew <- c(
    "RRef","E0","TSoil","TRef","T0")
  iDepr = which(!c(
    missing(R_ref.n),missing(E_0.n),missing(Tsoil.n),missing(T_ref.n),missing(T_0.n)))
  if (length(iDepr)) warning(
    "Argument names ",varNamesDepr[iDepr]," have been deprecated."
    ," Please, use instead ", varNamesNew[iDepr])
  # Fitting temperature T_0 from  paper
  R <- RRef * exp(E0 * (1/(TRef - T0) - 1/(TSoil - T0) ) )
  attr(R, 'varnames') <- 'R'
  attr(R, 'units') <- 'umol_m-2_s-1'
  return(R)
  ##value<<
  ## Data vector of soil respiration rate (R, umol CO2 m-2 s-1)
}
attr(fLloydTaylor, "ex") <- function() {
  T <- c(-10:30)
  resp <- fLloydTaylor(10, 330, T + 273.15)
	plot(resp ~ T)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Solar radiation properties
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# #' @export
# fCalcSunPosition <- function(
#   ##description<<
#   ## Calculate the position of the sun
#   DoY = DoY.V.n             ##<<
#   ## Data vector with day of year (DoY), same length as Hour or length 1
#   , Hour = Hour.V.n           ##<<
#   ## Data vector with time as decimal hour of local time zone
#   , LatDeg = Lat_deg.n          ##<< Latitude in (decimal) degrees
#   , LongDeg = Long_deg.n         ##<< Longitude in (decimal) degrees
#   , TimeZone = TimeZone_h.n       ##<< Time zone (in hours)
#   , useSolartime = TRUE	##<<
#   ## by default corrects hour (given in local winter time) for latitude
#   ## to solar time (where noon is exactly at 12:00). Set this to FALSE
#   ## to directly use local winter time
#   , DoY.V.n            ##<< deprecated
#   , Hour.V.n           ##<< deprecated
#   , Lat_deg.n          ##<< deprecated
#   , Long_deg.n         ##<< deprecated
#   , TimeZone_h.n       ##<< deprecated
#   , useSolartime.b = TRUE	##<< deprecated
#   ##author<<  ## AMM
# ) {
#   if (!missing(useSolartime.b)) useSolartime <- useSolartime.b
#   varNamesDepr <- c(
#     "DoY.V.n","Hour.V.n","Lat_deg.n","Long_deg.n","TimeZone_h.n"
#     ,"useSolartime.b")
#   varNamesNew <- c(
#     "DoY","Hour","LatDeg","LongDeg","TimeZone"
#     ,"useSolartime")
#   iDepr = which(!c(
#     missing(DoY.V.n),missing(Hour.V.n),missing(Lat_deg.n),missing(Long_deg.n)
#     ,missing(TimeZone_h.n),missing(useSolartime.b)))
#   if (length(iDepr)) warning(
#     "Argument names ",varNamesDepr[iDepr]," have been deprecated."
#     ," Please, use instead ", varNamesNew[iDepr])
#   #
#   # Formulas taken from Alessandro Cescatti's C ++ code
#   # Fractional year in radians
#   FracYearRad <- 2 * pi * (DoY - 1) / 365.24
#
#   # Equation of time in hours, accounting for changes in the time of solar noon
#   EqTimeHour <- (0.0072 * cos(FracYearRad) - 0.0528 * cos(2 * FracYearRad)
#                  - 0.0012 * cos(3 * FracYearRad) - 0.1229 * sin(FracYearRad)
#                     - 0.1565 * sin(2 * FracYearRad) - 0.0041 * sin(3 * FracYearRad) )
#
#   # Local time in hours
#   LocTimeHour <- (LongDeg / 15 - TimeZone)
#
#   ##details<<
#   ## This code assumes that Hour is given in local winter time zone, and
#   ## corrects it by longitude to
#   ## solar time (where noon is exactly at 12:00).
#   ## Note: This is different form reference PVWave-code,
#   ## that does not account for solar time and uses winter time zone.
#   ## Set argument \code{useSolartime.b} to FALSE to use the
#   ## local winter time instead.
#
#   # Solar time
#   # Correction for local time and equation of time
#   SolTimeHour <- if (useSolartime) {
#     # Correction for local time and equation of time
#     Hour + LocTimeHour + EqTimeHour
#   } else {
#     #! Note: For reproducing values close to Fluxnet Rg_pot which is without
#     #local time and eq of time correction
#     #! (CEIP is even different)
#     warning('Solar position calculated without correction for local time '
#             , 'and equation of time.')
#     Hour
#   }
#   # Conversion to radians
#   SolTimeRad <- (SolTimeHour - 12) * pi / 12.0
#   # Correction for solar time < -pi to positive, important for SolAzim_rad.V.n below
#   SolTimeRad <- ifelse(SolTimeRad < -pi, SolTimeRad + 2 * pi, SolTimeRad)
#   attr(SolTimeHour, 'varnames') <- 'SolTime'
#   attr(SolTimeHour, 'units') <- 'hour'
#
#   #Solar declination in radians, accounting for the earth axis tilt
#   SolDeclRad <- ((0.33281 - 22.984 * cos(FracYearRad) - 0.34990 * cos(2 * FracYearRad)
#                    - 0.13980 * cos(3 * FracYearRad) + 3.7872 * sin(FracYearRad)
#                   + 0.03205 * sin(2 * FracYearRad)
#                    + 0.07187 * sin(3 * FracYearRad)) / 180 * pi)
#   attr(SolDeclRad, 'varnames') <- 'SolDecl'
#   attr(SolDeclRad, 'units') <- 'rad'
#
#   # Solar elevation (vertical, zenithal angle) in radians with zero for horizon
#   SolElevRad <-  asin(sin(SolDeclRad) * sin(LatDeg / 180 * pi)
#                            + cos(SolDeclRad)*cos(LatDeg/180*pi)*cos(SolTimeRad))
#   attr(SolElevRad, 'varnames') <- 'SolElev'
#   attr(SolElevRad, 'units') <- 'rad'
#
#   # Solar azimuth (horizontal angle) with zero for North
#   SolAzimCos <- ((cos(SolDeclRad) * cos(SolTimeRad)
#                    - sin(SolElevRad) * cos(LatDeg / 180 * pi) )
#                        / (sin(LatDeg / 180 * pi) * cos(SolElevRad) ) )
#   # Correction if off edge values
#   SolAzimCos[SolAzimCos > +1] <- 1
#   SolAzimCos[SolAzimCos < -1] <- 1
#   # Conversion to radians
#   SolAzim_rad.V.n <- acos(SolAzimCos)
#   # Determine if solar azimuth is East or West depending on solar time
#   SolAzim_rad.V.n <- ifelse(
#     SolTimeRad < 0, pi - SolAzim_rad.V.n, pi + SolAzim_rad.V.n)
#   attr(SolAzimCos, 'varnames') <- 'SolAzim'
#   attr(SolAzimCos, 'units') <- 'rad'
#
#   ##value<<
#   ## Data list with the following items:
#   SolPosition.L <- list(
#     SolTime = SolTimeHour     ##<< Solar time (SolTime, hours)
#     , SolDecl = SolDeclRad  ##<< Solar declination (SolDecl, rad)
#     , SolElev = SolElevRad  ##<< Solar elevation with 0 at horizon (SolElev, rad)
#     , SolAzim = SolAzim_rad.V.n  ##<< Solar azimuth with 0 at North (SolAzim, rad)
#   )
# }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fCalcExtRadiation <- function(
  ##description<<
  ## Calculate the extraterrestrial solar radiation with the
  ## eccentricity correction
  DoY = DoY.V.n       ##<< Data vector with day of year (DoY)
  , DoY.V.n           ##<< deprecated, use DoY
  ##author<<
  ## AMM
) {
  if (!missing(DoY.V.n)) warning(
    "Argument name 'DoY.V.n' is deprecated. Use instead DoY")
  # Calculate extraterrestrial solar radiation after Lanini, 2010
  # (Master thesis, Bern University)
  # Fractional year in radians
  FracYearRad <- 2 * pi * (DoY - 1) / 365.24

  # Total solar irradiance
  SolarIrr_Wm2.c <- 1366.1 #W / m-2

  #Eccentricity correction
  ExtRadiation.V.n <- SolarIrr_Wm2.c * (
    1.00011 + 0.034221 * cos(FracYearRad) + 0.00128 * sin(FracYearRad)
     + 0.000719 * cos(2 * FracYearRad) + 0.000077 * sin(2 * FracYearRad)
     )
  attr(ExtRadiation.V.n, 'varnames') <- 'ExtRad'
  attr(ExtRadiation.V.n, 'units') <- 'W_m-2'
  ExtRadiation.V.n
  ##value<<
  ## Data vector of extraterrestrial radiation (ExtRad, W_m-2)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
#' @importFrom solartime computeSunPositionDoyHour
fCalcPotRadiation <- function(
  ##description<<
  ## Calculate the potential radiation
  DoY = DoY.V.n             ##<< Data vector with day of year (DoY), same length as Hour or length 1
  , Hour = Hour.V.n           ##<< Data vector with time as decimal hour of local time zone
  , LatDeg = Lat_deg.n          ##<< Latitude in (decimal) degrees
  , LongDeg = Long_deg.n         ##<< Longitude in (decimal) degrees
  , TimeZone = TimeZone_h.n       ##<< Time zone (in hours)
  , useSolartime = TRUE	##<< by default corrects hour (given in local winter time)
  ##  for latitude to solar time
  ## (where noon is exactly at 12:00). Set this to FALSE to directly use local winter time
  , DoY.V.n            ##<< deprecated
  , Hour.V.n           ##<< deprecated
  , Lat_deg.n          ##<< deprecated
  , Long_deg.n         ##<< deprecated
  , TimeZone_h.n       ##<< deprecated
  , useSolartime.b = TRUE	##<< deprecated
  ##author<<
  ## AMM
  #For testing PotRadiation(julday, hour)
) {
  if (!missing(useSolartime.b)) useSolartime <- useSolartime.b
  varNamesDepr <- c(
    "DoY.V.n","Hour.V.n","Lat_deg.n","Long_deg.n","TimeZone_h.n"
    ,"useSolartime.b")
  varNamesNew <- c(
    "DoY","Hour","LatDeg","LongDeg","TimeZone"
    ,"useSolartime")
  iDepr = which(!c(
    missing(DoY.V.n),missing(Hour.V.n),missing(Lat_deg.n),missing(Long_deg.n)
    ,missing(TimeZone_h.n),missing(useSolartime.b)))
  if (length(iDepr)) warning(
    "Argument names ",varNamesDepr[iDepr]," have been deprecated."
    ," Please, use instead ", varNamesNew[iDepr])
  # Calculate potential radiation from solar elevation and extraterrestrial
  # solar radiation
  SolElevRad <- computeSunPositionDoyHour(
    DoY, Hour, LatDeg, LongDeg, TimeZone
    , isCorrectSolartime = useSolartime)[,"elevation"]
  ExtRadiation <- fCalcExtRadiation(DoY)
  PotRadiation <- ifelse(
    SolElevRad <= 0, 0, ExtRadiation * sin(SolElevRad) )
  attr(PotRadiation, 'varnames') <- 'PotRad'
  attr(PotRadiation, 'units') <- attr(ExtRadiation, 'units')
  PotRadiation
  ##value<<
  ## Data vector of potential radiation (PotRad, W_m-2)
}
attr(fCalcPotRadiation, "ex") <- function() {
	hour <- seq(8, 16, by = 0.1)
	potRadSolar <- fCalcPotRadiation(160, hour, 39.94, -5.77, TimeZone = +1)
	potRadLocal <- fCalcPotRadiation(160, hour, 39.94, -5.77, TimeZone = +1
		  , useSolartime = FALSE)
	plot(potRadSolar ~ hour, type = 'l')
	abline(v = 13, lty = "dotted")
	lines(potRadLocal ~  hour, col = "blue")
	abline(v = 12, col = "blue", lty = "dotted")
	legend("bottomright", legend = c("solar time", "local winter time")
		, col = c("black", "blue"), inset = 0.05, lty = 1)
}
