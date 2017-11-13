#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with geo functions to calculate latent variables or convert units
#+++ Dependencies: fCheckOutsideRange() in DataFunctions.R
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit conversions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fConvertCtoK <- function(
  ##description<<
  ## Convert degree Celsius to degree Kelvin
  Celsius.V.n           ##<< Data vector in Celsius (degC)
  ##author<<
  ## AMM
)
{
  fCheckOutsideRange(cbind(Celsius = Celsius.V.n), 'Celsius', c('<', -273.15), 'fConvertCtoK')
  Kelvin.V.n <-  Celsius.V.n + 273.15
  attr(Kelvin.V.n, 'varnames') <- 'Temp_K'
  attr(Kelvin.V.n, 'units') <- 'degK'
  return(Kelvin.V.n)
  ##value<<
  ## Data vector in temperature Kelvin (Temp_K, degK)
}

#' @export
fConvertKtoC <- function(
  ##description<<
  ## Convert degree Kelvin to degree Celsius
  Kelvin.V.n            ##<< Data vector in Kelvin (degK)
  ##author<<
  ## AMM
)
{
  fCheckOutsideRange(cbind(Kelvin = Kelvin.V.n), 'Kelvin', c('<', 0), 'fConvertKtoC')
  Celsius.V.n <-  Kelvin.V.n - 273.15
  attr(Celsius.V.n, 'varnames') <- 'Temp_C'
  attr(Celsius.V.n, 'units') <- 'degC'
  return(Celsius.V.n)
  ##value<<
  ## Data vector in temperature Celsius (Temp_C, degC)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fConvertVisibleWm2toPhotons <- function(
  ##description<<
  ## Convert units of visible radiation from irradiance to photons flux
  Wm2.V.n               ##<< Data vector in units of irradiance (W m-2)
  )
{
  # With Planck's equation at 550 nm wavelength (see also FormelBackup.doc and NormanWeiss1985 paper)
  Photons.V.n <- 4.6 * Wm2.V.n
  attr(Vis.V.n, 'varnames') <- 'PPFD'
  attr(Vis.V.n, 'units') <- 'umol_m-2_s-1'
  return(Photons.V.n)
  ##value<<
  ## Data vector in units of photons flux (PPFD, umol photons m-2 s-1)
}

#' @export
fConvertGlobalToVisible <- function(
  ##description<<
  ## Partition global (solar) radiation into only visible (the rest is UV and infrared)
  Global.V.n            ##<< Data vector of global radiation (W m-2)
  ##author<<
  ## AMM
)
{
  # Ratio of visible to total solar radiation, see Weiss-Norman 1985 paper
  Vis.V.n <- 0.5 * Global.V.n
  attr(Vis.V.n, 'varnames') <- 'VisRad'
  attr(Vis.V.n, 'units') <- 'W_m-2'
  return(Vis.V.n)
  ##value<<
  ## Data vector of visible part of solar radiation (VisRad, W m-2)
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Latent variable calculations
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fCalcVPDfromRHandTair <- function(
  ##description<<
  ## Calculate VPD from rH and Tair
  RH.V.n                ##<< Data vector of relative humidity (rH, %)
  , Tair.V.n             ##<< Data vector of air temperature (Tair, degC)
  ##author<<
  ## AMM
  )
{
  fCheckOutsideRange(cbind(RelHumidity_Percent = RH.V.n), 'RelHumidity_Percent', c('<', 0, '|', '>', 100), 'fCalcVPDfromRHandTair')
  fCheckOutsideRange(cbind(AirTemp_degC = Tair.V.n), 'AirTemp_degC', c('<', -70, '|', '>', 60), 'fCalcVPDfromRHandTair')
  # See Kolle Logger Tools Software 2012 (Magnus coefficients for water between 0 and 100 degC)
  VPD.V.n <- 6.1078 * (1 -RH.V.n / 100) * exp(17.08085 * Tair.V.n / (234.175 + Tair.V.n))
  attr(VPD.V.n, 'varnames') <- 'VPD'
  attr(VPD.V.n, 'units') <- 'hPa'
  return(VPD.V.n)
  ##value<<
  ## Data vector of vapour pressure deficit (VPD, hPa (mbar))
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fCalcSVPfromTair <- function(
  ##description<<
  ## Calculate SVP (of water) from Tair
  Tair.V.n              ##<< Data vector of air temperature (Tair, degC)
  ##author<<
  ## AMM
  ) {
  # Approximation formula after CANVEG (Berkeley) code
  TZero.c <- 273.15  #Absolute zero
  SVP.V.n <- exp(54.8781919 - 6790.4985 / (Tair.V.n + TZero.c) - 5.02808 * log(Tair.V.n + TZero.c))
  attr(SVP.V.n, 'varnames') <- 'SVP'
  attr(SVP.V.n, 'units') <- 'hPa'
  return(SVP.V.n)
  ##value<<
  ## Data vector of saturation vapor pressure (SVP, hPa (mbar))
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fCalcAVPfromVMFandPress <- function(
  ##description<<
  ## Calculate AVP from VMF and Press
  VMF.V.n                ##<< Vapor mole fraction (VMF, mol / mol)
  , Press.V.n             ##<< Atmospheric pressure (Press, hPa)
  ##author<<
  ## AMM
  ) {
  # Calculation of actual vapor pressure, also called vapor partial pressure
  AVP.V.n <- (VMF.V.n) * Press.V.n
  attr(AVP.V.n, 'varnames') <- 'AVP'
  attr(AVP.V.n, 'units') <- 'hPa'
  return(AVP.V.n)
  ##value<<
  ## Data vector of actual vapor pressure (AVP, hPa (mbar))
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fCalcRHfromAVPandTair <- function(
  ##description<<
  ## Calculate relative humidity from actual vapour pressure and air tempature
  AVP.V.n               ##<< Data vector of actual vapour pressure (AVP, hPa (mbar))
  , Tair.V.n             ##<< Data vector of air temperature (Tair, degC)
  ##author<<
  ## AMM
  )
{
  # Definition of relative humidity (ratio of AVP to SVP)
  SVP.V.n <- fCalcSVPfromTair(Tair.V.n)
  RH.V.n <- AVP.V.n / SVP.V.n * 100
  # Restrict to physically plausible range
  RH.V.n <- ifelse(RH.V.n >= 0, RH.V.n, 0)
  RH.V.n <- ifelse(RH.V.n <= 100, RH.V.n, 100)
  attr(RH.V.n, 'varnames') <- 'rH'
  attr(RH.V.n, 'units') <- '%'
  return(RH.V.n)
  ##value<<
  ## Data vector of relative humidity (rH, %)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fCalcETfromLE <- function(
  ##description<<
  ## Calculate ET from LE and Tair
  LE.V.n                ##<< Data vector of latent heat (LE, W m-2)
  , Tair.V.n             ##<< Data vector of air temperature (Tair, degC)
  ##author<<
  ## AMM
  ) {
  # Calculated from the water density and latent heat of vaporation, see Moffat manuscript on WUE
  ET.V.n <- LE.V.n / ((2.501 - 0.00236 * Tair.V.n) * 18.016)
  attr(ET.V.n, 'varnames') <- 'ET'
  attr(ET.V.n, 'units') <- 'mmol_m-2_s-1'
  return(ET.V.n)
  ##value<<
  ## Data vector of evapotranspiration (ET, mmol H20 m-2 s-1)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fLloydTaylor <- function(
  ##title<<
  ## Temperature dependence of soil respiration
  ##description<<
  ## Temperature dependence of soil respiration after Equation 11 in Lloyd & Taylor (1994)
  R_ref.n               ##<< Respiration rate at reference temperature
  , E_0.n                ##<< Temperature sensitivity ("activation energy") in Kelvin (degK)
  , Tsoil.n              ##<< Soil temperature in Kelvin (degK)
  , T_ref.n = 273.15 + 10    ##<< Reference temperature of 10 degC in Kelvin (degK)
  , T_0.n = 227.13         ##<< Regression temperature as fitted by LloydTaylor (1994) in Kelvin (degK)
  ##author<<
  ## AMM
  ##reference<<
  ## Lloyd J, Taylor JA (1994) On the temperature dependence of soil respiration. Functional Ecology, 8, 315-323.
)
{
  # Fitting temperature T_0 from  paper
  R <- R_ref.n * exp(E_0.n * (1 / (T_ref.n-T_0.n) - 1 / (Tsoil.n-T_0.n) ) )
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


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Solar radiation properties
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fCalcSunPosition <- function(
  ##description<<
  ## Calculate the position of the sun
  DoY.V.n               ##<< Data vector with day of year (DoY)
  , Hour.V.n             ##<< Data vector with time as decimal hour
  , Lat_deg.n            ##<< Latitude in (decimal) degrees
  , Long_deg.n           ##<< Longitude in (decimal) degrees
  , TimeZone_h.n         ##<< Time zone (in hours)
  , useSolartime.b = TRUE	##<< by default corrects hour (given in local winter time) for latitude to solar time
	##<< where noon is exactly at 12:00. Set this to FALSE to compare to code that uses local winter time
  ##author<<
  ## AMM
  #TEST: data('Example_DETha98', package = 'REddyProc'); DoY.V.n <- EddyData.F$DoY; Hour.V.n <- EddyData.F$Hour;
  #TEST: Lat_deg.n <- 51.0; Long_deg.n <- 13.6; TimeZone_h.n <- 1.0
  #TEST: fCalcSunPosition(EddyData.F$DoY, EddyData.F$Hour, Lat_deg.n = 51.0, Long_deg.n = 13.6, TimeZone_h.n = 1.0)
)
{
  # Formulas taken from Alessandro Cescatti's C ++ code
  # Fractional year in radians
  FracYear_rad.V.n <- 2 * pi * (DoY.V.n-1) / 365.24

  # Equation of time in hours, accounting for changes in the time of solar noon
  EqTime_h.V.n <- (0.0072 * cos(FracYear_rad.V.n) - 0.0528 * cos(2 * FracYear_rad.V.n) - 0.0012 * cos(3 * FracYear_rad.V.n) - 0.1229 * sin(FracYear_rad.V.n)
                    - 0.1565 * sin(2 * FracYear_rad.V.n) - 0.0041 * sin(3 * FracYear_rad.V.n) )

  # Local time in hours
  LocTime_h.V.n <- (Long_deg.n / 15 - TimeZone_h.n)

  ##details<<
  ## This code assumes that Hour is given in local winter time zone, and corrects it by longitude to
  ## solar time (where noon is exactly at 12:00).
  ## Note: This is different form reference PVWave-code,
  ## that does not account for solar time and uses winter time zone.
  ## Set argument \code{useSolartime.b} to FALSE to use the local winter time instead.

  # Solar time
  # Correction for local time and equation of time
  SolTime_h.V.n <- if (useSolartime.b) {
    # Correction for local time and equation of time
    Hour.V.n + LocTime_h.V.n + EqTime_h.V.n
  } else {
    #! Note: For reproducing values close to Fluxnet Rg_pot which is without local time and eq of time correction
    #! (CEIP is even different)
    warning('Solar position calculated without correction for local time and equation of time.')
    Hour.V.n
  }
  # Conversion to radians
  SolTime_rad.V.n <- (SolTime_h.V.n - 12) * pi / 12.0
  # Correction for solar time < -pi to positive, important for SolAzim_rad.V.n below
  SolTime_rad.V.n <- ifelse(SolTime_rad.V.n < -pi, SolTime_rad.V.n + 2 * pi, SolTime_rad.V.n)
  attr(SolTime_h.V.n, 'varnames') <- 'SolTime'
  attr(SolTime_h.V.n, 'units') <- 'hour'

  #Solar declination in radians, accounting for the earth axis tilt
  SolDecl_rad.V.n <- ( (0.33281-22.984 * cos(FracYear_rad.V.n) - 0.34990 * cos(2 * FracYear_rad.V.n) - 0.13980 * cos(3 * FracYear_rad.V.n)
                        + 3.7872 * sin(FracYear_rad.V.n) + 0.03205 * sin(2 * FracYear_rad.V.n) + 0.07187 * sin(3 * FracYear_rad.V.n)) / 180 * pi)
  attr(SolDecl_rad.V.n, 'varnames') <- 'SolDecl'
  attr(SolDecl_rad.V.n, 'units') <- 'rad'

  # Solar elevation (vertical, zenithal angle) in radians with zero for horizon
  SolElev_rad.V.n <-  asin(sin(SolDecl_rad.V.n) * sin(Lat_deg.n / 180 * pi)
                           + cos(SolDecl_rad.V.n) * cos(Lat_deg.n / 180 * pi) * cos(SolTime_rad.V.n))
  attr(SolElev_rad.V.n, 'varnames') <- 'SolElev'
  attr(SolElev_rad.V.n, 'units') <- 'rad'

  # Solar azimuth (horizontal angle) with zero for North
  SolAzim_cos.V.n <- ( (cos(SolDecl_rad.V.n) * cos(SolTime_rad.V.n) - sin(SolElev_rad.V.n) * cos(Lat_deg.n / 180 * pi) )
                       / (sin(Lat_deg.n / 180 * pi) * cos(SolElev_rad.V.n) ) )
  # Correction if off edge values
  SolAzim_cos.V.n[SolAzim_cos.V.n > + 1] <- 1
  SolAzim_cos.V.n[SolAzim_cos.V.n < -1] <- 1
  # Conversion to radians
  SolAzim_rad.V.n <- acos(SolAzim_cos.V.n)
  # Determine if solar azimuth is East or West depending on solar time
  SolAzim_rad.V.n <- ifelse(SolTime_rad.V.n < 0, pi - SolAzim_rad.V.n, pi + SolAzim_rad.V.n)
  attr(SolAzim_cos.V.n, 'varnames') <- 'SolAzim'
  attr(SolAzim_cos.V.n, 'units') <- 'rad'

  ##value<<
  ## Data list with the following items:
  SolPosition.L <- list(
    SolTime = SolTime_h.V.n     ##<< Solar time (SolTime, hours)
    , SolDecl = SolDecl_rad.V.n  ##<< Solar declination (SolDecl, rad)
    , SolElev = SolElev_rad.V.n  ##<< Solar elevation with 0 at horizon (SolElev, rad)
    , SolAzim = SolAzim_rad.V.n  ##<< Solar azimuth with 0 at North (SolAzim, rad)
  )
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fCalcExtRadiation <- function(
  ##description<<
  ## Calculate the extraterrestrial solar radiation with the eccentricity correction
  DoY.V.n           ##<< Data vector with day of year (DoY)
  ##author<<
  ## AMM
)
{
  # Calculate extraterrestrial solar radiation after Lanini, 2010 (Master thesis, Bern University)
  # Fractional year in radians
  FracYear_rad.V.n <- 2 * pi * (DoY.V.n-1) / 365.24

  # Total solar irradiance
  SolarIrr_Wm2.c <- 1366.1 #W / m-2

  #Eccentricity correction
  ExtRadiation.V.n <- SolarIrr_Wm2.c * (1.00011 + 0.034221 * cos(FracYear_rad.V.n) + 0.00128 * sin(FracYear_rad.V.n)
                                        + 0.000719 * cos(2 * FracYear_rad.V.n) + 0.000077 * sin(2 * FracYear_rad.V.n))
  attr(ExtRadiation.V.n, 'varnames') <- 'ExtRad'
  attr(ExtRadiation.V.n, 'units') <- 'W_m-2'
  ExtRadiation.V.n
  ##value<<
  ## Data vector of extraterrestrial radiation (ExtRad, W_m-2)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
fCalcPotRadiation <- function(
  ##description<<
  ## Calculate the potential radiation
  DoY.V.n             ##<< Data vector with day of year (DoY), same length as Hour or length 1
  , Hour.V.n           ##<< Data vector with time as decimal hour of local time zone
  , Lat_deg.n          ##<< Latitude in (decimal) degrees
  , Long_deg.n         ##<< Longitude in (decimal) degrees
  , TimeZone_h.n       ##<< Time zone (in hours)
  , useSolartime.b = TRUE	##<< by default corrects hour (given in local winter time) for latitude to solar time
	##<< (where noon is exactly at 12:00). Set this to FALSE to directly use local winter time
  ##author<<
  ## AMM
  #For testing PotRadiation(julday, hour)
)
{
  # Calculate potential radiation from solar elevation and extraterrestrial solar radiation
  SolElev_rad.V.n <- fCalcSunPosition(DoY.V.n, Hour.V.n, Lat_deg.n, Long_deg.n, TimeZone_h.n, useSolartime.b = useSolartime.b)$SolElev
  ExtRadiation.V.n <- fCalcExtRadiation(DoY.V.n)
  PotRadiation.V.n <- ifelse(SolElev_rad.V.n <= 0, 0, ExtRadiation.V.n * sin(SolElev_rad.V.n) )

  attr(PotRadiation.V.n, 'varnames') <- 'PotRad'
  attr(PotRadiation.V.n, 'units') <- attr(ExtRadiation.V.n, 'units')
  PotRadiation.V.n
  ##value<<
  ## Data vector of potential radiation (PotRad, W_m-2)
}
attr(fCalcPotRadiation, "ex") <- function() {
	hour <- seq(8, 16, by = 0.1)
	potRadSolar <- fCalcPotRadiation(160, hour, 39.94, -5.77, TimeZone =+ 1)
	potRadLocal <- fCalcPotRadiation(160, hour, 39.94, -5.77, TimeZone =+ 1
		, useSolartime.b = FALSE)
	plot(potRadSolar ~ hour, type = 'l')
	abline(v = 13, lty = "dotted")
	lines(potRadLocal ~  hour, col = "blue")
	abline(v = 12, col = "blue", lty = "dotted")
	legend("bottomright", legend = c("solar time", "local winter time")
		, col = c("black", "blue"), inset = 0.05, lty = 1)
}
