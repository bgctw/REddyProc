#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with geo functions to calculate latent variables or convert units
#+++ Dependencies: fCheckOutsideRange() in DataFunctions.R 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit conversions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fConvertCtoK <- function(
  ##title<<
  ## Convert degree Celsius to degree Kelvin
  Celsius.V.n           ##<< Data vector in Celsius (°C)
  ##author<<
  ## AMM
)
{
  fCheckOutsideRange(cbind(Celsius=Celsius.V.n), 'Celsius', c('<', -273.15), 'fConvertCtoK')
  Kelvin.V.n <-  Celsius.V.n + 273.15
  attr(Kelvin.V.n, 'varname') <- 'Temp_K'
  attr(Kelvin.V.n, 'units') <- 'degK'
  return(Kelvin.V.n)
  ##value<<
  ## Data vector in temperature Kelvin (Temp_K, °K)
}

fConvertKtoC <- function(
  ##title<<
  ## Convert degree Kelvin to degree Celsius
  Kelvin.V.n            ##<< Data vector in Kelvin (°K)
  ##author<<
  ## AMM
)
{
  fCheckOutsideRange(cbind(Kelvin=Kelvin.V.n), 'Kelvin', c('<', 0), 'fConvertKtoC')
  Celsius.V.n <-  Kelvin.V.n - 273.15
  attr(Celsius.V.n, 'varname') <- 'Temp_C'
  attr(Celsius.V.n, 'units') <- 'degC'
  return(Celsius.V.n)
  ##value<<
  ## Data vector in temperature Celsius (Temp_C, °C)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fConvertVisibleWm2toPhotons <- function(
  ##title<<
  ## Convert units of visible radiation from irradiance to photons flux
  Wm2.V.n               ##<< Data vector in units of irradiance (W m-2) 
  )
{
  # With Planck's equation at 550 nm wavelength (see also FormelBackup.doc and NormanWeiss1985 paper)
  Photons.V.n <- 4.6 * Wm2.V.n
  attr(Vis.V.n, 'varname') <- 'PPFD'
  attr(Vis.V.n, 'units') <- 'umol_m-2_s-1'
  return(Photons.V.n)
  ##value<<
  ## Data vector in units of photons flux (PPFD, umol photons m-2 s-1)
}

fConvertGlobalToVisible <- function(
  ##title<<
  ## Partition global (solar) radiation into only visible (the rest is UV and infrared)
  Global.V.n            ##<< Data vector of global radiation (W m-2)
  ##author<<
  ## AMM
)
{
  # Ratio of visible to total solar radiation, see Weiss-Norman 1985 paper
  Vis.V.n <- 0.5 * Global.V.n
  attr(Vis.V.n, 'varname') <- 'VisRad'
  attr(Vis.V.n, 'units') <- 'W_m-2'
  return(Vis.V.n)
  ##value<<
  ## Data vector of visible part of solar radiation (VisRad, W m-2)
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Latent variable calculations
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCalcVPDfromRHandTair <- function(
  ##title<<
  ## Calculate VPD from rH and Tair
  RH.V.n                ##<< Data vector of relative humidity (rH, %)
  ,Tair.V.n             ##<< Data vector of air temperature (°Tair, C)
  ##author<<
  ## AMM
  )
{ 
  fCheckOutsideRange(cbind(RelHumidity_Percent=RH.V.n), 'RelHumidity_Percent', c('<', 0, '|', '>', 100), 'fCalcVPDfromRHandTair')
  fCheckOutsideRange(cbind(AirTemp_degC=Tair.V.n), 'AirTemp_degC', c('<', -70, '|', '>', 60), 'fCalcVPDfromRHandTair')
  # See Kolle Logger Tools Software 2012 (Magnus coefficients for water between 0 and 100°C)
  VPD.V.n <- 6.1078 * (1 -RH.V.n/100) * exp(17.08085*Tair.V.n/(234.175+Tair.V.n))
  attr(VPD.V.n, 'varname') <- 'VPD'
  attr(VPD.V.n, 'units') <- 'hPa'
  return(VPD.V.n)
  ##value<<
  ## Data vector of vapour pressure deficit (VPD, hPa)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCalcSVPfromTair <- function(
  ##title<<
  ## Calculate SVP (of water) from Tair
  Tair.V.n              ##<< Data vector of air temperature (Tair, °C)
  ##author<<
  ## AMM
  ) {
  # Approximation formula after CANVEG (Berkeley) code
  TZero.c <- 273.15
  SVP.V.n <- exp(54.8781919 - 6790.4985 / (Tair.V.n+TZero.c) - 5.02808 * log(Tair.V.n+TZero.c))
  attr(SVP.V.n, 'varname') <- 'SVP'
  attr(SVP.V.n, 'units') <- 'mbar'
  return(SVP.V.n)
  ##value<<
  ## Data vector of saturation VP (SVP, mbar)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCalcRHfromAVPandTair <- function(
  ##title<<
  ## Calculate relative humidity from actual vapour pressure and air tempature
  AVP.V.n               ##<< Data vector of actual vapour pressure (AVP, mbar)
  ,Tair.V.n             ##<< Data vector of air temperature (Tair, °C)
  ##author<<
  ## AMM
  )
{
  # Definition of relative humidity (ratio of AVP to SVP)
  SVP.V.n <- fCalcSVPfromTair(Tair.V.n)
  RH.V.n <- AVP.V.n/SVP.V.n * 100
  attr(RH.V.n, 'varname') <- 'rH'
  attr(RH.V.n, 'units') <- '%'
  return(RH.V.n)
  ##value<<
  ## Data vector of relative humidity (rH, %)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCalcETfromLE <- function(
  ##title<<
  ## Calculate ET from LE and Tair
  LE.V.n                ##<< Data vector of latent heat (LE, W m-2)
  ,Tair.V.n             ##<< Data vector of air temperature (Tair, °C)
  ##author<<
  ## AMM
  ) {
  # Calculated from the water density and latent heat of vaporation, see Moffat manuscript on WUE
  ET.V.n <- LE.V.n / ((2.501 - 0.00236 * Tair.V.n) * 18.016)
  attr(ET.V.n, 'varname') <- 'ET'
  attr(ET.V.n, 'units') <- 'mmol_m-2_s-1'
  return(ET.V.n)
  ##value<<
  ## Data vector of evapotranspiration (ET, mmol H20 m-2 s-1)
}
