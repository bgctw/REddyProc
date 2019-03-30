bigleaf package replacing GeoFunctions of REddyProc
===================================================

`REddyProc` package included several utility functions that were
somewhat out of the package scope. These functions are removed from
package.

This vignette shows how to replace them by functions from the `bigleaf`
package.

    if (!require("bigleaf", quietly = TRUE)) stop(
      "bigleaf package must be installed to create this vignette.")
    library(REddyProc)

Vapour pressure deficit (VPD)
-----------------------------

**Vapour pressure deficit (VPD)** was computed with REddyProc in
*h**P**a* from relative humidity in % and air temperature in
**<sup>∘</sup>*C*.

    VPD0 <- fCalcVPDfromRHandTair(DEGebExample$rH, DEGebExample$Tair)

`bigleaf` package computes VPD in *k**P**a* and requires relative
humidity as fraction .

    VPD.hPa <- rH.to.VPD(DEGebExample$rH/100, DEGebExample$Tair)*10

**Saturation vapor pressure (SVP)** or eSat in *h**P**a* was computed
from air temperature.

    Tair <- seq(10,25,by = 5)

    eSat0 <- fCalcSVPfromTair(Tair)

is replaced by `Esat.slope` which uses *k**P**a* as pressure unit:

    (eSat <- Esat.slope(Tair)$Esat * 10)

    ## [1] 12.26030 17.01672 23.32596 31.60057

**Actual vapor pressure (AVP)** or e in *h**P**a* was also computed from
Vapor mole fraction (VMF) in *m**o**l*/*m**o**l* and pressure in
*h**P**a*.

    VMF <- seq(0.01,0.03,by = 0.005)
    press.in.hPa <- 1000 

    e0 <- fCalcAVPfromVMFandPress(VMF, press.in.hPa)

There is no replacement function, as this is just the multiplication of
the two arguments.

    (e <- VMF * press.in.hPa)

    ## [1] 10 15 20 25 30

**Relative humitity (rH)** in % was computed from AVP in *h**P**a* and
temperature.

    e.in.hPa <- seq(0,30,by = 5)
    Tair <- 25           

    (rH0 <- fCalcRHfromAVPandTair(e.in.hPa, Tair))

Again this is replaced by a function using pressure units *k**P**a* and
relative humitidy as fraction.

    (rH <- e.to.rH(e.in.hPa/10, Tair)*100)

    ## [1]  0.0000 15.8225 31.6450 47.4675 63.2900 79.1125 94.9350

Evapotranspiration from latent heat and air temperature
-------------------------------------------------------

    LE <- seq(300,500,by = 50)
    Tair <- 25

    ET0 <- fCalcETfromLE(LE, Tair)

The corresponding bigleaf function `LE.to.ET` returns a value in
kg/m2/s. This needs to be converted to mmol/m2/s as returned by the
former `fCalcETfromLE`.

    ETkg <- LE.to.ET(LE, Tair)
    (ETmmol <- kg.to.mol(ETkg)*1000)

    ## [1]  6.819916  7.956569  9.093222 10.229874 11.366527

Converting visible radiation from irradiance to photons flux
------------------------------------------------------------

Photon flux density (PPFD) of visible light can be computed from energy
in incoming radiation

    Rg <- 200

`bigleaf` function `Rg.to.PPFD` combines the two former REddyProc
functions `fConvertVisibleWm2toPhotons` and `fConvertGlobalToVisible`.

    PPFDVis0 <- fConvertGlobalToVisible(fConvertVisibleWm2toPhotons(Rg))

    (PPFDVis <- Rg.to.PPFD(Rg))

    ## [1] 460

The PPFD of light including non-visible parts, i.e. former
`fConvertVisibleWm2toPhotons`, is obtained by setting argument
`frac_PAR` to 1.

    (PPFDAll <- Rg.to.PPFD(Rg, frac_PAR = 1))

    ## [1] 920

Potential and Extraterrestrial solar radiation
----------------------------------------------

Potential radiation (*W**m*<sup>−2</sup>) depends on time and
geo-location.

    doy <- 160
    hour <- seq(6,18,by = 0.2)
    latDeg <- 39.94
    longDeg <- -5.77
    timezone <- +1

Formerly, REddyProc provided:

    (potRad0 <- fCalcPotRadiation(doy, hour, latDeg, longDeg, timezone))

This is replaced by bigleaf:

    head(potRad <- potential.radiation(doy, hour, latDeg, longDeg, timezone))

    ## [1]   1.722378  48.072563  95.193985 142.957488 191.232157 239.885672

Extraterrestrial solar radiation was formerly computed by REddyProc by:

    (extRad0 <- fCalcExtRadiation(doy))

This is replaced by bigleaf:

    (extRad <- extraterrestrial.radiation(doy))

    ## [1] 1324.598

Required computation of sun position (`computeSunPosition`) and
difference between apparent local time time and time zone time
(`computeSolarToLocalTimeDifference`) have been moved to package
`solartime`.
