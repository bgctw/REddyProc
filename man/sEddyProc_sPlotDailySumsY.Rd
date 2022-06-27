\name{sEddyProc_sPlotDailySumsY}
\alias{sEddyProc_sPlotDailySumsY}
\title{sEddyProc sPlotDailySumsY}
\description{This function first computes the average flux for each day.
If the original unit is not "per day", then it need to be converted to
"per day" by argument \code{timeFactor}.
Furthermore, a change of the mass unit is provided by argument
\code{massFactor}.
The default parameters assume original units of mumol CO2 / m2 / second
and convert to gC / m2 / day.
The conversion factors allow plotting variables with different units}
\usage{sEddyProc_sPlotDailySumsY(Var, VarUnc = "none", 
    Year, timeFactor = 3600 * 24, massFactor = (44.0096/1e+06) * 
        (12.011/44.0096), unit = "gC/m2/day", 
    dts = sINFO$DTS, data = cbind(sDATA, 
        sTEMP))}
\arguments{
  \item{Var}{(Filled) variable to plot}
  \item{VarUnc}{Uncertainty
estimates for variable}
  \item{Year}{Year to plot}
  \item{timeFactor}{time
conversion factor with default per second to per day}
  \item{massFactor}{mass
conversion factor with default from mumol CO2 to g C}
  \item{unit}{unit
of the daily sums}
  \item{dts}{numeric integer}
  \item{data}{data.frame with variables to plot}
}



\author{AMM, KS
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}





