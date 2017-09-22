\name{sPlotDailySumsY}
\alias{sPlotDailySumsY}
\title{sEddyProc$sPlotDailySumsY - Plot daily sum of specified year}
\description{The daily sums for a single year are plotted to the current device, scaled to all data.
The daily sums are only calculated for days with complete data.
This function first computes the everage flux for each day.
If the original unit is not "per day", then it need to be converted to "per day" by argument \code{timeFactor.n}. 
Furthermore, a change of the mass unit is provided by argument \code{massFactor.n}.
The default parameters assume original units of mumol CO2/m2/second and convert to gC/m2/day.
The conversion factors allow plotting variables with different units}
\usage{sPlotDailySumsY(Var.s, VarUnc.s = "none", 
    Year.i, timeFactor.n = 3600 * 24, massFactor.n = (44.0096/1e+06) * 
        (12.011/44.0096), unit.s = "gC/m2/day")}
\arguments{
  \item{Var.s}{(Filled) variable to plot}
  \item{VarUnc.s}{Uncertainty estimates for variable}
  \item{Year.i}{Year to plot}
  \item{timeFactor.n}{time conversion factor with default per second to per day}
  \item{massFactor.n}{mass conversion factor with default from mumol CO2 to g C}
  \item{unit.s}{resulting unit}
}



\author{AMM, KS
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





