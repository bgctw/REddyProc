\name{fLoadFluxNCIntoDataframe}
\alias{fLoadFluxNCIntoDataframe}
\title{Load NetCDF file}
\description{Load specified variables and time stamp information from NetCDF file in Fluxnet BGI format.
The time stamp information needs to be provided as variables 'year', 'month', 'day', 'hour'.}
\usage{fLoadFluxNCIntoDataframe(VarList.V.s, FileName.s, 
    Dir.s = "", NcPackage.s = "ncdf4", fReadTime = fReadTimeSeveralCols, 
    ...)}
\arguments{
  \item{VarList.V.s}{String vector of variables to be read in}
  \item{FileName.s}{File name as a string}
  \item{Dir.s}{Directory as a string}
  \item{NcPackage.s}{Name of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4') as a string}
  \item{fReadTime}{function that reads time columns, must append columns year (from 0AD), month, day, and hour (fractional)}
  \item{\dots}{further arguments to var.get.nc or ncvar_get, such as start and count}
}

\value{Data frame with data from nc file.}

\author{AMM, KS
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}




\examples{\donttest{
EddyNCData.F <- fLoadFluxNCIntoDataframe(c('NEE', 'Rg', 'NEE_f')
, getExamplePath('Example_DE-Tha.1996.1998.hourly_selVars.nc', TRUE))
}}
