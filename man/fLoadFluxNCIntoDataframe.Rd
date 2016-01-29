\name{fLoadFluxNCIntoDataframe}
\alias{fLoadFluxNCIntoDataframe}
\title{Load NetCDF file}
\description{Load specified variables and time stamp information from NetCDF file in Fluxnet BGI format.
The time stamp information needs to be provided as variables 'year', 'month', 'day', 'hour'.}
\usage{fLoadFluxNCIntoDataframe(VarList.V.s, FileName.s, Dir.s = "", 
    NcPackage.s = "ncdf4", fReadTime = fReadTimeSeveralCols, 
    ...)}
\arguments{
  \item{VarList.V.s}{Vector of variables to be read in}
  \item{FileName.s}{File name}
  \item{Dir.s}{Directory}
  \item{NcPackage.s}{Name of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')}
  \item{fReadTime}{function that reads time columns, must append columns year (from 0AD), month, day, and hour (fractional)}
  \item{\dots}{further arguments to var.get.nc or ncvar_get, such as start and count}
}

\value{Data frame with data from nc file.}

\author{AMM, KS
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
if( FALSE ) { #Do not always execute example code (e.g. on package installation)
  Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
  EddyNCData.F <- fLoadFluxNCIntoDataframe(c('NEE', 'Rg', 'NEE_f'), 'Example_DE-Tha.1996.1998.hourly.nc', Dir.s)
}
}
