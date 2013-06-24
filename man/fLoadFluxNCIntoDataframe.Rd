\name{fLoadFluxNCIntoDataframe}
\alias{fLoadFluxNCIntoDataframe}
\title{Load specified variables and time stamp information from NetCDF file}
\description{The time stamp information needs to be provided as variables 'year', 'month', 'day', 'hour' (Fluxnet BGI format).}
\usage{fLoadFluxNCIntoDataframe(VarList.V.s, FileName.s, Dir.s = "")}
\arguments{
  \item{VarList.V.s}{Vector of variables to be read in}
  \item{FileName.s}{File name}
  \item{Dir.s}{Directory}
}

\value{Data frame with data from nc file.}

\author{AMM, KS
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
# Example code
if( file.exists('inst/MDSdata/Example_DE-Tha.1996.1998.hourly.nc') )
  EddyNCData.F <- fLoadFluxNCIntoDataframe(c('NEE', 'Rg', 'NEE_f'), 'Example_DE-Tha.1996.1998.hourly.nc', 'inst/MDSdata')
}
