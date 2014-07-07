\name{fLoadFluxNCInfo}
\alias{fLoadFluxNCInfo}
\title{Get site information from BGI NetCDF files}
\description{Load site information attributes such as latitude, longitude and others from BGI NetCDF files}
\usage{fLoadFluxNCInfo(FileName.s, Dir.s, NcPackage.s = "ncdf4", CallFunction.s = "")}
\arguments{
  \item{FileName.s}{NetCDF file name}
  \item{Dir.s}{Directory}
  \item{NcPackage.s}{Name of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')}
  \item{CallFunction.s}{Name of function called from}
}
\details{Description of attribute list:\describe{
\item{ID}{SiteID}
\item{DIMS}{Number of data rows}
\item{LON}{Longitude}
\item{LAT}{Latitude}
\item{TZ}{Time zone}
\item{ELEV}{Elevation}
\item{IGBP}{IGBP class}
}}
\value{Attibute list}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





