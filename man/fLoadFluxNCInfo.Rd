\name{fLoadFluxNCInfo}
\alias{fLoadFluxNCInfo}
\title{Get site information from BGI NetCDF files}
\description{Load site information attributes such as latitude, longitude and others from BGI NetCDF files}
\usage{fLoadFluxNCInfo(FileName.s, Dir.s, NcPackage.s = "ncdf4", 
    CallFunction.s = "")}
\arguments{
  \item{FileName.s}{NetCDF file name as a string}
  \item{Dir.s}{Directory as a string}
  \item{NcPackage.s}{Name (string) of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')}
  \item{CallFunction.s}{Name (string) of function called from}
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
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





