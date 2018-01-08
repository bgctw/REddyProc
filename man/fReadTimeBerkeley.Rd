\name{fReadTimeBerkeley}
\alias{fReadTimeBerkeley}
\title{fReadTimeBerkeley}
\description{Reads time columns (year, month, day, hour) from column in ISODate integer format}
\usage{fReadTimeBerkeley(Data.F, FileName.s, Dir.s, 
    NcPackage.s, CallFunction.s = "", colTime = "TIMESTAMP_END", 
    ...)}
\arguments{
  \item{Data.F}{Data frame}
  \item{FileName.s}{NetCDF file name as a string}
  \item{Dir.s}{Directory as a string}
  \item{NcPackage.s}{Name (string) of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')}
  \item{CallFunction.s}{Name (string) of function called from}
  \item{colTime}{the column name (string) holding time with format described in details}
  \item{\dots}{further arguments to var.get.nc or ncvar_get, such as start and count}
}
\details{In the Berkeley-Release of the fluxnet data, the time is stored as an integer
with base10-digits representing YYYYMMddhhmm}


\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





