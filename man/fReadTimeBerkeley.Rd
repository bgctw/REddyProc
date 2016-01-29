\name{fReadTimeBerkeley}
\alias{fReadTimeBerkeley}
\title{fReadTimeBerkeley}

\usage{fReadTimeBerkeley(Data.F, FileName.s, Dir.s, NcPackage.s, 
    CallFunction.s = "", colTime = "TIMESTAMP_END", 
    ...)}
\arguments{
  \item{Data.F}{Data frame}
  \item{FileName.s}{NetCDF file name}
  \item{Dir.s}{Directory}
  \item{NcPackage.s}{Name of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')}
  \item{CallFunction.s}{Name of function called from}
  \item{colTime}{the column name holding time with format described in details}
  \item{\dots}{further arguments to var.get.nc or ncvar_get, such as start and count}
}
\details{In the Berkeley-Release of the fluxnet data, the time is stored as an integer
with base10-digits representing YYYYMMddhhmm }


\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





