\name{fReadTimeSeveralCols}
\alias{fReadTimeSeveralCols}
\title{fReadTimeSeveralCols}
\description{Reading }
\usage{fReadTimeSeveralCols(Data.F, FileName.s, Dir.s, NcPackage.s, 
    CallFunction.s = "", colYear = "year", colMonth = "month", 
    colDay = "day", colHour = "hour", defaultHour = 0, 
    ...)}
\arguments{
  \item{Data.F}{Data frame}
  \item{FileName.s}{NetCDF file name as a string}
  \item{Dir.s}{Directory as a string}
  \item{NcPackage.s}{Name (string) of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')}
  \item{CallFunction.s}{Name (string) of function called from}
  \item{colYear}{Name (string) of variable holding the year}
  \item{colMonth}{Name (string) of variable holding the month}
  \item{colDay}{Name (string) of variable holding the day}
  \item{colHour}{Name (string) of variable holding the hour}
  \item{defaultHour}{(numeric) default that is used when colHour=NA, when only days are specified}
  \item{\dots}{further arguments to var.get.nc or ncvar_get, such as start and count}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





