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
  \item{FileName.s}{NetCDF file name}
  \item{Dir.s}{Directory}
  \item{NcPackage.s}{Name of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')}
  \item{CallFunction.s}{Name of function called from}
  \item{colYear}{Name of variable holding the year}
  \item{colMonth}{Name of variable holding the month}
  \item{colDay}{Name of variable holding the day}
  \item{colHour}{Name of variable holding the hour}
  \item{defaultHour}{default used when colHour=NA, when only days are specified}
  \item{\dots}{further arguments to var.get.nc or ncvar_get, such as start and count}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





