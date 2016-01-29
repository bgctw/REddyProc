\name{fAddNCFVar}
\alias{fAddNCFVar}
\title{fAddNCFVar}
\description{Add variable from NetCDF file to data frame}
\usage{fAddNCFVar(Data.F, Var.s, FileName.s, Dir.s, NcPackage.s, 
    CallFunction.s = "", VarNew.s = Var.s, ...)}
\arguments{
  \item{Data.F}{Data frame}
  \item{Var.s}{Variable name or names}
  \item{FileName.s}{NetCDF file name}
  \item{Dir.s}{Directory}
  \item{NcPackage.s}{Name of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')}
  \item{CallFunction.s}{Name of function called from}
  \item{VarNew.s}{Name of the variable in Data.F, offer renaming}
  \item{\dots}{further arguments to var.get.nc or ncvar_get, such as start and count}
}

\value{Data frame with new nc variable added.}

\author{AMM, KS, TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





