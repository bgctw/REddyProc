\name{fAddNCFVar}
\alias{fAddNCFVar}
\title{fAddNCFVar}
\description{Add variable from NetCDF file to data frame}
\usage{fAddNCFVar(Data.F, Var.s, FileName.s, Dir.s, 
    NcPackage.s, CallFunction.s = "", VarNew.s = Var.s, 
    ...)}
\arguments{
  \item{Data.F}{Data frame}
  \item{Var.s}{Variable name or names (vector of strings)}
  \item{FileName.s}{NetCDF file name as a string}
  \item{Dir.s}{Directory as a string}
  \item{NcPackage.s}{Name (string) of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')}
  \item{CallFunction.s}{Name (string) of function called from}
  \item{VarNew.s}{Name (string) of the variable in Data.F, offer renaming}
  \item{\dots}{further arguments to var.get.nc or ncvar_get, such as start and count}
}

\value{Data frame with new nc variable added.}

\author{AMM, KS, TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





