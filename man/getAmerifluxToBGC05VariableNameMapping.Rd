\name{getAmerifluxToBGC05VariableNameMapping}
\alias{getAmerifluxToBGC05VariableNameMapping}
\title{getAmerifluxToBGC05VariableNameMapping}
\description{Get a mapping of variable names of the Berkeley 2016 release of the Fluxnet to of REddyProc defaults to names 		}
\usage{getAmerifluxToBGC05VariableNameMapping(map = character(), 
    mapDefault = c(YEAR = "Year", DOY = "DoY", 
        NEE = "NEE", LE = "LE", H = "H", 
        SW_IN = "Rg", TA = "Tair", TS = "Tsoil", 
        RH = "rH", VPD = "VPD", USTAR = "Ustar", 
        NEE_PI = "NEE_orig", H_PI = "H_orig", 
        LE_PI = "LE_orig", NEE_F = "NEE_f", 
        H_F = "H_f", LE_F = "LE_f", NEE_QC = "NEE_fqc", 
        H_QC = "H_fqc", LE_QC = "LE_fqc"))}
\arguments{
  \item{map}{named character vector: additional mapping, that extends or overwrites defaults in \code{mapDefault}}
  \item{mapDefault}{named character vector: default mapping}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{renameVariablesInDataframe}}}

