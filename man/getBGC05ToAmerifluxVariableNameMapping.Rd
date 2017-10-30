\name{getBGC05ToAmerifluxVariableNameMapping}
\alias{getBGC05ToAmerifluxVariableNameMapping}
\title{getBGC05ToAmerifluxVariableNameMapping}
\description{Get a mapping of variable names of REddyProc defaults to names of the Berkeley 2016 release of the Fluxnet data}
\usage{getBGC05ToAmerifluxVariableNameMapping(map = character(), 
    mapDefault = c(Year = "YEAR", DoY = "DOY", 
        Rg = "SW_IN", Tair = "TA", Tsoil = "TS", 
        rH = "RH", VPD = "VPD", Ustar = "USTAR", 
        NEE_orig = "NEE_PI", H_orig = "H_PI", 
        LE_orig = "LE_PI", NEE_f = "NEE_F", 
        H_f = "H_F", LE_f = "LE_F", NEE_fqc = "NEE_QC", 
        H_fqc = "H_QC", LE_fqc = "LE_QC"))}
\arguments{
  \item{map}{named character vector: additional mapping, that extends or overwrites defaults in \code{mapDefault}}
  \item{mapDefault}{named character vector: default mapping}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{renameVariablesInDataframe}}}
\examples{
# adding mapping of foo, and overwriting mapping of DoY
getBGC05ToAmerifluxVariableNameMapping(c(foo="FOO",DoY="doy"))
}
