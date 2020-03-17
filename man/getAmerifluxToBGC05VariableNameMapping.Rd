\name{getAmerifluxToBGC05VariableNameMapping}
\alias{getAmerifluxToBGC05VariableNameMapping}
\title{getAmerifluxToBGC05VariableNameMapping}
\description{map Ameriflux variable names to REddyProc defaults to names}
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
  \item{map}{named character vector: additional mapping,
that extends or overwrites defaults in \code{mapDefault}}
  \item{mapDefault}{named character vector: default mapping}
}
\details{Get a mapping of variable names of Ameriflux
(Berkley 2016 Fluxnet release)
to of REddyProc defaults to names}


\author{TW,
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{renameVariablesInDataframe}}}

