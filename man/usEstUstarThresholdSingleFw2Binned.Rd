\name{usEstUstarThresholdSingleFw2Binned}
\alias{usEstUstarThresholdSingleFw2Binned}
\title{usEstUstarThresholdSingleFw2Binned}
\description{estimate the Ustar threshold for single subset, using FW2 algorithm}
\usage{usEstUstarThresholdSingleFw2Binned(Ust_bins.f, 
    ctrlUstarEst = usControlUstarEst())}
\arguments{
  \item{Ust_bins.f}{data.frame with column s NEE_avg and Ust_avg, of Ustar bins}
  \item{ctrlUstarEst}{parameter list,
see \code{\link{usControlUstarEst}} for defaults and description}
}
\details{Demand that threshold is higher than \code{ctrlUstarEst$minNuStarPlateau}
records. If fewer records}


\author{TW, OM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





