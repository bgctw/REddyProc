\name{usEstUstarThresholdSingleFw2Binned}
\alias{usEstUstarThresholdSingleFw2Binned}
\title{usEstUstarThresholdSingleFw2Binned}
\description{estimate the Ustar threshold for single subset, using FW2 algorithm}
\usage{usEstUstarThresholdSingleFw2Binned(Ust_bins.f, 
    ctrlUstarEst.l = usControlUstarEst())}
\arguments{
  \item{Ust_bins.f}{data.frame with column s NEE_avg and Ust_avg, of Ustar bins}
  \item{ctrlUstarEst.l}{parameter list,
see \code{\link{usControlUstarEst}} for defaults and description}
}
\details{Demand that threshold is higher than \code{ctrlUstarEst.l$minNuStarPlateau}
records. If fewer records}


\author{TW, OM
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





