\name{usEstUstarThresholdSingleFw1Binned}
\alias{usEstUstarThresholdSingleFw1Binned}
\title{usEstUstarThresholdSingleFw1Binned}
\description{estimate the Ustar threshold for single subset, using FW1 algorithm}
\usage{usEstUstarThresholdSingleFw1Binned(Ust_bins.f, 
    ctrlUstarEst.l = usControlUstarEst())}
\arguments{
  \item{Ust_bins.f}{data.frame with columns NEE_avg and Ust_avg, of Ustar bins}
  \item{ctrlUstarEst.l}{parameter list,
see \code{\link{usControlUstarEst}} for defaults and description}
}
\details{Relying on binned NEE and Ustar}

\references{inspired by Papale 2006}
\author{TW, OM
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}





