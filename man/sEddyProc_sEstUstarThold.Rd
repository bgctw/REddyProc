\name{sEddyProc_sEstUstarThold}
\alias{sEddyProc_sEstUstarThold}
\title{sEddyProc$sEstUstarThreshold - Estimating ustar threshold}
\description{Calling \code{\link{usEstUstarThreshold}} for class data and storing results}
\usage{sEddyProc_sEstUstarThold(UstarColName = "Ustar", 
    NEEColName = "NEE", TempColName = "Tair", 
    RgColName = "Rg", ..., seasonFactor = usCreateSeasonFactorMonth(sDATA$sDateTime))}
\arguments{
  \item{UstarColName}{column name for UStar}
  \item{NEEColName}{column name for NEE}
  \item{TempColName}{column name for air temperature}
  \item{RgColName}{column name for solar radiation for
omitting night time data}
  \item{\dots}{further arguments to
\code{\link{usEstUstarThreshold}}}
  \item{seasonFactor}{
}
}

\value{result component \code{uStarTh} of \code{\link{usEstUstarThreshold}}.
In addition the result is stored in class variable \code{sUSTAR_DETAILS}.}

\author{TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}





