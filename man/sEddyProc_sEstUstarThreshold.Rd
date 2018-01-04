\name{sEddyProc_sEstUstarThreshold}
\alias{sEddyProc_sEstUstarThreshold}
\title{sEddyProc$sEstUstarThreshold - Estimating ustar threshold}
\description{Calling \code{\link{usEstUstarThreshold}} for class data and storing results}
\usage{sEddyProc_sEstUstarThreshold(UstarColName = "Ustar", 
    NEEColName = "NEE", TempColName = "Tair", 
    RgColName = "Rg", ...)}
\arguments{
  \item{UstarColName}{column name for UStar}
  \item{NEEColName}{column name for NEE}
  \item{TempColName}{column name for air temperature}
  \item{RgColName}{column name for solar radiation for
omitting night time data}
  \item{\dots}{further arguments to
\code{\link{usEstUstarThreshold}}}
}

\value{restult of \code{\link{usEstUstarThreshold}}. In addition the
result is stored in class variable sUSTAR and the bins as
additional columsn to sDATA}

\author{TW
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}





