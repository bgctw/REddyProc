\name{usGetYearOfSeason}
\alias{usGetYearOfSeason}
\title{usGetYearOfSeason}
\description{determine the year of the record of middle of seasons}
\usage{usGetYearOfSeason(seasonFactor.v, sDateTime.v)}
\arguments{
  \item{seasonFactor.v}{factor vector of length data:
for each record which season it belongs to}
  \item{sDateTime.v}{POSIX.t vector of length data:
for each record: center of half-hour period
(corresponding to sDATA$sDateTime)}
}

\value{named integer vector, with names corresponding to seasons}

\author{TW
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}





