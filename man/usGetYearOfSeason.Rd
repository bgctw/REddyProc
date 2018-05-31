\name{usGetYearOfSeason}
\alias{usGetYearOfSeason}
\title{usGetYearOfSeason}
\description{determine the year of the record of middle of seasons}
\usage{usGetYearOfSeason(seasonFactor, sDateTime.v)}
\arguments{
  \item{seasonFactor}{factor vector of length data:
for each record which season it belongs to}
  \item{sDateTime.v}{POSIX.t vector of length data:
for each record: center of half-hour period
(corresponding to sDATA$sDateTime)}
}

\value{named integer vector, with names corresponding to seasons}

\author{TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





