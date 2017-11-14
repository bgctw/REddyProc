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
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





