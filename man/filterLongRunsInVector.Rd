\name{filterLongRunsInVector}
\alias{filterLongRunsInVector}
\title{filterLongRunsInVector}
\description{replace runs of numerically equal values by NA}
\usage{filterLongRunsInVector(x, minNRunLength = 8, 
    replacement = NA, na.rm = TRUE)}
\arguments{
  \item{x}{vector in which to replace long runs}
  \item{minNRunLength}{minimum length of a run to replace.
Defaults to 4 hours in half-hourly spaced data.}
  \item{replacement}{value replacing the original values in long run}
  \item{na.rm}{set to FALSE if NA values interrupt runs}
}

\value{vector \code{x} with long runs replaced by NA}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}





