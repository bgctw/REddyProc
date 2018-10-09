\name{filterLongRunsInVector}
\alias{filterLongRunsInVector}
\title{filterLongRunsInVector}
\description{replace runs of numerically equal values by NA}
\usage{filterLongRunsInVector(x, minNRunLength = 2, 
    replacement = NA, na.rm = TRUE)}
\arguments{
  \item{x}{vector in which to replace long runs}
  \item{minNRunLength}{minimum run length to report}
  \item{replacement}{value replacing values in runs}
  \item{na.rm}{set to FALSE if NA values interrupt runs}
}

\value{vector \code{x} with long runs replaced by NA}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





