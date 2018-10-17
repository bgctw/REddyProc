\name{filterLongRuns}
\alias{filterLongRuns}
\title{filterLongRuns}
\description{replace runs, i.e sequences of numerically equal values, by NA}
\usage{filterLongRuns(data, colNames, ...)}
\arguments{
  \item{data}{data.frame with columns to filter}
  \item{colNames}{string vector of names indicating which columns to filter}
  \item{\dots}{further arguments to \code{\link{filterLongRunsInVector}}
such as \code{minNRunLength}.}
}
\details{Longer runs, i.e. sequences of numerically identical values,
in a series of measurments hint to
problems during a noisy measurement, e.g. by sensor malfunction due to
freezing.
This function, replaces such values in such runs to indicate missing values.}
\value{data.frame \code{ans} with long runs in specified columns replaced by NA}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





