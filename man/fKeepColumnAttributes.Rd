\name{fKeepColumnAttributes}
\alias{fKeepColumnAttributes}
\title{fKeepColumnAttributes}
\description{Copy column attributes after processing a data.frame}
\usage{fKeepColumnAttributes(x, FUN, ...)}
\arguments{
  \item{x}{data.frame to be processed}
  \item{FUN}{\code{function(x::data.frame, ...) -> data.frame} to be applied}
  \item{\dots}{additional arguments to FUN}
}
\details{The columns of the resulting data.frame that match a column name in x
will get the same attributes as in x.}
\value{result of \code{function(x, ...)} with column attributes preserved}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}





