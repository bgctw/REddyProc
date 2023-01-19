\name{get_day_boundaries}
\alias{get_day_boundaries}
\title{get day boundaries}
\description{Return the first timestamp in the vector at 00:30 and the last at 00:00}
\usage{get_day_boundaries(dt)}
\arguments{
  \item{dt}{vector of equidistant POSIXt timestamps with several records a day, usually 48}
}



\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{fConvertTimeToPosix}}, \code{\link{subset_entire_days}}}

