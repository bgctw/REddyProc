\name{filter_entire_days}
\alias{filter_entire_days}
\title{filter entire days}
\description{Omit records before the start of the first full day and the end of the last full day}
\usage{filter_entire_days(df, col_time = "DateTime")}
\arguments{
  \item{df}{data.frame with column col_time of equidistant}
  \item{col_time}{Name of the column with the equidistant timesteps}
}
\details{Column attributes such as 'units' are kept.}


\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{help_DateTimes}}, \code{\link{get_day_boundaries}}
\code{\link{fKeepColumnAttributes}}}

