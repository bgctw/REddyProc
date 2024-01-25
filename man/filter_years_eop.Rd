\name{filter_years_eop}
\alias{filter_years_eop}
\title{filter years eop}
\description{Subset data.frame to given years respecting the end-of-period convention}
\usage{filter_years_eop(df, years, col_time = "DateTime")}
\arguments{
  \item{df}{data.frame with column col_time of equidistant}
  \item{years}{integer vector of years of the form \code{c(1998, 1998)}}
  \item{col_time}{Name of the column with the equidistant timesteps}
}
\details{The end-of-period (usually end-of-half-hour) convention
in the Fluxnet community results in midnight
and new-year being the last record of the previous day or the year respectively,
although POSIXt function will report the next day or year respectively.

Column attributes such as 'units' are kept.}


\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{help_DateTimes}}, \code{\link{filter_entire_days}}
\code{\link{fKeepColumnAttributes}}}

