\name{help_DateTimes}
\alias{help_DateTimes}
\title{help DateTimes}
\description{Overview of functions helping with Timestamps and Dates}
\usage{help_DateTimes()}
\details{Functions helping with preparing and subsetting timestamps:
\itemize{
\item Convert different time formats to POSIX:
\code{\link{fConvertTimeToPosix}}
\item Convert JulianDate format used in Berkeley release to POSIXct:
\code{\link{BerkeleyJulianDateToPOSIXct}}
\item Return the first timestap at (end_of_first_record_in_day) and the
last at midnight:
\code{\link{get_day_boundaries}}
\item Omit records before the start of the first full day and the end of
the last full day:
\code{\link{subset_entire_days}}
\item Subset data.frame to given years respecting the end-of-period
convention: \code{\link{filter_years_eop}}
}

Back to \link{REddyProc-package}.}


\author{TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}





