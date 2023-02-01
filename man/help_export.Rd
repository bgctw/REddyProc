\name{help_export}
\alias{help_export}
\title{help export}
\description{Overview of functions helping with exporting Data and Results}
\usage{help_export()}
\details{Functions helping with exporting data
\itemize{
\item Export Input data from REddyProc class:
\code{\link{sEddyProc_sExportData}}
\item Export Computed results from REddyProc class:
\code{\link{sEddyProc_sExportResults}}
\item Write data frame to ASCII tab-separated text file:
\code{\link{fWriteDataframeToFile}}
}

Writing a file that can be supplied to the REddyProc webservice at
MPI-BGC Jena can be done by exporting data from REddyProc class \code{EProc}.

\preformatted{
df <- EProc$sExportData()
fWriteDataframeToFile(df, "myfilename.txt", isSplitDatetime = TRUE)
}

For preparing minimal working examples also consider
\itemize{
\item Omit records before the start of the first full day and the end of
the last full day:
\code{df <- \link{subset_entire_days}(df)}
\item Subset data.frame to one or two years:
\code{df <- \link{filter_years_eop}(df, c(1998))}
}

There are several functions that import from file of different formats.
\itemize{
\item Load text file with one header and one unit row into data frame:
\code{\link{fLoadTXTIntoDataframe}}
\item Reads sequence of annual files in the format of Europe-fluxdata 2016:
\code{\link{fLoadEuroFlux16}}
\item Read basic variables from Ameriflux standard (as of 2022) files:
\code{\link{fLoadAmeriflux22}}
\item Read NetCDF files -> moved to separate package REddyProcNCDF
(https://github.com/bgctw/REddyProcNCDF)
}

Back to \link{REddyProc-package}.}


\author{TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}





