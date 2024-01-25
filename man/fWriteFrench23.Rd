\name{fWriteFrench23}
\alias{fWriteFrench23}
\title{fWriteFrench23}
\description{Write data frame to ASCII comma-separated text file}
\usage{fWriteFrench23(data, filename, isSplitDatetime = FALSE, 
    digits = 5)}
\arguments{
  \item{data}{Data frame to be exported, with unit attributes attached to columns}
  \item{filename}{(string)  name (including path) of the output file}
  \item{isSplitDatetime}{set to TRUE to create columns Year, DoY and Hour}
  \item{digits}{(integer) number of digits, i.e. precision, for numeric values}
}
\details{Writes data.frame as comma-separated file after two header rows.

The first header row contains the column names, and the second units.

Spaces in column names are replaced by underscore and \% is replaced by
the word percent.}


\author{TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{fWriteDataframeToFile}}}

