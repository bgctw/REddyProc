\name{fWriteDataframeToFile}
\alias{fWriteDataframeToFile}
\title{fWriteDataframeToFile}
\description{Write data frame to ASCII tab-separated text file}
\usage{fWriteDataframeToFile(Data.F, FileName.s, 
    Dir.s = "", Digits.n = 5)}
\arguments{
  \item{Data.F}{Data frame}
  \item{FileName.s}{File base name as a string}
  \item{Dir.s}{Directory as a string}
  \item{Digits.n}{(integer) number of digits, i.e. precision, for numeric values}
}
\details{Missing values are flagged as -9999.0}
\value{Output of data frame written to file of specified type.}

\author{AMM, KS
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}




\examples{
if (FALSE) { #Do not always execute example code (e.g. on package installation)
  Dir.s <- paste(system.file(package = 'REddyProc'), 'examples', sep = ' / ')
  EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
  fWriteDataframeToFile(EddyData.F, 'OutputTest.txt', 'out')
}
}
