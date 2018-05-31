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
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}




\examples{
(Dir.s <- tempdir())   # directory where output is written to
fWriteDataframeToFile(Example_DETha98, 'OutputTest.txt', Dir.s = Dir.s)
}
