\name{fWriteDataframeToFile}
\alias{fWriteDataframeToFile}
\title{fWriteDataframeToFile}
\description{Write data frame to ASCII tab-separated text file}
\usage{fWriteDataframeToFile(Data.F, FileName = FileName.s, 
    Dir = if (!missing(Dir.s)) Dir.s else "", 
    Digits = if (!missing(Digits.n)) Digits.n else 5, 
    FileName.s, Dir.s, Digits.n)}
\arguments{
  \item{Data.F}{Data frame}
  \item{FileName}{File base name as a string}
  \item{Dir}{Directory as a string}
  \item{Digits}{
}
  \item{FileName.s}{deprecated}
  \item{Dir.s}{deprecated}
  \item{Digits.n}{deprecated}
}
\details{Missing values are flagged as -9999.0}
\value{Output of data frame written to file of specified type.}

\author{AMM, KS
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}




\examples{
(Dir <- tempdir())   # directory where output is written to
fWriteDataframeToFile(Example_DETha98, 'OutputTest.txt', Dir = Dir)
}
