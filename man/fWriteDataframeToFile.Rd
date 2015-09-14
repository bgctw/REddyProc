\name{fWriteDataframeToFile}
\alias{fWriteDataframeToFile}
\title{fWriteDataframeToFile}
\description{Write data frame to ASCII tab-separated text file}
\usage{fWriteDataframeToFile(Data.F, FileName.s, Dir.s = "", 
    Digits.n = 5)}
\arguments{
  \item{Data.F}{Data frame}
  \item{FileName.s}{File base name}
  \item{Dir.s}{Directory}
  \item{Digits.n}{number of digits, i.e. precision, for numeric values}
}
\details{Missing values are flagged as -9999.0}
\value{Output of data frame written to file of specified type.}

\author{AMM, KS
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
if( FALSE ) { #Do not always execute example code (e.g. on package installation)
  Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
  EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
  fWriteDataframeToFile(EddyData.F, 'OutputTest.txt', 'out')
}
}
