\name{fWriteDataframeToFile}
\alias{fWriteDataframeToFile}
\title{Write data frame to ASCII tab-separated text file}

\usage{fWriteDataframeToFile(Data.F, FileName.s, Dir.s = "")}
\arguments{
  \item{Data.F}{Data frame}
  \item{FileName.s}{File base name}
  \item{Dir.s}{Directory}
}
\details{Missing values are flagged as -9999.0}
\value{Output of data frame written to file of specified type.}

\author{AMM, KS
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
# Example code
if (FALSE) { #Example code, do not always execute (e.g. on package installation)
if( file.exists('data/Example_DETha98.txt') ) {
    EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt','data')
    fWriteDataframeToFile(EddyData.F, 'OutputTest.txt', 'out')
  }
}
}
