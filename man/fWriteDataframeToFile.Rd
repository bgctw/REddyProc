\name{fWriteDataframeToFile}
\alias{fWriteDataframeToFile}
\title{Write data frame to ASCII or NetCDF file}

\usage{fWriteDataframeToFile(Data.F, FileName.s, Dir.s = "", FileType.s = "txt")}
\arguments{
  \item{Data.F}{Data frame}
  \item{FileName.s}{File base name}
  \item{Dir.s}{Directory}
  \item{FileType.s}{File output type}
}
\details{'txt' - for tab delimited text file with header and unit row
'nc' - for very simple NetCDF file of numeric columns
With missing values flagged as -9999.0}
\value{Output of data frame written to file of specified type.}

\author{AMM, KS
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
# Example code
if( file.exists('data/Example_DETha98.txt') ) {
  EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt','data')
  if (FALSE) { #Example code, do not always execute (e.g. on package installation)
    fWriteDataframeToFile(EddyData.F, 'OutputTest', 'out')
    fWriteDataframeToFile(EddyData.F, 'OutputTest', 'out', 'nc')
  }
}
}
