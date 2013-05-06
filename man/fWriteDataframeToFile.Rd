\name{fWriteDataframeToFile}
\alias{fWriteDataframeToFile}
\title{Write data frame to ASCII or NetCDF file}

\usage{fWriteDataframeToFile(Data.F, BaseName.s, Dir.s, FileType.s = "txt", 
    OutputName.s = "none")}
\arguments{
  \item{Data.F}{Data frame}
  \item{BaseName.s}{File base name}
  \item{Dir.s}{Directory}
  \item{FileType.s}{File output type}
  \item{OutputName.s}{Optional: Alternative complete file name}
}
\details{'txt' - for tab delimited text file
'nc' - for NetCDF file
With missing values flagged as -9999.0}
\value{Output of data frame written to file of specified type.}

\author{AMM, KS
(Antje M. Moffat, Kerstin Sickel, Thomas Wutzler)}




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
