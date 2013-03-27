\name{fLoadTXTIntoDataframe}
\alias{fLoadTXTIntoDataframe}
\title{Load text file with one header and one (discarded) unit row into data frame}
\description{If gaps with the flag -9999.0 exist, these are set to NA.}
\usage{
fLoadTXTIntoDataframe(FileName.s,
Dir.s)
}
\arguments{
  \item{FileName.s}{File name}
  \item{Dir.s}{Directory}
}

\value{Data frame with data from text file.}

\author{AMM
(Antje M. Moffat, Kerstin Sickel, Thomas Wutzler)}




\examples{
# Example code
if( file.exists('data/Example_DETha98.txt') )
  EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt','data')
}
