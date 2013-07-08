\name{fLoadTXTIntoDataframe}
\alias{fLoadTXTIntoDataframe}
\title{Load text file with one header and one unit row into data frame}
\description{If gaps with the flag -9999.0 exist, these are set to NA.}
\usage{fLoadTXTIntoDataframe(FileName.s, Dir.s = "")}
\arguments{
  \item{FileName.s}{File name}
  \item{Dir.s}{Directory}
}

\value{Data frame with data from text file.}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
if( FALSE ) { #Do not always execute example code (e.g. on package installation)
  Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
  EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt',Dir.s)
}
}
