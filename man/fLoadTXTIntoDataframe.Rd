\name{fLoadTXTIntoDataframe}
\alias{fLoadTXTIntoDataframe}
\title{Load text file with one header and one unit row into data frame}
\description{If gaps with the flag -9999.0 exist, these are set to NA.}
\usage{fLoadTXTIntoDataframe(FileName.s, Dir.s = "")}
\arguments{
  \item{FileName.s}{File name}
  \item{Dir.s}{Directory}
}
\details{Alternatively \code{\link{fLoadFluxNCIntoDataframe}} loads data from NetCDF-Files.
In addition, \code{\link{fLoadEuroFlux16}} loads data from several annual files in format corresponding to europe-fluxdata 2016.

For using only part of the records, use \code{fFilterAttr} to keep units attributes.}
\value{Data frame with data from text file.}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
if( FALSE ) { #Do not always execute example code (e.g. on package installation)
  Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
  EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt',Dir.s)
}
}
