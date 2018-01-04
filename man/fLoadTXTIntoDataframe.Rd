\name{fLoadTXTIntoDataframe}
\alias{fLoadTXTIntoDataframe}
\title{Load text file with one header and one unit row into data frame}
\description{If gaps with the flag -9999.0 exist, these are set to NA.}
\usage{fLoadTXTIntoDataframe(FileName.s, Dir.s = "")}
\arguments{
  \item{FileName.s}{File name as a character string}
  \item{Dir.s}{Directory as a character string}
}
\details{Alternatively \code{\link{fLoadFluxNCIntoDataframe}} loads data from NetCDF-Files.
In addition, \code{\link{fLoadEuroFlux16}} loads data from several annual files in format corresponding to europe-fluxdata 2016.

For using only part of the records, use \code{fFilterAttr} to keep units attributes.}
\value{Data frame with data from text file.}

\author{AMM
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}




\examples{
if (FALSE) { #Do not always execute example code (e.g. on package installation)
	examplePath <- getExamplePath('Example_DETha98.txt', TRUE)
	EddyData.F <- fLoadTXTIntoDataframe(examplePath)
}
}
