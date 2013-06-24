\name{fStripFileExtension}
\alias{fStripFileExtension}
\title{Strip file extension }

\usage{fStripFileExtension(lFiles.V.s)}
\arguments{
  \item{lFiles.V.s}{Vector with names of all available site files}
}

\value{Character vector containing the first part of file names (before first dot in file name).}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
# Example code
if( file.exists('data/Example_DETha98.txt') )
  fStripFileExtension(fInitFilesDir('data','txt'))
}
