\name{fStripFileExtension}
\alias{fStripFileExtension}
\title{fStripFileExtension}
\description{Strip file extension }
\usage{fStripFileExtension(lFiles.V.s)}\arguments{
  \item{lFiles.V.s}{Vector with names of all available site files}
}

\value{Character vector containing the first part of file names (before first dot in file name).}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
if( FALSE ) { #Do not always execute example code (e.g. on package installation)
  Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
  fStripFileExtension(fInitFilesDir(Dir.s, 'txt'))
}
}
