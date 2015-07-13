\name{fInitFilesDir}
\alias{fInitFilesDir}
\title{fInitFilesDir}
\description{Get all available files with specific file extension in directory}
\usage{fInitFilesDir(Dir.s, lFileExt.s)}\arguments{
  \item{Dir.s}{Directory to be searched for files}
  \item{lFileExt.s}{File extension specification}
}

\value{Character vector with names of all available site files.}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
if( FALSE ) { #Do not always execute example code (e.g. on package installation)
  Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
  fInitFilesDir(Dir.s, 'txt')
}
}
