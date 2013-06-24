\name{fInitFilesDir}
\alias{fInitFilesDir}
\title{Get all available files with specific file extension in directory}

\usage{fInitFilesDir(Dir.s, lFileExt.s)}
\arguments{
  \item{Dir.s}{Directory to be searched for files}
  \item{lFileExt.s}{File extension specification}
}

\value{Character vector with names of all available site files.}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
# Example code
if( file.exists('data/Example_DETha98.txt') )
  fInitFilesDir('data','txt')
}
