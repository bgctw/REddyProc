\name{getExamplePath}
\alias{getExamplePath}
\title{getExamplePath}
\description{checks if given example filename is existing and if not tries to download it.}
\usage{getExamplePath(filename = "Example_DETha98.txt", 
    exampleDir = .getExampleDir(), isTryDownload = TRUE, 
    remoteDir = "")}
\arguments{
  \item{filename}{the name of the example file}
  \item{exampleDir}{directory where examples are lookd up and downloaded to}
  \item{isTryDownload}{set to FALSE to avoid trying to download examples}
  \item{remoteDir}{the URL where example data are to be downloaded}
}
\details{Example input text data files are not distributed with the package, because
it exceeds allowed package size.
Rather, the example files will be downloaded when required from github by this function.

The remoteDir (github) must be reachable, and the writing directory must be writeable.}
\value{the full path name to the example data or if not available an zero-length character.
Allows to check for if( length(getExamplePath()) ) ...}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
if( FALSE ){ # only for interactive use
	examplePath <- getExamplePath("Example_DETha98.txt")
	if( length(examplePath) ) tmp <- fLoadTXTIntoDataframe(examplePath)
	#test for having no write access to the package directory
	#getExamplePath("Example_DETha98.txt"
	#	, exampleDir = .getExampleDir(package="someNonExistentPackage"))		
}
}
