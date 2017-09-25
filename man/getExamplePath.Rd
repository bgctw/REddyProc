\name{getExamplePath}
\alias{getExamplePath}
\title{getExamplePath}
\description{checks if given example filename is existing and if not tries to download it.}
\usage{getExamplePath(filename = "Example_DETha98.txt", 
    dir = paste(system.file(package = "REddyProc"), 
        "examples", sep = "/"), isTryDownload = TRUE, 
    remoteDir = "https://github.com/bgctw/REddyProc/raw/master/examples")}
\arguments{
  \item{filename}{
}
  \item{dir}{
}
  \item{isTryDownload}{
}
  \item{remoteDir}{
}
}
\details{Example input text data files are not distributed with the package, because
it exceeds allowed package size.
Rather, the example files will be downloaded when required from github by this function.

The remoteDir (github) must be reachable, and the writing directory must be writeable.
If the default dir (below the installed REddyProc package) is not writeable for the user, 
the user can provide an alternative directory with argument \code{dir}.}
\value{the full path name to the example data or if not available an zero-length character.
Allows to check for if( length(existsExampleFile()) ) ...}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





