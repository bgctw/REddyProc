\name{getExamplePath}
\alias{getExamplePath}
\title{getExamplePath}
\description{checks if example filename is existing and if not tries to download it.}
\usage{getExamplePath(filename = "Example_DETha98.txt", 
    isTryDownload = FALSE, exampleDir = getREddyProcExampleDir(), 
    remoteDir = "")}
\arguments{
  \item{filename}{the name of the example file}
  \item{isTryDownload}{scalar logical whether to try
downloading the file to package or tmp directory.
Because of CRAN checks, need to explicitly set to TRUE}
  \item{exampleDir}{directory where examples are
looked up and downloaded to}
  \item{remoteDir}{the URL do download from}
}
\details{Example input text data files are not distributed with the package, because
it exceeds allowed package size.
Rather, the example files will be downloaded when required from github
by this function.

The remoteDir (github) must be reachable, and the writing directory
must be writeable.}
\value{the full path name to the example data or if not available
an zero-length character.
Allows to check for \code{if (length(getExamplePath()) ) ... }}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





