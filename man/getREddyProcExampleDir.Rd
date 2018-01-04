\name{getREddyProcExampleDir}
\alias{getREddyProcExampleDir}
\title{getREddyProcExampleDir}
\description{get the example directory inside temporary directory}
\usage{getREddyProcExampleDir(isPreferParentDir = identical(Sys.getenv("NOT_CRAN"), 
    "true"), subDir = "REddyProcExamples")}
\arguments{
  \item{isPreferParentDir}{logical scalar, wheter to prefer
temp parent directory instead of the R-session temp-Directory.
See details.}
  \item{subDir}{the name of the subdirectory inside the
tmp directory, where examples are stored}
}
\details{If \code{isPreferParentDir = FALSE} (the default),
the examples will be downloaded again for
each new R-session in a session specific directory as given by
\code{\link{tempdir}}. This corresponds to CRAN policy.
IF TRUE, the parent of \code{\link{tempdir}} will be used, so that
downloads of examples are preserved across R-sessions.
This is the default if
environement variable "NOT_CRAN" is defined,
when running from testthat::\code{\link{check}}.}


\author{Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{getExamplePath}}}
\examples{
# R session specific
getREddyProcExampleDir()
# outisde R-session specific
getREddyProcExampleDir(TRUE)
}