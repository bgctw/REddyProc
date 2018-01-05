\name{donttest}
\alias{donttest}
\title{donttest}
\description{run a block of code only in interactive R sessions}
\usage{donttest(block, run.donttest = interactive())}
\arguments{
  \item{block}{block of code to be run or not}
  \item{run.donttest}{same as arguemnt in \code{\link{example}}.
Logical scalar whether to evaluate the block of code.
Defaults to TRUE in interactive sessions and FALSE otherwise.}
}
\details{Rd-files use the syntax \code{\\donttest{block}}
, which is latex code (see \code{\link{example}}).
In order to specify the same behaviour with valid R-code
this function can be used. This is necessary, if inlinedocs is used
to generate the Rd files.}
\value{return value of \code{eval(block)} or NULL if not evaluated.}

\author{Thomas Wutzler
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





