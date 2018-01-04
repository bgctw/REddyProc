\name{NonrectangularLRCFitter_getParameterNames}
\alias{NonrectangularLRCFitter_getParameterNames}
\title{NonrectangularLRCFitter getParameterNames}
\description{return the parameter names used by this Light Response Curve Function}
\usage{NonrectangularLRCFitter_getParameterNames()}

\value{string vector of parameter names. Positions are important.
Adds sixth parameter, \code{logitconv} to the parameters
of \code{\link{LightResponseCurveFitter_getParameterNames}}
\item{logitconf}{logit-transformed convexity parameter.
The value at original scale is obtained by
\code{conv = 1 / (1 + exp(-logitconv))}
}}

\author{Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{NonrectangularLRCFitter_predictGPP}}}

