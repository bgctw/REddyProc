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

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{NonrectangularLRCFitter_predictGPP}}}

