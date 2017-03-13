\name{NonrectangularLRCFitter_getParameterNames}
\alias{NonrectangularLRCFitter_getParameterNames}
\title{NonrectangularLRCFitter getParameterNames}
\description{return the parameter names used by this Light Response Curve Function}
\usage{NonrectangularLRCFitter_getParameterNames()}

\value{string vector of parameter names. Positions are important.
Adds sixth parameter, \code{logitconv} to the parameters of \code{\link{LightResponseCurveFitter_getParameterNames}} 
\item{logitconf}{logit-transformed convexity parameter. The value at original scale is obtained by \code{conv = 1/(1+exp(-logitconv))}
}}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{NonrectangularLRCFitter_predictGPP}}}

