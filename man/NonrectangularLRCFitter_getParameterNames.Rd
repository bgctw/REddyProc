\name{NonrectangularLRCFitter_getParameterNames}
\alias{NonrectangularLRCFitter_getParameterNames}
\title{NonrectangularLRCFitter getParameterNames}
\description{return the parameter names used by this Light Response Curve Funciton}
\usage{NonrectangularLRCFitter_getParameterNames()}

\value{string vector of parameter names. Positions are important.
\item{k}{TODO}
\item{beta}{saturation of GPP at high radiation}
\item{alpha}{initial slope}
\item{RRef}{basal respiration}
\item{E0}{temperature sensitivity estimated from night-time data}
\item{logitconf}{logit-transformed convexity parameter. The valua at original scale is obtained by conv = 1/(1+exp(-logitconv))
}}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





