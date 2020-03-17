\name{LightResponseCurveFitter_optimLRCBounds}
\alias{LightResponseCurveFitter_optimLRCBounds}
\title{LightResponseCurveFitter optimLRCBounds}
\description{Optimize parameters with refitting with some fixed parameters if outside bounds}
\usage{LightResponseCurveFitter_optimLRCBounds(theta0, 
    parameterPrior, ..., lastGoodParameters, 
    ctrl)}
\arguments{
  \item{theta0}{initial parameter estimate}
  \item{parameterPrior}{prior estimate of model parameters}
  \item{\dots}{further parameters to \code{.optimLRC}, such as \code{dsDay}}
  \item{lastGoodParameters}{parameters vector of last successful fit}
  \item{ctrl}{list of further controls, such as
\code{isNeglectVPDEffect = TRUE}}
}
\details{If parameters alpha or k are outside bounds (Table A1 in Lasslop 2010),
refit with some parameters fixed
to values from fit of previous window.

No parameters are reported if alpha<0 or RRef < 0 or beta0 < 0
or beta0 > 250

Not parameters are reported if the data did not contain records that
are near light saturation.
This is checked by comparing the prediction at highest PAR with the
beta parameter}
\value{list result of optimization as of
\code{\link{LightResponseCurveFitter_optimLRCOnAdjustedPrior}} with entries
\item{theta}{ numeric parameter vector that includes the fixed components}
\item{iOpt}{ integer vector of indices of the vector that have been optimized}
\item{convergence}{ scalar integer indicating bad conditions on fitting
(see \code{\link{LightResponseCurveFitter_fitLRC}}) }}

\author{TW, MM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{LightResponseCurveFitter_fitLRC}}}

