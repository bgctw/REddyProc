\name{LightResponseCurveFitter-class}
\Rdversion{1.1}
\docType{class}
\alias{LightResponseCurveFitter-class}
\alias{LightResponseCurveFitter}

\title{Class \code{"LightResponseCurveFitter"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("LightResponseCurveFitter")
}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{computeLRCGradient(theta, Rg, VPD, Temp, VPD0, fixVPD)}:}{ ~~ }
    \item{\code{predictLRC(theta, Rg, VPD, Temp, VPD0, fixVPD)}:}{ ~~ }
    \item{\code{computeCost(thetaOpt, theta, iOpt, flux, sdFlux, parameterPrior, sdParameterPrior, ..., VPD0, fixVPD)}:}{ ~~ }
    \item{\code{optimLRC(theta, iOpt, sdParameterPrior, isUsingHessian, ...)}:}{ ~~ }
    \item{\code{isParameterInBounds(theta, sdTheta, RRefNight, ctrl)}:}{ ~~ }
    \item{\code{optimLRCBounds(theta0, parameterPrior, ..., lastGoodParameters.V.n, ctrl)}:}{ ~~ }
    \item{\code{getParameterInitials(thetaPrior)}:}{ ~~ }
    \item{\code{getPriorScale(thetaPrior, medianRelFluxUncertainty, nRec, ctrl)}:}{ ~~ }
    \item{\code{getPriorLocation(NEEDay, RRefNight, E0)}:}{ ~~ }
    \item{\code{optimLRCOnAdjustedPrior(theta, iOpt, dsDay, parameterPrior, ctrl, ...)}:}{ ~~ }
    \item{\code{fitLRC(dsDay, E0, sdE0, RRefNight, controlGLPart, lastGoodParameters)}:}{ ~~ }
    \item{\code{getParameterNames()}:}{ ~~ }
  }
}
