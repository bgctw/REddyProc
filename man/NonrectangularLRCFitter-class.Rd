\name{NonrectangularLRCFitter-class}
\Rdversion{1.1}
\docType{class}
\alias{NonrectangularLRCFitter-class}
\alias{NonrectangularLRCFitter}

\title{Class \code{"NonrectangularLRCFitter"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Extends}{
Class \code{"\linkS4class{LightResponseCurveFitter}"}, directly.

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
showClass("NonrectangularLRCFitter")
}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{computeGPPGradient(Rg, Amax, alpha, logitconv)}:}{ ~~ }
    \item{\code{getParameterNames()}:}{ ~~ }
    \item{\code{getPriorLocation(NEEDay, RRefNight, E0)}:}{ ~~ }
    \item{\code{getPriorScale(thetaPrior, medianRelFluxUncertainty, nRec, ctrl)}:}{ ~~ }
    \item{\code{getOptimizedParameterPositions(isUsingFixedVPD, isUsingFixedAlpha)}:}{ ~~ }
    \item{\code{predictLRC(theta, Rg, VPD, Temp, VPD0, fixVPD)}:}{ ~~ }
    \item{\code{predictGPP(Rg, Amax, alpha, conv)}:}{ ~~ }
    \item{\code{computeLRCGradient(theta, Rg, VPD, Temp, VPD0, fixVPD, TRef)}:}{ ~~ }
  }

The following methods are inherited (from the corresponding class):
computeLRCGradient ("LightResponseCurveFitter"), predictGPP ("LightResponseCurveFitter"), predictLRC ("LightResponseCurveFitter"), getOptimizedParameterPositions ("LightResponseCurveFitter"), getPriorScale ("LightResponseCurveFitter"), getPriorLocation ("LightResponseCurveFitter"), getParameterNames ("LightResponseCurveFitter"), fitLRC ("LightResponseCurveFitter"), optimLRCOnAdjustedPrior ("LightResponseCurveFitter"), getParameterInitials ("LightResponseCurveFitter"), optimLRCBounds ("LightResponseCurveFitter"), isParameterInBounds ("LightResponseCurveFitter"), optimLRC ("LightResponseCurveFitter"), computeCost ("LightResponseCurveFitter")
}
