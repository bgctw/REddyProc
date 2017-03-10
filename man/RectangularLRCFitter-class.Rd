\name{RectangularLRCFitter-class}
\Rdversion{1.1}
\docType{class}
\alias{RectangularLRCFitter-class}
\alias{RectangularLRCFitter}

\title{Class \code{"RectangularLRCFitter"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Extends}{
Class \code{"\linkS4class{NonrectangularLRCFitter}"}, directly.
Class \code{"\linkS4class{LightResponseCurveFitter}"}, by class "NonrectangularLRCFitter", distance 2.

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
showClass("RectangularLRCFitter")
}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{computeLRCGradient(theta, Rg, VPD, Temp, VPD0, fixVPD)}:}{ ~~ }
    \item{\code{predictLRC(theta, Rg, VPD, Temp, VPD0, fixVPD)}:}{ ~~ }
    \item{\code{optimLRCBounds(theta0, parameterPrior, ..., lastGoodParameters.V.n, ctrl)}:}{ ~~ }
    \item{\code{getPriorScale(thetaPrior, medianRelFluxUncertainty, nRec, ctrl)}:}{ ~~ }
    \item{\code{getPriorLocation(NEEDay, RRefNight, E0)}:}{ ~~ }
    \item{\code{getParameterNames()}:}{ ~~ }
  }

The following methods are inherited (from the corresponding class):
computeLRCGradient ("NonrectangularLRCFitter"), predictLRC ("NonrectangularLRCFitter"), optimLRCBounds ("NonrectangularLRCFitter"), getPriorScale ("NonrectangularLRCFitter"), getPriorLocation ("NonrectangularLRCFitter"), getParameterNames ("NonrectangularLRCFitter"), computeCost ("LightResponseCurveFitter"), optimLRC ("NonrectangularLRCFitter"), isParameterInBounds ("NonrectangularLRCFitter"), getParameterInitials ("NonrectangularLRCFitter"), optimLRCOnAdjustedPrior ("LightResponseCurveFitter"), fitLRC ("LightResponseCurveFitter"), getParameterNames ("LightResponseCurveFitter"), getPriorLocation ("LightResponseCurveFitter"), getPriorScale ("LightResponseCurveFitter"), getParameterInitials ("LightResponseCurveFitter"), optimLRCBounds ("LightResponseCurveFitter"), isParameterInBounds ("LightResponseCurveFitter"), optimLRC ("LightResponseCurveFitter"), predictLRC ("LightResponseCurveFitter"), computeLRCGradient ("LightResponseCurveFitter")
}
