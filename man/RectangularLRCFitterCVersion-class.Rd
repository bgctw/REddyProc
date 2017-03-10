\name{RectangularLRCFitterCVersion-class}
\Rdversion{1.1}
\docType{class}
\alias{RectangularLRCFitterCVersion-class}
\alias{RectangularLRCFitterCVersion}

\title{Class \code{"RectangularLRCFitterCVersion"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Extends}{
Class \code{"\linkS4class{RectangularLRCFitter}"}, directly.
Class \code{"\linkS4class{NonrectangularLRCFitter}"}, by class "RectangularLRCFitter", distance 2.
Class \code{"\linkS4class{LightResponseCurveFitter}"}, by class "RectangularLRCFitter", distance 3.

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
showClass("RectangularLRCFitterCVersion")
}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{computeCost(thetaOpt, theta, iOpt, flux, sdFlux, parameterPrior, sdParameterPrior, ..., VPD0, fixVPD)}:}{ ~~ }
  }

The following methods are inherited (from the corresponding class):
computeCost ("LightResponseCurveFitter"), computeLRCGradient ("LightResponseCurveFitter"), predictLRC ("LightResponseCurveFitter"), optimLRC ("LightResponseCurveFitter"), isParameterInBounds ("LightResponseCurveFitter"), optimLRCBounds ("LightResponseCurveFitter"), getParameterInitials ("LightResponseCurveFitter"), getPriorScale ("LightResponseCurveFitter"), getPriorLocation ("LightResponseCurveFitter"), getParameterNames ("LightResponseCurveFitter"), getParameterNames ("RectangularLRCFitter"), fitLRC ("LightResponseCurveFitter"), optimLRCOnAdjustedPrior ("LightResponseCurveFitter"), getPriorLocation ("RectangularLRCFitter"), getPriorScale ("RectangularLRCFitter"), getParameterInitials ("NonrectangularLRCFitter"), optimLRCBounds ("RectangularLRCFitter"), isParameterInBounds ("NonrectangularLRCFitter"), optimLRC ("NonrectangularLRCFitter"), predictLRC ("RectangularLRCFitter"), computeLRCGradient ("RectangularLRCFitter"), getParameterNames ("NonrectangularLRCFitter"), getPriorLocation ("NonrectangularLRCFitter"), getPriorScale ("NonrectangularLRCFitter"), optimLRCBounds ("NonrectangularLRCFitter"), predictLRC ("NonrectangularLRCFitter"), computeLRCGradient ("NonrectangularLRCFitter")
}
