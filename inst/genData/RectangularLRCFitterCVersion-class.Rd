\name{RectangularLRCFitterCVersion-class}
\Rdversion{1.1}
\docType{class}
\alias{RectangularLRCFitterCVersion-class}
%\alias{RectangularLRCFitterCVersion}

\title{Class \code{"RectangularLRCFitterCVersion"}}

\description{
Common rectangular hyperbolic light-response curve fitting, implemented with faster C-based cost function.
}
\section{Extends}{
Class \code{"\linkS4class{RectangularLRCFitter}"}, directly.
Class \code{"\linkS4class{LightResponseCurveFitter}"}, by class "RectangularLRCFitter", distance 2.

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{computeCost(thetaOpt, theta, iOpt, flux, sdFlux, parameterPrior, sdParameterPrior, ..., VPD0, fixVPD)}:}{ ~~ }
  }

The following methods are inherited (from the corresponding class):
computeCost ("LightResponseCurveFitter"), computeLRCGradient ("LightResponseCurveFitter"), predictGPP ("RectangularLRCFitter"), predictLRC ("LightResponseCurveFitter"), optimLRC ("LightResponseCurveFitter"), isParameterInBounds ("LightResponseCurveFitter"), optimLRCOnAdjustedPrior ("LightResponseCurveFitter"), getOptimizedParameterPositions ("LightResponseCurveFitter"), optimLRCBounds ("LightResponseCurveFitter"), getParameterInitials ("LightResponseCurveFitter"), getPriorScale ("LightResponseCurveFitter"), getPriorLocation ("LightResponseCurveFitter"), fitLRC ("LightResponseCurveFitter"), getParameterNames ("LightResponseCurveFitter"), predictGPP ("LightResponseCurveFitter"), computeGPPGradient ("RectangularLRCFitter")
}
