\name{NonrectangularLRCFitter-class}
\Rdversion{1.1}
\docType{class}
\alias{NonrectangularLRCFitter-class}
% \alias{NonrectangularLRCFitter}

\title{Class \code{"NonrectangularLRCFitter"}}
\description{
Nonrectangular hyperbolic light-response curve fitting.
}
\section{Extends}{
Class \code{"\linkS4class{LightResponseCurveFitter}"}, directly.

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}

\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{computeGPPGradient(Rg, Amax, alpha, logitconv)}:}{ ~~ }
    \item{\code{getParameterNames()}:}{ ~~ }
    \item{\code{getPriorLocation(NEEDay, RRefNight, E0)}:}{ ~~ }
    \item{\code{getPriorScale(thetaPrior, medianRelFluxUncertainty, nRec, ctrl)}:}{ ~~ }
    \item{\code{getOptimizedParameterPositions(isUsingFixedVPD, isUsingFixedAlpha)}:}{ ~~ }
    \item{\code{predictLRC(theta, Rg, VPD, Temp, VPD0, fixVPD, TRef)}:}{ ~~ }
    \item{\code{predictGPP(Rg, Amax, alpha, conv)}:}{ ~~ }
    \item{\code{computeLRCGradient(theta, Rg, VPD, Temp, VPD0, fixVPD, TRef)}:}{ ~~ }
  }

The following methods are inherited (from the corresponding class):
computeLRCGradient ("LightResponseCurveFitter"), predictGPP ("LightResponseCurveFitter"), predictLRC ("LightResponseCurveFitter"), getOptimizedParameterPositions ("LightResponseCurveFitter"), getPriorScale ("LightResponseCurveFitter"), getPriorLocation ("LightResponseCurveFitter"), getParameterNames ("LightResponseCurveFitter"), fitLRC ("LightResponseCurveFitter"), getParameterInitials ("LightResponseCurveFitter"), optimLRCBounds ("LightResponseCurveFitter"), optimLRCOnAdjustedPrior ("LightResponseCurveFitter"), isParameterInBounds ("LightResponseCurveFitter"), optimLRC ("LightResponseCurveFitter"), computeCost ("LightResponseCurveFitter")
}
