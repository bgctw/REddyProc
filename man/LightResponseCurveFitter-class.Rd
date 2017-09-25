\name{LightResponseCurveFitter-class}
\Rdversion{1.1}
\docType{class}
\alias{LightResponseCurveFitter-class}
% \alias{LightResponseCurveFitter}

\title{Class \code{"LightResponseCurveFitter"}}
\description{
Base class for fitting parameters to light response curves (LRC)

Concrete classes for the following LRC functions are available:
\itemize{
\item common rectangular hyperbolic light-response: \code{\link{RectangularLRCFitter-class}}
\item nonrectangular hyperbolic light-response: \code{\link{NonrectangularLRCFitter-class}}
\item logistic sigmoid light-response: \code{\link{LogisticSigmoidLRCFitter-class}}
}

They mostly differ in their prediction of GPP by method \code{\link{LightResponseCurveFitter_predictGPP}}.
} % end description

\section{Extends}{
All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.
}
\author{TW}

\keyword{classes}
\section{Methods}{
  \describe{
    \item{\code{\link{LightResponseCurveFitter_computeLRCGradient}(theta, Rg, VPD, Temp, VPD0, fixVPD, TRef)}:}{  }
    \item{\code{\link{LightResponseCurveFitter_predictGPP}(Rg, ...)}:}{  }
    \item{\code{\link{LightResponseCurveFitter_predictLRC}(theta, Rg, VPD, Temp, VPD0, fixVPD, TRef)}:}{  }
    \item{\code{\link{LightResponseCurveFitter_computeCost}(thetaOpt, theta, iOpt, flux, sdFlux, parameterPrior, sdParameterPrior, ...)}:}{  }
    \item{\code{\link{LightResponseCurveFitter_optimLRC}(theta, iOpt, sdParameterPrior, ..., ctrl, isUsingHessian)}:}{  }
    \item{\code{\link{LightResponseCurveFitter_isParameterInBounds}(theta, sdTheta, RRefNight, ctrl)}:}{  }
    \item{\code{\link{LightResponseCurveFitter_optimLRCOnAdjustedPrior}(theta, iOpt, dsDay, parameterPrior, ctrl, ...)}:}{  }
    \item{\code{\link{LightResponseCurveFitter_getOptimizedParameterPositions}(isUsingFixedVPD, isUsingFixedAlpha)}:}{  }
    \item{\code{\link{LightResponseCurveFitter_optimLRCBounds}(theta0, parameterPrior, ..., lastGoodParameters, ctrl)}:}{  }
    \item{\code{\link{LightResponseCurveFitter_getParameterInitials}(thetaPrior)}:}{  }
    \item{\code{\link{LightResponseCurveFitter_getPriorScale}(thetaPrior, medianRelFluxUncertainty, nRec, ctrl)}:}{  }
    \item{\code{\link{LightResponseCurveFitter_getPriorLocation}(NEEDay, RRefNight, E0)}:}{  }
    \item{\code{\link{LightResponseCurveFitter_fitLRC}(dsDay, E0, sdE0, RRefNight, controlGLPart, lastGoodParameters)}:}{  }
    \item{\code{\link{LightResponseCurveFitter_getParameterNames}()}:}{  }
  }
}
