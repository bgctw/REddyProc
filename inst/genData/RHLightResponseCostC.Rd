\name{RHLightResponseCostC}
\alias{RHLightResponseCostC}
\alias{_REddyProc_RHLightResponseCostC}
\title{RHLightResponseCostC}

\usage{RHLightResponseCostC(theta, flux, sdFlux,
    parameterPrior, sdParameterPrior, Rg,
    VPD, Temp, VPD0, fixVPD)}

\description{
	Computing residual sum of squares for predictions vs. data of NEE implemented
	in C. See \code{\link{LightResponseCurveFitter_computeCost}} for a description.
}

\arguments{
  \item{theta}{parameter vector with positions as in argument of
		  \code{\link{LightResponseCurveFitter_getParameterNames}}
}
  \item{flux}{numeric: NEP (-NEE) or GPP time series [umolCO2 / m2 / s],
		  should not contain NA
}
  \item{sdFlux}{numeric: standard deviation of Flux [umolCO2 / m2 / s],
		  should not contain NA
}
  \item{parameterPrior}{numeric vector along theta: prior estimate of
		  parameter (range of values)
}
  \item{sdParameterPrior}{standard deviation of parameterPrior
}
  \item{Rg}{ppfd [numeric] -> photosynthetic flux density
		   [umol / m2 / s] or Global Radiation
}
  \item{VPD}{VPD [numeric] -> Vapor Pressure Deficit [hPa]
}
  \item{Temp}{Temp [degC] -> Temperature [degC]
}
  \item{VPD0}{VPD0 [hPa] -> Parameters VPD0 fixed to 10 hPa
}
  \item{fixVPD}{boolean scalar or vector of nrow theta:
		   fixVPD if TRUE the VPD effect is not considered and VPD is not part
		   of the computation
}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





