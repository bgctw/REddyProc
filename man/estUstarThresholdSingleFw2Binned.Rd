\name{estUstarThresholdSingleFw2Binned}
\alias{estUstarThresholdSingleFw2Binned}
\title{estUstarThresholdSingleFw2Binned}
\description{estimate the Ustar threshold for single subset, using FW2 algorithm  	}
\usage{estUstarThresholdSingleFw2Binned(Ust_bins.f, ctrlUstarEst.l = controlUstarEst())}
\arguments{
  \item{Ust_bins.f}{data.frame with column s NEE_avg and Ust_avg, of NEE by Ustar bins by \code{\link{binUstar}}}
  \item{ctrlUstarEst.l}{parameter list, see \code{\link{controlUstarEst}} for defaults and description}
}
\details{Demand that threshold is higher than \code{ctrlUstarEst.l$minNuStarPlateau} records.
If fewer records  }


\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





