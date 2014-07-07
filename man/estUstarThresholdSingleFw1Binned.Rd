\name{estUstarThresholdSingleFw1Binned}
\alias{estUstarThresholdSingleFw1Binned}
\title{estUstarThresholdSingleFw1Binned}
\description{estimate the Ustar threshold for single subset, using FW1 algorithm, relying on binned NEE and Ustar}
\usage{estUstarThresholdSingleFw1Binned(Ust_bins.f, ctrlUstarEst.l = controlUstarEst())}
\arguments{
  \item{Ust_bins.f}{data.frame with columns NEE_avg and Ust_avg, of NEE by Ustar bins by \code{\link{binUstar}}}
  \item{ctrlUstarEst.l}{parameter list, see \code{\link{controlUstarEst}} for defaults and description}
}


\references{inspired by Papale 2006}
\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





