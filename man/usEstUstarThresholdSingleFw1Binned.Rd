\name{usEstUstarThresholdSingleFw1Binned}
\alias{usEstUstarThresholdSingleFw1Binned}
\title{usEstUstarThresholdSingleFw1Binned}
\description{estimate the Ustar threshold for single subset, using FW1 algorithm}
\usage{usEstUstarThresholdSingleFw1Binned(Ust_bins.f, 
    ctrlUstarEst.l = usControlUstarEst())}
\arguments{
  \item{Ust_bins.f}{data.frame with columns NEE_avg and Ust_avg, of Ustar bins}
  \item{ctrlUstarEst.l}{parameter list,
see \code{\link{usControlUstarEst}} for defaults and description}
}
\details{Relying on binned NEE and Ustar}

\references{inspired by Papale 2006}
\author{TW, OM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





