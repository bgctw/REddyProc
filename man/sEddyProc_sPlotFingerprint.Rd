\name{sEddyProc_sPlotFingerprint}
\alias{sEddyProc_sPlotFingerprint}
\title{sEddyProc$sPlotFingerprint - Image with fingerprints of each year}
\description{Generates image in specified format \code{Format.s} (e.g. 'pdf' or 'png') 
with fingerprint, see also \code{\link{sEddyProc_sPlotFingerprintY}}.}
\usage{sEddyProc_sPlotFingerprint(Var.s, QFVar.s = "none", 
    QFValue.n = NA_real_, Format.s = "pdf", 
    Dir.s = "plots", ...)}
\arguments{
  \item{Var.s}{Variable to plot}
  \item{QFVar.s}{Quality flag of variable to be filled}
  \item{QFValue.n}{Value of quality flag for data to plot}
  \item{Format.s}{Graphics file format (e.g. 'pdf', 'png')}
  \item{Dir.s}{Directory for plotting}
  \item{\dots}{further arguments to \code{\link{sEddyProc_sPlotFingerprintY}}}
}



\author{KS, AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





