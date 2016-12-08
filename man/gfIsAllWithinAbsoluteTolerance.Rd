\name{gfIsAllWithinAbsoluteTolerance}
\alias{gfIsAllWithinAbsoluteTolerance}
\title{gfIsAllWithinAbsoluteTolerance}
\description{test for each column that absolute differences are smaller than tolerance}
\usage{gfIsAllWithinAbsoluteTolerance(covWin, target, tol, 
    isCovNAInTolerance = FALSE, nCov = nrow(covWin))}
\arguments{
  \item{covWin}{numeric matrix with covariates in rows and records in columns}
  \item{target}{numeric vector of covariates at target record}
  \item{tol}{numeric vector of tolerances for each covariate}
  \item{isCovNAInTolerance}{logical scalar: set to TRUE to assume that covariates with NA are within tolerance (default to assume its not)}
  \item{nCov}{may provide for slight speedup}
}

\value{logical vector of length ncol(covWin) with TRUE if column is within tolerance}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





