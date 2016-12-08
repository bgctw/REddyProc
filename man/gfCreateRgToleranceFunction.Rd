\name{gfCreateRgToleranceFunction}
\alias{gfCreateRgToleranceFunction}
\title{gfCreateRgToleranceFunction}
\description{create a tolerance function for that adjusts tolerances for Rg column}
\usage{gfCreateRgToleranceFunction(tolerance, iRgColumns)}
\arguments{
  \item{tolerance}{the vector of tolerances}
  \item{iRgColumns}{the position of radiation in target}
}
\details{The order of entries in \code{tolerance}, and \code{iRgColumns} must match the order to covariates 
supplied to \code{\link{gfGapFillLookupTable}} 

Reduce tolerance of radiation column to [20, min(Rg_tolerance, Rg_target)] depending on the target radiation}
\value{tolerance given at creation of the function with entry for radiation columns}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





