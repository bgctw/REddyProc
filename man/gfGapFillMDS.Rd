\name{gfGapFillMDS}
\alias{gfGapFillMDS}
\title{gfGapFillMDS}
\description{Successive Gap-Filling with increasing window sizes and omitting covariates based on Rg, VPD, Tair, and diurnal cycle}
\usage{gfGapFillMDS(ds, fillVarName = "NEE", covarVarNames = c("Rg", 
    "VPD", "Tair"), tolerance = c(Rg = 50, VPD = 5, 
    Tair = 2.5), RgVarNames = c("Rg"), nRecInDay = 48L, 
    isFillAll = FALSE, isVerbose = TRUE, qfVarName = "none", 
    qfValue = NA_real_, maxGapSize = nRecInDay * 60L, 
    ...)}
\arguments{
  \item{ds}{data.frame with variables to fill and covariates}
  \item{fillVarName}{scalar string: name of variable to fill}
  \item{covarVarNames}{string vector of column names that hold covariates to determine similar conditions.
<< the first covariate is most important because its used alone, if no similar conditions were found based on all covariates}
  \item{tolerance}{numeric vector of tolerances with named components for each entry in meteoVarNames}
  \item{RgVarNames}{string vector: column names for which radiation relative tolerance bounds is to be applied (see \code{\link{gfCreateRgToleranceFunction}})}
  \item{nRecInDay}{integer scalar: number of records within one day, default corresponds to half-hourly records}
  \item{isFillAll}{set to TRUE to compute statistics also for good quality records}
  \item{isVerbose}{set to FALSE to suppress progress messages}
  \item{qfVarName}{scalar string: name of variable withe integer quality flag}
  \item{qfValue}{integer scalar: Value of quality flag for _good_ (original) data, other data is set to missing}
  \item{maxGapSize}{integer scalar: maximum number of records in a gap, that is still be filled, longer gaps are not filled}
  \item{\dots}{further arguments to \code{\link{gfGapFillLookupTable}}, such as \code{minNSimilar}}
}

\value{data.frame of the same length as ds with variables <fillVarname>_f, 

a data.frame with columns of sta
\item{fmean}{mean of similar values}
\item{fnum}{number of similar values}
\item{fsd}{standard deviation of similar values}
\item{fmeth}{method of determining similar values; 1 = at least 3 covariates, 2 = at least one covariate, 3 = mean diurnal course}
\item{fqc}{quality flag;  0 = original, 1 = most reliable, 2 = medium, 3 = least reliable}
\item{filled}{if orignial value is finite, original value, else fmean}}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





