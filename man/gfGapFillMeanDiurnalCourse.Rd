\name{gfGapFillMeanDiurnalCourse}
\alias{gfGapFillMeanDiurnalCourse}
\title{gfGapFillMeanDiurnalCourse}
\description{Gap filling with Mean Diurnal Course (MDC)
Mean Diurnal Course (MDC) algorithm based on average values within +/- one hour of adjacent days.}
\usage{gfGapFillMeanDiurnalCourse(toFill, winExtInDays, ..., 
    nRecInDay = 48L, toleranceInHours = 1)}
\arguments{
  \item{toFill}{numeric vector to be filled}
  \item{winExtInDays}{half-window width in days, full window will be}
  \item{\dots}{other arguments to \code{\link{gfGapFillLookupTable}}, such as \code{minNSimilar}, or\code{iRecsGap}}
  \item{nRecInDay}{integer scalar: number of records within one day, default corresponds to half-hourly records}
  \item{toleranceInHours}{numeric scalar: maximum difference in "hour since daystart" to select similar values}
}

\value{result of \code{\link{gfGapFillLookupTable}}}

\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





