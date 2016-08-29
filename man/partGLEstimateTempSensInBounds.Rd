\name{partGLEstimateTempSensInBounds}
\alias{partGLEstimateTempSensInBounds}
\title{partGLEstimateTempSensInBounds}
\description{Estimate temperature sensitivity E_0 and R_ref of ecosystem respiration, and apply bounds or previous estimate}
\usage{partGLEstimateTempSensInBounds(REco.V.n, temperatureKelvin.V.n, 
    prevE0 = NA, prevR_ref = NA)}
\arguments{
  \item{REco.V.n}{numeric vector: night time NEE, i.e. ecosytem respiration}
  \item{temperatureKelvin.V.n}{temperature in K}
  \item{prevE0}{numeric scalar: the previous guess of Temperature Sensitivity}
  \item{prevR_ref}{nuermic scalar: previous guess of}
}
\details{If E_0 is out of bounds [50,400] then report E_0 of estimate from previous window and R_ref as mean of the respiration.
If no previous estimate is available, report lower bound of 50 or upper bound of 400 respectively.  }
\value{list with entries
\item{E_0}{numeric scalar of estimated temperature sensitivty E0 bounded to [50,400]}
\item{E_0_SD}{numeric scalar of standard deviation of E0}
\item{R_ref}{numeric scalar of estimated respiration at reference temperature}
\item{resFit}{the fit-object
}}

\author{MM, TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partGLFitLRCWindows}}}

