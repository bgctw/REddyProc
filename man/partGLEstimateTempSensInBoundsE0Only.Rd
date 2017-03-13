\name{partGLEstimateTempSensInBoundsE0Only}
\alias{partGLEstimateTempSensInBoundsE0Only}
\title{partGLEstimateTempSensInBoundsE0Only}
\description{Estimate temperature sensitivity E_0 and R_ref of ecosystem respiration, and apply bounds or previous estimate}
\usage{partGLEstimateTempSensInBoundsE0Only(REco, temperatureKelvin, 
    prevE0 = NA)}
\arguments{
  \item{REco}{numeric vector: night time NEE, i.e. ecosytem respiration}
  \item{temperatureKelvin}{numeric vector: temperature in Kelvin of same length as REco}
  \item{prevE0}{numeric scalar: the previous guess of Temperature Sensitivity}
}
\details{Basal respiration is reported for temperature of 15 degree Celsius. However during the fit
a reference temperature of the median of the dataset is used. This is done to avoid
strong correlations between estimated parameters E0 and R_ref, that occure if reference temperature 
is outside the center of the data.

If E_0 is out of bounds [50,400] then report E0 as NA }
\value{list with entries
\item{E0}{numeric scalar of estimated temperature sensitivty E0 bounded to [50,400]}
\item{sdE0}{numeric scalar of standard deviation of E0}
\item{TRefFit}{numeric scalar reference temperature used in the E0 fit}
\item{RRefFit}{numeric scalar respiration at TRefFit
}}

\author{MM, TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{partGLFitLRCWindows}}}

