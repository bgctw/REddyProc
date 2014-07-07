\name{controlUstarEst}
\alias{controlUstarEst}
\title{controlUstarEst}
\description{Default list of parameters for determining UStar of a single binned series	}
\usage{controlUstarEst(ustPlateauFwd = 10, ustPlateauBack = 6, plateauCrit = 0.95, 
    corrCheck = 0.5)}
\arguments{
  \item{ustPlateauFwd}{number of subsequent thresholds to compare to in fwd mode}
  \item{ustPlateauBack}{number of subsequent thresholds to compare to in back mode}
  \item{plateauCrit}{significant differences between a u* value and the mean of a "plateau"}
  \item{corrCheck}{threshold value for correlation between Tair and u* data}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{estUstarThresholdSingleFw2Binned}}, \code{\link{controlUstarSubsetting}} }
\examples{
controlUstarEst()
}
