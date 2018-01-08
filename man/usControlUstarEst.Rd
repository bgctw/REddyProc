\name{usControlUstarEst}
\alias{usControlUstarEst}
\title{usControlUstarEst}
\description{Default list of parameters for determining UStar of a single binned series}
\usage{usControlUstarEst(ustPlateauFwd = 10, ustPlateauBack = 6, 
    plateauCrit = 0.95, corrCheck = 0.5, 
    firstUStarMeanCheck = 0.2, isOmitNoThresholdBins = TRUE, 
    isUsingCPTSeveralT = FALSE, isUsingCPT = FALSE, 
    minValidUStarTempClassesProp = 0.2, minValidBootProp = 0.4, 
    minNuStarPlateau = 3L)}
\arguments{
  \item{ustPlateauFwd}{number of subsequent uStar bin values to compare
to in fwd mode}
  \item{ustPlateauBack}{number of subsequent uStar bin values to compare
to in back mode}
  \item{plateauCrit}{significant differences between a u * value and
the mean of a "plateau"}
  \item{corrCheck}{threshold value for correlation between Tair
and u * data}
  \item{firstUStarMeanCheck}{if first uStar bin average of a class is already
larger than this value, the temperature class is skipped.}
  \item{isOmitNoThresholdBins}{if TRUE, bins where no threshold was found
are ignored. Set to FALSE to report highest uStar bin for these cases}
  \item{isUsingCPTSeveralT}{set to TRUE to use changePointDetection
without binning uStar but with additionally changed aggregation scheme for
several temperature classifications}
  \item{isUsingCPT}{set to TRUE to use changePointDetection without
binning uStar before in usual aggregation method (good for comparing methods,
but not recommended, overruled by isUsingCPTSeveralT = TRUE)}
  \item{minValidUStarTempClassesProp}{seasons, in which only less than this
proportion of temperature classes a threshold was detected,
are excluded from aggregation}
  \item{minValidBootProp}{minimum proportion of bootstrap samples
for which a threshold was detected. Below this proportion
NA quantiles are reported.}
  \item{minNuStarPlateau}{minimum number of records in plateau, threshold
must be larger than mean of this many bins}
}



\author{TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{usEstUstarThresholdSingleFw2Binned}},
\code{\link{usControlUstarSubsetting}}}
\examples{
usControlUstarEst()
}
