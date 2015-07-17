\name{sEstUstarThresholdDistribution}
\alias{sEstUstarThresholdDistribution}
\title{sEstUstarThresholdDistribution}
\description{Estimating the distribution of u* threshold by bootstrapping over data}
\usage{sEstUstarThresholdDistribution(ctrlUstarEst.l = controlUstarEst(), 
    ctrlUstarSub.l = controlUstarSubsetting(),
 ..., seasonFactor.v = createSeasonFactorMonth(sDATA$sDateTime), 
    seasonFactorsYear = getYearOfSeason(seasonFactor.v, ds$sDateTime), 
    nSample = 100L, probs = c(0.05, 0.5, 0.95))}\arguments{
  \item{ctrlUstarEst.l}{control parameters for estimating uStar on a single binned series, see \code{\link{controlUstarEst}}}
  \item{ctrlUstarSub.l}{control parameters for subsetting time series (number of temperature and Ustar classes \ldots), see \code{\link{controlUstarSubsetting}}}
  \item{\dots}{further arguments to \code{\link{sEstUstarThresholdYears}}}
  \item{seasonFactor.v}{factor of seasons to split (data is resampled only within the seasons)}
  \item{seasonFactorsYear}{named integer vector: for each seasonFactor level, get the year that this season belongs to}
  \item{nSample}{the number of repetitions in the bootstrap}
  \item{probs}{the quantiles of the bootstrap sample to return. Default is the 5\%, median and 95\% of the bootstrap}
}
\details{The choice of the criterion for sufficiently turbulent conditions (u* > choosen threshold)
introduces large uncertainties in calculations based on gap-filled Eddy data.
Hence, it is good practice to compare derived quantities based on gap-filled data using a range of u* threshold estimates.

\describe{\item{Quality Assurance}{
If more than \code{ctrlUstarEst.l$minValidBootProp} (default 40\%) did not report a treshold, 
no quantiles (i.e. NA) are reported.
}}		}
\value{A data.frame with columns aggregationMode, year, and UStar estimate based on the unresampled data. 
The other columns correpond to the quantiles of Ustar estimate
for given probabilities (argument \code{probs} based on the distribution of estimates using resampled the data.}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{sEstUstarThreshold}}, \code{\link{sMDSGapFillAfterUStarDistr}}}
\examples{
if( FALSE ){	# takes long, so do not execute on each install or check
	# load the data and generate DateTime column
	Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
	EddyData.F <- ds <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
	EddyDataWithPosix.F <- ds <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
	EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
	(res <- EddyProc.C$sEstUstarThresholdDistribution(nSample=10))	# for real applications use larger sample size
	getAnnualSeasonUStarMappingFromDistributionResult(res)
}
}
