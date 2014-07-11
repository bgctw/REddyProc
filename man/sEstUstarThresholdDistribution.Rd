\name{sEstUstarThresholdDistribution}
\alias{sEstUstarThresholdDistribution}
\title{sEstUstarThresholdDistribution}
\description{Estimating the distribution of u* trheshold by bootstrapping over data}
\usage{sEstUstarThresholdDistribution(ctrlUstarEst.l = controlUstarEst(), 
    ctrlUstarSub.l = controlUstarSubsetting(), ..., seasonFactor.v = as.factor(createSeasonFactorMonth(sDATA$sDateTime)), 
    yearFactor.v = as.factor(as.POSIXlt(sDATA$sDateTime)$year + 
        1900), nSample = 30, probs = c(0.05, 0.5, 0.95))}
\arguments{
  \item{ctrlUstarEst.l}{control parameters for estimating uStar on a single binned series, see \code{\link{controlUstarEst}}}
  \item{ctrlUstarSub.l}{control parameters for subsetting time series (number of temperature and Ustar classes \ldots), see \code{\link{controlUstarSubsetting}}}
  \item{\dots}{further arguments to \code{\link{sEstUstarThresholdYears}}}
  \item{seasonFactor.v}{factor of seasons to split}
  \item{yearFactor.v}{factor vector (nrow(dsYear) of seasons so split dsYear}
  \item{nSample}{the number of repetitions in the bootstrap}
  \item{probs}{the quantiles of the bootstrap sample to return. Default is the 5\%, median and 95\% of the bootstrap}
}
\details{The choice of the criterion for sufficiently turbulent conditions (u* > choosen threshold)
introduced large uncertainties in calculations based on gap-filled Eddy data.

This method explores the probability density of the threshold by repeating its estimation
on a bootstrapped sample.
By default it returns the 90\% confidence interval (arguement \code{probs}. 
For larger intervals the sample number need to be increased (arguement \code{probs}. }
\value{a matrix (nYear x (1+nProbs): first two column is the original estimate 
the other columns correponsing to the quantiles of Ustar estimate 
for given probabilities (arguement \code{probs}
Rownames hold the corresonding years.}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{sEstUstarThreshold}}, \code{\link{sEstUstarThresholdYears}}, \code{\link{sMDSGapFillUStar}}}
\examples{
if( FALSE ){	# takes long, so do not execute on each install or check
	# load the data and generate DateTime column
	Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
	EddyData.F <- ds <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
	EddyDataWithPosix.F <- ds <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
	EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
	# initialize parallel setup and do the bootstrap, may omit
	#sfInit(parallel=TRUE,cpus=2)
	#options("boot.parallel" = "snow")
	#getOption("boot.parallel")
	#options("boot.parallel" = NULL)
	(res <- EddyProc.C$sEstUstarThresholdDistribution(nSample=10))
	#(res <- sEstUstarThresholdDistribution(subset(ds, as.POSIXlt(ds$DateTime)$year==98 ), nSample=20))
}
}
