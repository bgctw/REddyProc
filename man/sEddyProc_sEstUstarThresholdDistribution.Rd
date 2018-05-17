\name{sEddyProc_sEstUstarThresholdDistribution}
\alias{sEddyProc_sEstUstarThresholdDistribution}
\title{sEddyProc sEstUstarThresholdDistribution}
\description{Estimating the distribution of u * threshold by bootstrapping over data}
\usage{sEddyProc_sEstUstarThresholdDistribution(ctrlUstarEst.l = usControlUstarEst(), 
    ctrlUstarSub.l = usControlUstarSubsetting(), 
    UstarColName = "Ustar", NEEColName = "NEE", 
    TempColName = "Tair", RgColName = "Rg", 
    ..., seasonFactor.v = usCreateSeasonFactorMonth(sDATA$sDateTime), 
    seasonFactorsYear = usGetYearOfSeason(seasonFactor.v, 
        ds$sDateTime), nSample = 100L, probs = c(0.05, 
        0.5, 0.95), verbose.b = TRUE)}
\arguments{
  \item{ctrlUstarEst.l}{control parameters
for estimating uStar on a single binned series,
see \code{\link{usControlUstarEst}}}
  \item{ctrlUstarSub.l}{control parameters
for subsetting time series (number of temperature and Ustar classes
\ldots), see \code{\link{usControlUstarSubsetting}}}
  \item{UstarColName}{column name for UStar}
  \item{NEEColName}{column name for NEE}
  \item{TempColName}{column name for air temperature}
  \item{RgColName}{column name for solar radiation for
omitting night time data}
  \item{\dots}{further arguments to \code{\link{sEddyProc_sEstUstarThreshold}}}
  \item{seasonFactor.v}{factor of seasons to split (data is resampled only within the seasons)}
  \item{seasonFactorsYear}{named integer vector: for each seasonFactor level, get the year that
this season belongs to}
  \item{nSample}{the number of repetitions in the bootstrap}
  \item{probs}{the quantiles of the bootstrap sample
to return. Default is the 5\%, median and 95\% of the bootstrap}
  \item{verbose.b}{set to FALSE to omit printing progress}
}
\details{The choice of the criterion for sufficiently turbulent conditions
(u * > chosen threshold)
introduces large uncertainties in calculations based on gap-filled Eddy data.
Hence, it is good practice to compare derived quantities based on
gap-filled data using a range of u * threshold estimates.

This method explores the probability density of the threshold by
repeating its estimation
on a bootstrapped sample.
By default it returns the 90\% confidence interval (argument \code{probs}).
For larger intervals the sample number need to be
increased (argument \code{probs}).

\describe{\item{Quality Assurance}{
If more than \code{ctrlUstarEst.l$minValidBootProp}
(default 40\%) did not report a threshold,
no quantiles (i.e. NA) are reported.
}}}
\value{A data.frame with columns \code{aggregationMode}, \code{year},
and \code{UStar} estimate based on the non-resampled data.
The other columns correspond to the quantiles of Ustar estimate
for given probabilities (argument \code{probs}) based on the distribution
of estimates using resampled the data.}

\author{TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{sEddyProc_sEstUstarThreshold}}
, \code{\link{sEddyProc_sMDSGapFillAfterUStarDistr}}}

