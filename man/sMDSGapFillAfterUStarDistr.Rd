\name{sMDSGapFillAfterUStarDistr}
\alias{sMDSGapFillAfterUStarDistr}
\title{GapFilling for several filters of friction velocity Ustar}
\description{sEddyProc$sMDSGapFillUStar - calling \code{\link{sMDSGapFill}} for several filters of friction velocity Ustar}
\usage{sMDSGapFillAfterUStarDistr(FluxVar.s = "NEE", UstarVar.s = "Ustar", 
    UstarThres.m.n = .self$sEstUstarThresholdDistribution(), 
    UstarSuffix.V.s = c("Ustar", "U05", "U50", "U95"), ...)}
\arguments{
  \item{FluxVar.s}{Variable, i.e. collumn name,  of net ecosystem fluxes, default 'NEE'}
  \item{UstarVar.s}{Column name of friction velocity u* (ms-1), default 'Ustar'}
  \item{UstarThres.m.n}{Numeric matrix( nYear x nUStar): output of \code{\link{sEstUstarThresholdDistribution}}: each row is a vector of ustar thresholds to apply before gap filling for one years.
Make sure that one row is given for each year in the dataset to gap-Fill.
If only one row, i.e. a vector is given, then it is used for each year.}
  \item{UstarSuffix.V.s}{String vector of column suffixes of length of collumns of UstarThres.V.n.
to distinguish results for different ustar values.
Length must correspond to column numbers in UstarThres.m.n
Defaults correspond to return default return value function \code{\link{sEstUstarThresholdDistribution}} 
(estimate on original series, 5\% of bootstrap, median of bootstrap, 95\% of bootstrap) }
  \item{\dots}{other arguments to \code{\link{sMDSGapFillAfterUStar}} and \code{\link{sMDSGapFill}}}
}
\details{The eddy covariance method does not work with low turbulence conditions. Hence the periods with low turbulence
indicated by a low friction velocity u* needs to be filtered out and gapfilled.
The threshold value of a sufficient u* causes one of the largest uncertainty components within the gap-filled data. 
Hence, it is good practice to compare derived quantities based on gap-filled data using different u* threshold values.

The u* threshold(s) are provided for filtering the conditions of low turbulence.
After filtering, the data is gap filled using the MDS algorithm \code{\link{sMDSGapFill}}.}
\value{Matrix (columns correspond to u*Scenarios) with quality flag from filtering ustar (0 - good data, 1 - filtered data)

Gap filling results in sTEMP data frame (with renamed columns), that can be retrieved by \code{\link{sExportResults}}.
Each of the columns is calculated for several u*r-estimates and distinguished by a suffix after the variable. 
By default NEE for best UStar estimate is given in column NEE_UStar_f, 
and NEE based on lower and upper 90\% confidence interval estimates of Ustar threshold 
are returned in columns NEE_U05_f and NEE_U95_f respectively.}

\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}



\seealso{\code{\link{sEstUstarThresholdDistribution}}}
\examples{
if( FALSE ){  # takes long, do not execute on every install
	# load the data from text file
	Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
	EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
	# create TimeStamp column
	EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
	
	#------------- default use case, based on sEstUstarThresholdDistribution()
	EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))   
	EddyProc.C$sMDSGapFillAfterUStarDistr('NEE' )     # calls sEstUstarThresholdDistribution 
	dsf <- EddyProc.C$sExportResults()
	colnames(dsf)		# note the different output columns corresponding to different Ustar estimates, best estimate with suffix "Ustar"
	#plot( NEE_U05_f ~ NEE_U95_f, dsf)	# differences between gapFilling using differing Ustar thresholds
	
	#------------- specifying two uStarThresholds directly
	EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
	ustar.m <- c(0.38, 0.44)	# used for all years
	tmp <- EddyProc.C$sMDSGapFillAfterUStarDistr('NEE', UstarThres.m.n=ustar.m, UstarSuffix.V.s=c("Ustar38","Ustar44") )
	dsf <- EddyProc.C$sExportResults()
	colnames(dsf)		# note the different output columns corresponding to different Ustar estimates, best estimate with suffix "Ustar"
	#plot( NEE_Ustar44_f ~ NEE_Ustar38_f, dsf)	# differences between gapFilling using differing Ustar thresholds
}
}
