\name{sMDSGapFillAfterUstar}
\alias{sMDSGapFillAfterUstar}
\title{sEddyProc$sMDSAfterFiltering- MDS gap filling algorithm after u* filtering}
\description{Calling \code{\link{sMDSGapFill}} after filtering for (provided) friction velocity u*}
\usage{sMDSGapFillAfterUstar(FluxVar.s = "NEE", UstarVar.s = "Ustar", 
    UstarThres.V.n, UstarSuffix.s = "", FlagEntryAfterLowTurbulence.b = TRUE, 
    addThresholdCols.b = TRUE, ...)}
\arguments{
  \item{FluxVar.s}{Variable, i.e. collumn name,  of net ecosystem fluxes, default 'NEE'}
  \item{UstarVar.s}{Column name of friction velocity u* (ms-1), default 'Ustar'}
  \item{UstarThres.V.n}{u* thresholds as single number or vector with values for each year}
  \item{UstarSuffix.s}{Suffix required if different u* scenarios are filled}
  \item{FlagEntryAfterLowTurbulence.b}{Default TRUE for flagging the first entry after low turbulance as bad condition.}
  \item{addThresholdCols.b}{Default TRUE for adding the threshold and the flag (1 bad data) as output columns}
  \item{\dots}{Other arguments passed to \code{\link{sMDSGapFill}}}
}
\details{The u* threshold(s) are provided for filtering the conditions of low turbulence.
After filtering, the data is gap filled using the MDS algorithm \code{\link{sMDSGapFill}}.

With \code{isFlagEntryAfterLowTurbulence set to TRUE}, to be more conservative, in addition
to the data acquired when u* is below the threshold,
the first half hour measured with good turbulence conditions
after a period with low turbulence is also removed (Papaple et al. 2006).}
\value{Vector with quality flag from filtering (bad data are FALSE)
Gap filling results are in sTEMP data frame (with renamed columns) that can be retrieved by \code{\link{sExportResults}}.}

\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
if( FALSE ){  # takes long, do not execute on every install
	# load the data from text file
	Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
	EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
	# create TimeStamp column
	EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
	
	EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
	Ustar <- 0.428 #EddyProc.C$sEstUstarThreshold()$UstarAggr	# estimate the Ustar
	EddyProc.C$sMDSGapFillAfterUstar('NEE',  UstarThres.V.n = Ustar)     # calls sEstUstarThresholdDistribution 
	dsf <- EddyProc.C$sExportResults()
	colnames(dsf)		
	#plot( NEE_U05_f ~ NEE_U95_f, dsf)	# differences between gapFilling using differing Ustar thresholds
	
	#------------- with suffix
	EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
	EddyProc.C$sMDSGapFillAfterUstar('NEE',  UstarThres.V.n = Ustar, UstarSuffix.s="Ustar")     # calls sEstUstarThresholdDistribution 
	colnames(EddyProc.C$sExportResults())	# note suffix in output Columns
}
}
