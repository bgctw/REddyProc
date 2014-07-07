\name{sMDSGapFillUStar}
\alias{sMDSGapFillUStar}
\title{GapFilling for several filters of friction velocity Ustar}
\description{MDS gap filling algorithm adapted after the PV-Wave code and paper by Markus Reichstein.

sEddyProc$sMDSGapFillUStar - calling sMDSGapFill for several filters of friction velocity Ustar}
\usage{sMDSGapFillUStar(Var.s, ustar.m = .self$sEstUstarThresholdDistribution(), 
    suffix.v = c("Ustar", "U05", "U50", "U95"), ..., UstarVar.s = "Ustar", 
    isFlagEntryAfterLowTurbulence = TRUE)}
\arguments{
  \item{Var.s}{Variable to be filled}
  \item{ustar.m}{Numeric matrix( nYear x nUStar): output of \code{\link{sEstUstarThresholdDistribution}}: each row is a vector of ustar thresholds to apply before gap filling for one years.
Make sure that one row is given for each year in the dataset to gap-Fill.
If only one row, i.e. a vector is given, then it is used for each year.
Defaults to single value at the 90\% quantile of UStar column.}
  \item{suffix.v}{String vector of length of ustar.v of column suffixes.
to distinguish results for different ustar.v.
Length must correspond to column numbers in ustar.m
Defaults correspond to return default return value function \code{\link{sEstUstarThresholdDistribution}} 
(estimate on original series, 5\% of bootstrap, median of bootstrap, 95\% of bootstrap) }
  \item{\dots}{other arguments to \code{\link{sMDSGapFill}}}
  \item{UstarVar.s}{Friction velocity ustar (ms-1)}
  \item{isFlagEntryAfterLowTurbulence}{set to FALSE to avoid flagging the first entry after low turbulance as bad condition}
}
\details{With \code{isFlagEntryAfterLowTurbulence set to TRUE}, to be more conservative, in addition
to the data acquired when u* is below the threshold,
the first half hour measured with good turbulence conditions
after a period with low turbulence is also removed (Papaple et al. 2006).}
\value{Gap filling results in sTEMP data frame (with renamed columns).}

\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
  if( FALSE ){  # takes long, do not execute on every install
		# load the data from text file
	  	Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
	  	EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
	  	# create TimeStamp column
	  	EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
	  
		#------------- default use case
		EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))   
	    EddyProc.C$sMDSGapFillUStar('NEE' )     # calls sEstUstarThresholdDistribution 
	    dsf <- EddyProc.C$sExportResults()
	    colnames(dsf)		# note the different output columns corresponding to different Ustar estimates, best estimate with suffix "Ustar"
	    #plot( NEE_U05_f ~ NEE_U95_f, dsf)	# differences between gapFilling using differing Ustar thresholds

		#------------- only using one Ustar estimate (omitting output columns for range of Ustar estimates)
		EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
		Ustar <- EddyProc.C$sEstUstarThreshold()$UstarAggr
		EddyProc.C$sMDSGapFillUStar('NEE', ustar.m=Ustar, suffix.v="Ustar")      
		colnames(EddyProc.C$sExportResults())
		
		#--------------- the advanced user can specify his own estimates of Ustar 
		# modifying arguements to sEstUstarThresholdDistribution, e.g quantiles to inspect
		EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))   
		ustar.m <- EddyProc.C$sEstUstarThresholdDistribution(EddyProc.C$sExportData(), nSample=10, probs=c(0.25,0.5,0.75))
		ustar.m		# note that the first entry corresponds to the non-bootstrapped Ustar estimate 
		EddyProc.C$sMDSGapFillUStar('NEE', ustar.m=ustar.m, suffix.v=c("Ustar","U25","U50","U75") )
		colnames(EddyProc.C$sExportResults())
		#
		# specify estimates directly
		EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg','Tair','VPD','Ustar'))
		ustar.m <- c(0.38, 0.44)
		ustar.m = matrix(c(0.38,0.42), byrow=TRUE, ncol=2, nrow=2, dimnames=list(years=c(1998,1999),probs=c("05","95") )) # possible different thresholds for different years  
		EddyProc.C$sMDSGapFillUStar('NEE', ustar.m=ustar.m, suffix.v=c("U38","U44") )
		colnames(EddyProc.C$sExportResults())
}
}
