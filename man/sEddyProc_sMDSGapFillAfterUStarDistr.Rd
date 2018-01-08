\name{sEddyProc_sMDSGapFillAfterUStarDistr}
\alias{sEddyProc_sMDSGapFillAfterUStarDistr}
\title{GapFilling for several filters of estimated friction velocity Ustar thresholds.}
\description{sEddyProc$sMDSGapFillUStarDistr - calling \code{\link{sEddyProc_sMDSGapFillAfterUstar}} for several filters of friction velocity Ustar}
\usage{sEddyProc_sMDSGapFillAfterUStarDistr(..., 
    UstarThres.df, UstarSuffix.V.s = colnames(UstarThres.df)[-1])}
\arguments{
  \item{\dots}{other arguments to \code{\link{sEddyProc_sMDSGapFillAfterUstar}} and \code{\link{sEddyProc_sMDSGapFill}}}
  \item{UstarThres.df}{data.frame with first column, season names, and remaining columns different estimates of uStar Threshold.
If the data.frame has only one row, then each uStar threshold estimate is applied to the entire dataset.
Entries in first column must match levels in argument \code{seasonFactor.v}}
  \item{UstarSuffix.V.s}{String vector
to distinguish result columns for different ustar values.
Its length must correspond to column numbers in \code{UstarThres.m.n}.}
}
\details{The eddy covariance method does not work with low turbulence conditions. Hence the periods with low turbulence
indicated by a low friction velocity u * needs to be filtered out and gapfilled (see \code{\link{sEddyProc_sMDSGapFill}}).
The threshold value of a sufficient u * causes one of the largest uncertainty components within the gap-filled data.
Hence, it is good practice to compare derived quantities based on gap-filled data using different u * threshold values.

For example a user could provide the the following collumns in argument \code{UstarThres.df}
and corresponding suffixes in argument \code{UstarSuffix.V.s}
\itemize{
\item season: identifier for which season this row is used.
\item Ustar: estimate on original series
\item U05: 5\% of bootstrap
\item U50: median of bootstrap
\item U95: 95\% of bootstrap)
}
Then the difference between output columns NEE_U05_f and NEE_U95_f corresponds to the uncertainty
introduced by the uncertain estimate of the u * threshold.}
\value{Matrix (columns correspond to u * Scenarios) with quality flag from filtering ustar (0 - good data, 1 - filtered data)

Gap filling results in sTEMP data frame (with renamed columns), that can be retrieved by \code{\link{sEddyProc_sExportResults}}.
Each of the columns is calculated for several u * r-estimates and distinguished by a suffix after the variable.
E.g. with an first entry "U05" in \code{UstarSuffix.V.s} corresponding to the first column in  \code{UstarThres.m.n},
the corresponding filled NEE can be found in output collumn "NEE_U05_f".}

\author{TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\href{useCase vignette}{../doc/useCase.html}}

