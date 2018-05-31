\name{sEddyProc_sMDSGapFillAfterUStarDistr}
\alias{sEddyProc_sMDSGapFillAfterUStarDistr}
\title{sEddyProc sMDSGapFillAfterUStarDistr}
\description{GapFilling for several filters of estimated friction velocity Ustar thresholds.}
\usage{sEddyProc_sMDSGapFillAfterUStarDistr(..., 
    uStarTh, uStarSuffixes = colnames(uStarTh)[-1])}
\arguments{
  \item{\dots}{other arguments to
\code{\link{sEddyProc_sMDSGapFillAfterUstar}} and \code{\link{sEddyProc_sMDSGapFill}}
such as \code{fluxVar}}
  \item{uStarTh}{data.frame with first column, season names,
and remaining columns different estimates of uStar Threshold.
If the data.frame has only one row, then each uStar threshold estimate
is applied to the entire dataset.
Entries in first column must match levels in argument \code{seasonFactor}}
  \item{uStarSuffixes}{String vector
to distinguish result columns for different ustar values.
Its length must correspond to column numbers in \code{UstarThres.m.n}.}
}
\details{This method is superseedec by
\code{\link{sEddyProc_sMDSGapFillUStarScens}} and only there
for backward portability.}


\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





