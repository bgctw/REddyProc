\name{sEddyProc_sMDSGapFillAfterUstar}
\alias{sEddyProc_sMDSGapFillAfterUstar}
\title{sEddyProc sMDSGapFillAfterUstar}
\description{sEddyProc$sMDSGapFillAfterUstar - MDS gap filling algorithm after u* filtering}
\usage{sEddyProc_sMDSGapFillAfterUstar(fluxVar, 
    uStarVar = "Ustar", uStarTh = .self$sGetUstarScenarios()[, 
        c("season", uStarSuffix), drop = FALSE], 
    uStarSuffix = "uStar", isFlagEntryAfterLowTurbulence = FALSE, 
    isFilterDayTime = FALSE, swThr = 10, 
    RgColName = "Rg", ...)}
\arguments{
  \item{fluxVar}{Flux variable to gap fill after ustar filtering}
  \item{uStarVar}{Column name of friction velocity u * (ms-1),
default 'Ustar'}
  \item{uStarTh}{data.frame with
first column, season names, and second column estimates of uStar Threshold.
Alternatively, a single value to be used as threshold for all records
If only one value is given, it is used for all records.}
  \item{uStarSuffix}{Different suffixes required are for
different u * scenarios}
  \item{isFlagEntryAfterLowTurbulence}{Set to TRUE for flagging the
first entry after low turbulence as bad condition (by value of 2).}
  \item{isFilterDayTime}{Set to TRUE to also filter day-time values,
default only filters night-time data}
  \item{swThr}{threshold of solar radiation below which data is
marked as night time respiration.}
  \item{RgColName}{Column name of incoming short wave radiation}
  \item{\dots}{Other arguments passed to \code{\link{sEddyProc_sMDSGapFill}}}
}
\details{Calling \code{\link{sEddyProc_sMDSGapFill}} after filtering for
(provided) friction velocity u*

The u* threshold(s) are provided with argument \code{uStarTh} for
filtering the conditions of low turbulence.
After filtering, the data is gap filled using the MDS algorithm
\code{\link{sEddyProc_sMDSGapFill}}.

With \code{isFlagEntryAfterLowTurbulence set to TRUE}, to be more
conservative, in addition
to the data acquired when uStar is below the threshold,
the first half hour measured with good turbulence conditions
after a period with low turbulence is also removed (Papale et al. 2006).}
\value{Vector with quality flag from filtering (here 0: good data
, 1: low turbulence, 2: first half hour after low turbulence
, 3: no threshold available, 4: missing uStar value)
Gap filling results are in sTEMP data frame (with renamed columns)
that can be retrieved by \code{\link{sEddyProc_sExportResults}}.}

\author{AMM, TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\itemize{
\item \code{\link{sEddyProc_sEstimateUstarScenarios}} and
\code{link{sEddyProc_sEstUstarThold}} for estimating the
u* threshold from the data.
\item \code{\link{sEddyProc_sMDSGapFillUStarScens}} for
automated gapfilling for several scenarios of u* threshold estimates.
}}

