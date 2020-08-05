\name{sEddyProc_sMDSGapFillUStarScens}
\alias{sEddyProc_sMDSGapFillUStarScens}
\title{sEddyProc sMDSGapFillUStarScens}
\description{gapfilling for several filters of estimated friction velocity Ustar thresholds.}
\usage{sEddyProc_sMDSGapFillUStarScens(...)}
\arguments{
  \item{\dots}{other arguments to
\code{\link{sEddyProc_sMDSGapFillAfterUstar}} and
\code{\link{sEddyProc_sMDSGapFill}}
such as \code{fluxVar}}
}
\details{sEddyProc$sMDSGapFillUStarDistr: calling
\code{\link{sEddyProc_sMDSGapFillAfterUstar}} for several filters of
friction velocity Ustar.

The scenarios need to be set before by
\code{\link{sEddyProc_sSetUstarScenarios}} or accepting the defaults
annual estimates of \code{link{sEddyProc_sEstimateUstarScenarios}}.

Then the difference between output columns NEE_U05_f and NEE_U95_f
corresponds to the uncertainty
introduced by the uncertain estimate of the u* threshold.}
\value{Matrix (columns correspond to u* Scenarios) with quality flag from
filtering ustar (0 - good data, 1 - filtered data)

Gap filling results in sTEMP data frame (with renamed columns), that
can be retrieved by \code{\link{sEddyProc_sExportResults}}.
Each of the outputs is calculated for several u* r-estimates and
distinguished by a suffix after the variable.
E.g. with an an entry "U05" in \code{uStarSuffixes} in
\code{\link{sEddyProc_sSetUstarScenarios}}
the corresponding filled NEE can be found in output column "NEE_U05_f".}

\author{TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\href{../doc/useCase.html}{useCase vignette}}

