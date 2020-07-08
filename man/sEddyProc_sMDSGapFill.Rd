\name{sEddyProc_sMDSGapFill}
\alias{sEddyProc_sMDSGapFill}
\title{sEddyProc sMDSGapFill}
\description{MDS gap filling algorithm adapted after the PV-Wave code and paper by Markus Reichstein.}
\usage{sEddyProc_sMDSGapFill(Var = Var.s, QFVar = if (!missing(QFVar.s)) QFVar.s else "none", 
    QFValue = if (!missing(QFValue.n)) QFValue.n else NA_real_, 
    V1 = if (!missing(V1.s)) V1.s else "Rg", 
    T1 = if (!missing(T1.n)) T1.n else 50, 
    V2 = if (!missing(V2.s)) V2.s else "VPD", 
    T2 = if (!missing(T2.n)) T2.n else 5, 
    V3 = if (!missing(V3.s)) V3.s else "Tair", 
    T3 = if (!missing(T3.n)) T3.n else 2.5, 
    FillAll = if (!missing(FillAll.b)) FillAll.b else TRUE, 
    isVerbose = if (!missing(Verbose.b)) Verbose.b else TRUE, 
    suffix = if (!missing(Suffix.s)) Suffix.s else "", 
    minNWarnRunLength = if (Var == "NEE") 4 * 
        .self$sINFO$DTS/24 else NA_integer_, 
    Var.s, QFVar.s, QFValue.n, V1.s, T1.n, 
    V2.s, T2.n, V3.s, T3.n, FillAll.b, Verbose.b, 
    Suffix.s)}
\arguments{
  \item{Var}{Variable to be filled}
  \item{QFVar}{
}
  \item{QFValue}{
}
  \item{V1}{Condition variable 1
(default: Global radiation 'Rg' in  W m-2)}
  \item{T1}{Tolerance interval 1
(default: 50 W m-2)}
  \item{V2}{Condition variable 2
(default: Vapour pressure deficit 'VPD' in hPa)}
  \item{T2}{Tolerance interval 2
(default: 5 hPa)}
  \item{V3}{Condition variable 3
(default: Air temperature 'Tair' in degC)}
  \item{T3}{Tolerance interval 3
(default: 2.5 degC)}
  \item{FillAll}{Fill
all values to estimate uncertainties}
  \item{isVerbose}{Print
status information to screen}
  \item{suffix}{String
suffix needed for different processing setups on the same dataset
(for explanations see below)}
  \item{minNWarnRunLength}{scalar integer:
warn if number of subsequent
numerically equal values exceeds this number.
Set to Inf or NA for no warnings.
defaults for "NEE" to records across 4 hours and no warning for others.}
  \item{Var.s}{deprecated}
  \item{QFVar.s}{deprecated}
  \item{QFValue.n}{deprecated}
  \item{V1.s}{deprecated}
  \item{T1.n}{deprecated}
  \item{V2.s}{deprecated}
  \item{T2.n}{deprecated}
  \item{V3.s}{deprecated}
  \item{T3.n}{deprecated}
  \item{FillAll.b}{deprecated}
  \item{Verbose.b}{deprecated}
  \item{Suffix.s}{deprecated}
}
\details{Initialize temporal data frame sTEMP for newly generated gap filled data and
qualifiers, see \code{\link{sEddyProc_sFillInit}} for explanations on suffixes.

Runs of numerically equal numbers hint to problems of the data and cause
unreasonable estimates of uncertainty. This routine warns the user.

MDS gap filling algorithm calls the subroutines Look Up Table
\code{\link{sEddyProc_sFillLUT}}
and Mean Diurnal Course \code{\link{sEddyProc_sFillMDC}} with different
window sizes as described in the reference.

To run dataset only with MDC algorithm \code{\link{sEddyProc_sFillMDC}},
set condition variable V1 to 'none'.

\describe{\item{Different processing setups on the same dataset}{
Attention: When processing the same site data set with different setups for
the gap filling or flux partitioning
(e.g. due to different ustar filters),
a string suffix is needed! This suffix is added to the result column names
to distinguish the results of the different setups.
}}}
\value{Gap filling results in sTEMP data frame (with renamed columns).}
\references{Reichstein, M. et al. (2005) On the separation of net ecosystem exchange
into assimilation and ecosystem respiration: review and improved algorithm.
Global Change Biology, 11, 1424-1439.}
\author{AMM, TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}





