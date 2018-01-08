\name{sEddyProc_sMDSGapFill}
\alias{sEddyProc_sMDSGapFill}
\title{sEddyProc$sMDSGapFill - MDS gap filling algorithm}
\description{MDS gap filling algorithm adapted after the PV-Wave code and paper by Markus Reichstein.}
\usage{sEddyProc_sMDSGapFill(Var.s, QFVar.s = "none", 
    QFValue.n = NA_real_, V1.s = "Rg", T1.n = 50, 
    V2.s = "VPD", T2.n = 5, V3.s = "Tair", 
    T3.n = 2.5, FillAll.b = TRUE, Verbose.b = TRUE, 
    Suffix.s = "")}
\arguments{
  \item{Var.s}{Variable to be filled}
  \item{QFVar.s}{Quality flag of variable to be filled}
  \item{QFValue.n}{Value of quality flag for _good_ (original) data, other data is set to missing}
  \item{V1.s}{Condition variable 1 (default: Global radiation 'Rg' in  W m-2)}
  \item{T1.n}{Tolerance interval 1 (default: 50 W m-2)}
  \item{V2.s}{Condition variable 2 (default: Vapour pressure deficit 'VPD' in hPa)}
  \item{T2.n}{Tolerance interval 2 (default: 5 hPa)}
  \item{V3.s}{Condition variable 3 (default: Air temperature 'Tair' in degC)}
  \item{T3.n}{Tolerance interval 3 (default: 2.5 degC)}
  \item{FillAll.b}{Fill all values to estimate uncertainties}
  \item{Verbose.b}{Print status information to screen}
  \item{Suffix.s}{String suffix needed for different processing setups on the same dataset (for explanations see below)}
}
\details{Initialize temporal data frame sTEMP for newly generated gap filled data and qualifiers, see \code{\link{sEddyProc_sFillInit}} for explanations on suffixes.

MDS gap filling algorithm calls the subroutines Look Up Table \code{\link{sEddyProc_sFillLUT}}
and Mean Diurnal Course \code{\link{sEddyProc_sFillMDC}} with different window sizes as described in the reference.

To run dataset only with MDC algorithm \code{\link{sEddyProc_sFillMDC}}, set condition variable V1.s to 'none'.

\describe{\item{Different processing setups on the same dataset}{
Attention: When processing the same site data set with different setups for the gap filling or flux partitioning
(e.g. due to different ustar filters),
a string suffix is needed! This suffix is added to the result column names to distinguish the results of the different setups.
}}}
\value{Gap filling results in sTEMP data frame (with renamed columns).}
\references{Reichstein, M. et al. (2005) On the separation of net ecosystem exchange
into assimilation and ecosystem respiration: review and improved algorithm. Global Change Biology, 11, 1424-1439.}
\author{AMM, TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





