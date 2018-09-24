\name{sEddyProc_sGLFluxPartition}
\alias{sEddyProc_sGLFluxPartition}
\title{sEddyProc sGLFluxPartition}
\description{Daytime-based Flux partitioning after Lasslop et al. (2010)}
\usage{sEddyProc_sGLFluxPartition(..., debug = list(useLocaltime = FALSE), 
    debug.l, isWarnReplaceColumns = TRUE)}
\arguments{
  \item{\dots}{arguments to \code{\link{partitionNEEGL}} in addition to the dataset
such as \code{suffix}}
  \item{debug}{List with debugging control.\describe{
\item{useLocaltime}{if TRUE use local time zone instead of
geo-solar time to compute potential radiation}
}}
  \item{debug.l}{deprecated, renamed to debug}
  \item{isWarnReplaceColumns}{set to FALSE to avoid the warning on
replacing output columns}
}
\details{Daytime-based partitioning of measured net ecosystem fluxes into gross
primary production (GPP)
and ecosystem respiration (Reco)}
\value{Flux partitioning results are in sTEMP data frame of the class.}
\references{Lasslop G, Reichstein M, Papale D, et al. (2010) Separation of net
ecosystem exchange into assimilation and respiration using
a light response curve approach: critical issues and global evaluation.
Global Change Biology, Volume 16, Issue 1, Pages 187-208}
\author{MM, TW
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





