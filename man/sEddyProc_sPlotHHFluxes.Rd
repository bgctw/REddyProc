\name{sEddyProc_sPlotHHFluxes}
\alias{sEddyProc_sPlotHHFluxes}
\title{sEddyProc sPlotHHFluxes}
\description{Produce image-plot with half-hourly fluxes for each year}
\usage{sEddyProc_sPlotHHFluxes(Var.s, QFVar.s = "none", 
    QFValue.n = NA_real_, Format.s = "pdf", 
    Dir.s = "plots")}
\arguments{
  \item{Var.s}{(Filled) variable to plot}
  \item{QFVar.s}{Quality flag of variable to be filled}
  \item{QFValue.n}{Value of quality flag for data to plot}
  \item{Format.s}{Graphics file format ('pdf' or 'png')}
  \item{Dir.s}{Directory for plotting}
}
\details{Generates image in specified format ('pdf' or 'png') with half-hourly fluxes
and their daily means,
see also \code{\link{sEddyProc_sPlotHHFluxesY}}.}


\author{KS, AMM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





