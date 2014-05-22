\name{sPlotHHFluxes}
\alias{sPlotHHFluxes}
\title{sEddyProc$sPlotHHFluxes - Image with half-hourly fluxes for each year}
\description{Generates image in specified format ('pdf' or 'png') with half-hourly fluxes and their daily means,
see also \code{\link{sPlotHHFluxesY}}.}
\usage{sPlotHHFluxes(Var.s, QFVar.s = "none", QFValue.n = NA_real_, 
    Format.s = "pdf", Dir.s = "plots")}
\arguments{
  \item{Var.s}{(Filled) variable to plot}
  \item{QFVar.s}{Quality flag of variable to be filled}
  \item{QFValue.n}{Value of quality flag for data to plot}
  \item{Format.s}{Graphics file format ('pdf' or 'png')}
  \item{Dir.s}{Directory for plotting}
}



\author{KS, AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





