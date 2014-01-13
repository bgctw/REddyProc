\name{sMRFluxPartition}
\alias{sMRFluxPartition}
\title{sMRFluxPartition - Flux partitioning after Reichstein et al. (2005)}
\description{Partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco)}
\usage{sMRFluxPartition(FluxVar.s = "NEE_f", QFFluxVar.s = "NEE_fqc", 
    QFFluxValue.n = 0, TempVar.s = "Tair_f", QFTempVar.s = "Tair_fqc", 
    QFTempValue.n = 0, RadVar.s = "Rg", Lat_deg.n, Long_deg.n, 
    TimeZone_h.n)}
\arguments{
  \item{FluxVar.s}{Variable of net ecosystem fluxes}
  \item{QFFluxVar.s}{Quality flag of variable}
  \item{QFFluxValue.n}{Value of quality flag for _good_ (original) data}
  \item{TempVar.s}{Filled air or soil temperature variable (degC)}
  \item{QFTempVar.s}{Quality flag of filled temperature variable}
  \item{QFTempValue.n}{Value of temperature quality flag for _good_ (original) data}
  \item{RadVar.s}{Unfilled (original) radiation variable}
  \item{Lat_deg.n}{Latitude in (decimal) degrees}
  \item{Long_deg.n}{Longitude in (decimal) degrees}
  \item{TimeZone_h.n}{Time zone (in hours)}
}
\details{Description of newly generated variables with partitioning results: \cr

PotRad - Potential radiation \cr
FP_NEEnight - Good (original) NEE nighttime fluxes  used for flux partitioning \cr
FP_Temp - Good (original) temperature measurements used for flux partitioning \cr
E_0 - Estimated temperature sensitivity \cr
R_ref - Estimated reference respiration \cr
Reco - Estimated ecosystem respiration \cr
GPP_f - Estimated gross primary production \cr}
\value{Flux partitioning results in sTEMP data frame (with renamed columns).}
\references{Reichstein M, Falge E, Baldocchi D et al. (2005) On the separation of net ecosystem exchange 
into assimilation and ecosystem respiration: review and improved algorithm. Global Change Biology, 11, 1424-1439.}
\author{AMM
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





