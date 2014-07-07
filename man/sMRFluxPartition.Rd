\name{sMRFluxPartition}
\alias{sMRFluxPartition}
\title{sMRFluxPartition - Flux partitioning after Reichstein et al. (2005)}
\description{Nighttime-based partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco)}
\usage{sMRFluxPartition(FluxVar.s = paste0("NEE", suffixDash.s, "_f"), 
    QFFluxVar.s = paste0("NEE", suffixDash.s, "_fqc"), QFFluxValue.n = 0, 
    TempVar.s = paste0("Tair", suffixDash.s, "_f"), QFTempVar.s = paste0("Tair", 
        suffixDash.s, "_fqc"), QFTempValue.n = 0, RadVar.s = "Rg", 
    Lat_deg.n, Long_deg.n, TimeZone_h.n, suffix.s = "", debug.l = list(useLocaltime.b = FALSE))}
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
  \item{suffix.s}{string inserted into column names before identifier (see \code{\link{sMDSGapFillUStar}}.}
  \item{debug.l}{list with debugging control, see \code{\link{sRegrE0fromShortTerm}}.\describe{
\item{useLocaltime.b}{by default corrects hour (given in local winter time) for latitude to solar time
<< where noon is exactly at 12:00. Set this to TRUE to compare to code that uses local winter time}
}}
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





