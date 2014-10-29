\name{sMRFluxPartition}
\alias{sMRFluxPartition}
\title{sEddyProc$sMRFluxPartition - Flux partitioning after Reichstein et al. (2005)}
\description{Nighttime-based partitioning of measured net ecosystem fluxes into gross primary production (GPP) and ecosystem respiration (Reco)}
\usage{sMRFluxPartition(FluxVar.s = paste0("NEE", suffixDash.s, "_f"), 
    QFFluxVar.s = paste0("NEE", suffixDash.s, "_fqc"), QFFluxValue.n = 0, 
    TempVar.s = paste0("Tair", suffixDash.s, "_f"), QFTempVar.s = paste0("Tair", 
        suffixDash.s, "_fqc"), QFTempValue.n = 0, RadVar.s = "Rg", 
    Lat_deg.n, Long_deg.n, TimeZone_h.n, suffix.s = "", debug.l = list(useLocaltime.b = FALSE))}
\arguments{
  \item{FluxVar.s}{Net ecosystem fluxes (NEE) variable, i.e. column name}
  \item{QFFluxVar.s}{Quality flag of NEE variable}
  \item{QFFluxValue.n}{Value of quality flag for _good_ (original) data}
  \item{TempVar.s}{Filled air- or soil temperature variable (degC)}
  \item{QFTempVar.s}{Quality flag of filled temperature variable}
  \item{QFTempValue.n}{Value of temperature quality flag for _good_ (original) data}
  \item{RadVar.s}{Unfilled (original) radiation variable}
  \item{Lat_deg.n}{Latitude in (decimal) degrees}
  \item{Long_deg.n}{Longitude in (decimal) degrees}
  \item{TimeZone_h.n}{Time zone (in hours)}
  \item{suffix.s}{String inserted into input column names before identifier.}
  \item{debug.l}{List with debugging control (passed also to \code{\link{sRegrE0fromShortTerm}}.\describe{
\item{useLocaltime.b}{see details on solar vs local time}
}}
}
\details{\describe{\item{Description of newly generated variables with partitioning results:}{
\itemize{
\item PotRad - Potential radiation \cr
\item FP_NEEnight - Good (original) NEE nighttime fluxes used for flux partitioning \cr
\item FP_Temp - Good (original) temperature measurements used for flux partitioning \cr
\item E_0 - Estimated temperature sensitivity \cr
\item R_ref - Estimated reference respiration \cr
\item Reco - Estimated ecosystem respiration \cr
\item GPP_f - Estimated gross primary production \cr
}
}}

\describe{\item{Background}{
This partitioning is based on the regression of nighttime respiration with temperature 
using the Lloyd-Taylor-Function \code{\link{fLloydTaylor}}.
First the temperature sensitivity E_0 is estimated from short term data, see \code{\link{sRegrE0fromShortTerm}}.
Next the reference temperature R_ref is estimated for successive periods throughout the whole dataset (see \code{\link{sRegrRref}}.
These estimates are then used to calculate the respiration during daytime and nighttime and with this GPP.
Attention: Gap filling of the net ecosystem fluxes (NEE) and temperature measurements (Tair or Tsoil) is required
prior to the partitioning!
}}

\describe{\item{\code{suffix.s}}{
For uncertainty analysis it is practical to use different versions of gap-filling before partitioning.
In order to support easy selection of input collumns with the default, the \code{suffix.s} 
is appended to all the input collumns.
Used e.g. by \code{\link{sMDSGapFillUStar}} ).
}}

\describe{\item{Selection of daytime data based on solar time}{
The respiration-temperature regression is very
sensitive to the selection of night- and daytime data.
Nighttime is selected by a combined threshold of current solar radiation and potential radiation. 
The current implementation calculates potential radiation based on exact solar time, based on latitude and longitude.
(see \code{\link{fCalcPotRadiation}} )
Therefore it might differ from implementations that use local winter clock time instead.
}}}
\value{Flux partitioning results (see variables in details) in sTEMP data frame (with renamed columns).}
\references{Reichstein M, Falge E, Baldocchi D et al. (2005) On the separation of net ecosystem exchange 
into assimilation and ecosystem respiration: review and improved algorithm. Global Change Biology, 11, 1424-1439.}
\author{AMM,TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





