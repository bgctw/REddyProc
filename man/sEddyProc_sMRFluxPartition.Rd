\name{sEddyProc_sMRFluxPartition}
\alias{sEddyProc_sMRFluxPartition}
\title{sEddyProc sMRFluxPartition}
\description{Nighttime-based partitioning of net ecosystem fluxes into gross fluxes GPP and REco}
\usage{sEddyProc_sMRFluxPartition(FluxVar = if (missing(FluxVar.s)) "NEE_f" else FluxVar.s, 
    QFFluxVar = if (missing(QFFluxVar.s)) "NEE_fqc" else QFFluxVar.s, 
    QFFluxValue = if (missing(QFFluxValue.n)) 0L else QFFluxValue.n, 
    TempVar = if (missing(TempVar.s)) "Tair_f" else TempVar.s, 
    QFTempVar = if (missing(QFTempVar.s)) "Tair_fqc" else QFTempVar.s, 
    QFTempValue = if (missing(QFTempValue.n)) 0 else QFTempValue.n, 
    RadVar = if (missing(RadVar.s)) "Rg" else RadVar.s, 
    TRef = if (missing(T_ref.n)) 273.15 + 
        15 else T_ref.n, suffix = if (missing(Suffix.s)) "" else Suffix.s, 
    FluxVar.s, QFFluxVar.s, QFFluxValue.n, 
    TempVar.s, QFTempVar.s, QFTempValue.n, 
    RadVar.s, T_ref.n, Suffix.s, debug.l, 
    debug = if (!missing(debug.l)) debug.l else list(useLocaltime = FALSE), 
    parsE0Regression = list())}
\arguments{
  \item{FluxVar}{Variable
name of column with original and
filled net ecosystem fluxes (NEE)}
  \item{QFFluxVar}{Quality
flag of NEE variable}
  \item{QFFluxValue}{Value
of quality flag for _good_ (original) data}
  \item{TempVar}{Filled
air- or soil temperature variable (degC)}
  \item{QFTempVar}{Quality
flag of filled temperature variable}
  \item{QFTempValue}{Value
of temperature quality flag for _good_
(original) data}
  \item{RadVar}{Unfilled
(original) radiation variable}
  \item{TRef}{Reference
temperature in Kelvin (degK)
used in \code{fLloydTaylor} for regressing Flux and Temperature}
  \item{suffix}{String
suffix needed for different processing
setups on the same dataset (for explanations see below)}
  \item{FluxVar.s}{deprecated}
  \item{QFFluxVar.s}{deprecated}
  \item{QFFluxValue.n}{deprecated}
  \item{TempVar.s}{deprecated}
  \item{QFTempVar.s}{deprecated}
  \item{QFTempValue.n}{deprecated}
  \item{RadVar.s}{deprecated}
  \item{T_ref.n}{deprecated}
  \item{Suffix.s}{deprecated}
  \item{debug.l}{deprecated}
  \item{debug}{List
with debugging control
(passed also to \code{sEddyProc_sRegrE0fromShortTerm}
for providing \code{fixedE0 = myE0}).\describe{
\item{useLocaltime}{see details on solar vs local time}
}}
  \item{parsE0Regression}{list
with further parameters passed down to
\code{sEddyProc_sRegrE0fromShortTerm} and \code{fRegrE0fromShortTerm},
such as \code{TempRange}}
}
\details{\describe{\item{
Description of newly generated variables with partitioning results:}{
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
This partitioning is based on the regression of nighttime respiration with
temperature using the Lloyd-Taylor-Function \code{\link{fLloydTaylor}}.
First the temperature sensitivity E_0 is estimated from short term data,
see \code{sEddyProc_sRegrE0fromShortTerm}.
Next the reference temperature R_ref is estimated for successive periods
throughout the whole dataset (see \code{sEddyProc_sRegrRref}).
These estimates are then used to calculate the respiration during daytime
and nighttime and with this GPP.
Attention: Gap filling of the net ecosystem fluxes (NEE) and temperature
measurements (Tair or Tsoil) is required
prior to the partitioning!
}}

\describe{\item{Selection of daytime data based on solar time}{
The respiration-temperature regression is very
sensitive to the selection of night- and daytime data.
Nighttime is selected by a combined threshold of current solar radiation
and potential radiation.
The current implementation calculates potential radiation based on exact
solar time, based on latitude and longitude.
(see \code{\link{fCalcPotRadiation}})
Therefore it might differ from implementations that use local winter
clock time instead.
}}

\describe{\item{Different processing setups on the same dataset}{
Attention: When processing the same site data set with different setups for
the gap filling or flux partitioning
(e.g. due to different ustar filters),
a string suffix is needed! This suffix is added to the result column names
to distinguish the results of the different setups.
If a suffix is provided and if the defaults for FluxVar and QFFluxVar
are used, the suffix will be added to their variable names
(e.g. 'NEE_f' will be renamed to 'NEE_uStar_f' and 'NEE_fqc' to
'NEE_uStar_fqc' for the suffix = 'uStar').
Currently, this works only with defaults of FluxVar = 'NEE_f'
and QFFluxVar = 'NEE_fqc'.
}}}
\value{Flux partitioning results (see variables in details) in sTEMP data frame
(with renamed columns).
On success, return value is NULL. On failure an integer scalar error code
is returned:
-111 if regression of E_0 failed due to insufficient relationship in the data.}
\references{Reichstein M, Falge E, Baldocchi D et al. (2005) On the separation of
net ecosystem exchange
into assimilation and ecosystem respiration: review and improved algorithm.
Global Change Biology, 11, 1424-1439.}
\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





