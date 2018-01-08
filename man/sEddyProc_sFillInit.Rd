\name{sEddyProc_sFillInit}
\alias{sEddyProc_sFillInit}
\title{sEddyProc$sFillInit - Initialize gap filling}
\description{Initializes data frame sTEMP for newly generated gap filled data and qualifiers.}
\usage{sEddyProc_sFillInit(Var.s, QFVar.s = "none", 
    QFValue.n = NA_real_, FillAll.b = TRUE)}
\arguments{
  \item{Var.s}{Variable to be filled}
  \item{QFVar.s}{Quality flag of variable to be filled}
  \item{QFValue.n}{Value of quality flag for _good_ (original) data, other data is set to missing}
  \item{FillAll.b}{Fill all values to estimate uncertainties}
}
\details{Description of newly generated variables with gap filled data and qualifiers: \cr

VAR\emph{_orig} - Original values used for gap filling \cr
VAR\emph{_f   } - Original values and gaps filled with mean of selected datapoints (condition depending on gap filling method) \cr
VAR\emph{_fqc} - Quality flag assigned depending on gap filling method and window length (0 = original data, 1 = most reliable, 2 = medium, 3 = least reliable) \cr
VAR\emph{_fall} - All values considered as gaps (for uncertainty estimates) \cr
VAR\emph{_fall_qc} - Quality flag assigned depending on gap filling method and window length (1 = most reliable, 2 = medium, 3 = least reliable) \cr
VAR\emph{_fnum} - Number of datapoints used for gap-filling \cr
VAR\emph{_fsd} - Standard deviation of datapoints used for gap filling (uncertainty) \cr
VAR\emph{_fmeth} - Method used for gap filling (1 = similar meteo condition (sFillLUT with Rg, VPD, Tair), 2 = similar meteo (sFillLUT with Rg only), 3 = mean diurnal course (sFillMDC)) \cr
VAR\emph{_fwin} - Full window length used for gap filling \cr

Long gaps (larger than 60 days) are not filled.}


\author{AMM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





