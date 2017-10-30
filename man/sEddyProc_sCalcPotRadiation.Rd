\name{sEddyProc_sCalcPotRadiation}
\alias{sEddyProc_sCalcPotRadiation}
\title{sEddyProc sCalcPotRadiation}
\description{compute potential radiation from position and time}
\usage{sEddyProc_sCalcPotRadiation(useSolartime.b = TRUE)}
\arguments{
  \item{useSolartime.b}{by default corrects hour (given in local winter time) for latitude to solar time
<< (where noon is exactly at 12:00). Set this to FALSE to directly use local winter time}
}

\value{column PotRad_NEW in sTEMP}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





