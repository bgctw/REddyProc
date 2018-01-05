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

\author{Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





