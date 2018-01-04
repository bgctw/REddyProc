\name{sEddyProc_sFillMDC}
\alias{sEddyProc_sFillMDC}
\title{sEddyProc$sFillMDC - Gap filling with Mean Diurnal Course (MDC)}
\description{Mean Diurnal Course (MDC) algorithm based on average values within +/- one hour of adjacent days}
\usage{sEddyProc_sFillMDC(WinDays.i, Verbose.b = TRUE)}
\arguments{
  \item{WinDays.i}{Window size for filling in days}
  \item{Verbose.b}{Print status information to screen}
}

\value{MDC filling results in sTEMP data frame.}

\author{AMM
Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Thomas Wutzler Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Kerstin Sickel Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Olaf Menzer Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Ladislav Šigut Šigut <sigut.l@czechglobe.cz> [ctb]}





