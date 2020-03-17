\name{sEddyProc_sFillMDC}
\alias{sEddyProc_sFillMDC}
\title{sEddyProc sFillMDC}
\description{Mean Diurnal Course (MDC) algorithm based on average values within +/- one hour of adjacent days}
\usage{sEddyProc_sFillMDC(WinDays.i, Verbose.b = TRUE)}
\arguments{
  \item{WinDays.i}{Window size for filling in days}
  \item{Verbose.b}{Print status information to screen}
}
\details{\describe{\item{Quality flag}{
\itemize{
\item 1: nDay <= 1
\item 2: nDay [2,5)
\item 3: nDay > 5
}
}}}
\value{MDC filling results in sTEMP data frame.}

\author{AMM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





