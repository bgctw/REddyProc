\name{sEddyProc_sFillLUT}
\alias{sEddyProc_sFillLUT}
\title{sEddyProc$sFillLUT - Gap filling with Look-Up Table (LUT)}
\description{Look-Up Table (LUT) algorithm of up to five conditions within prescribed window size}
\usage{sEddyProc_sFillLUT(WinDays.i, V1.s = "none", 
    T1.n = NA_real_, V2.s = "none", T2.n = NA_real_, 
    V3.s = "none", T3.n = NA_real_, V4.s = "none", 
    T4.n = NA_real_, V5.s = "none", T5.n = NA_real_, 
    Verbose.b = TRUE)}
\arguments{
  \item{WinDays.i}{Window size for filling in days}
  \item{V1.s}{Condition variable 1}
  \item{T1.n}{Tolerance interval 1}
  \item{V2.s}{Condition variable 2}
  \item{T2.n}{Tolerance interval 2}
  \item{V3.s}{Condition variable 3}
  \item{T3.n}{Tolerance interval 3}
  \item{V4.s}{Condition variable 4}
  \item{T4.n}{Tolerance interval 4}
  \item{V5.s}{Condition variable 5}
  \item{T5.n}{Tolerance interval 5}
  \item{Verbose.b}{Print status information to screen}
}

\value{LUT filling results in sTEMP data frame.}

\author{AMM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





