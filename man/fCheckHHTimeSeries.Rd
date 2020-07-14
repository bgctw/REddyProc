\name{fCheckHHTimeSeries}
\alias{fCheckHHTimeSeries}
\title{fCheckHHTimeSeries}
\description{Check half-hourly time series data}
\usage{fCheckHHTimeSeries(Time = Time.V.p, DTS = DTS.n, 
    CallFunction = if (!missing(CallFunction.s)) CallFunction.s else "", 
    Time.V.p, DTS.n, CallFunction.s)}
\arguments{
  \item{Time}{Time vector in POSIX format}
  \item{DTS}{Number of daily time steps (24 or 48)}
  \item{CallFunction}{
}
  \item{Time.V.p}{deprecated}
  \item{DTS.n}{deprecated}
  \item{CallFunction.s}{deprecated}
}
\details{The number of steps per day can be 24 (hourly) or 48 (half-hourly).

The time stamp needs to be provided in POSIX time format,

equidistant half-hours,

and stamped on the half hour.

The sEddyProc procedures require at least three months of data.

Full days of data are preferred: the total amount of data rows should be
a multiple of the daily time step, and

in accordance with FLUXNET standards, the dataset is spanning from the
end of the first (half-)hour (0:30 or 1:00, respectively) and
to midnight (0:00).}
\value{Function stops on errors.}

\author{AMM
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}





