\name{getStartRecsOfWindows}
\alias{getStartRecsOfWindows}
\title{getStartRecsOfWindows}
\description{compute the starting positions of windows for given size}
\usage{getStartRecsOfWindows(nRec, winSizeInDays = winSizeRefInDays, 
    winSizeRefInDays = 4L, strideInDays = floor(winSizeRefInDays/2L), 
    nRecInDay = 48L)}
\arguments{
  \item{nRec}{numeric scalar: number of records}
  \item{winSizeInDays}{Window size in days}
  \item{winSizeRefInDays}{Window size in days for reference window (e.g. day-Window for night time)}
  \item{strideInDays}{step in days for shifting the window, for alligning usually a factor of winSizeRef}
  \item{nRecInDay}{number of records within one day (for half-hourly data its 48)}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





