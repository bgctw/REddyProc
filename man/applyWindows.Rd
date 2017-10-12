\name{applyWindows}
\alias{applyWindows}
\title{applyWindows}
\description{apply a function to several windows of a data.frame}
\usage{applyWindows(ds, FUN, prevRes = list(), winSizeInDays = winSizeRefInDays, 
    winSizeRefInDays = 4L, strideInDays = floor(winSizeRefInDays/2L), 
    isVerbose = TRUE, nRecInDay = 48L, ...)}
\arguments{
  \item{ds}{dataframe to iterate}
  \item{FUN}{function to apply to subsets of the data.frame
taking a subset of the data.frame as first argument
the second: a one-row data.frame with window information (iWindow, dayStart, dayEnd, iRecStart, iRecEnd, iCentralRec)
the third: most recent valid result of calls FUN. Valid is a non-NULL result.  }
  \item{prevRes}{initial values for the list that is carried between windows}
  \item{winSizeInDays}{Window size in days}
  \item{winSizeRefInDays}{Window size in days for reference window (e.g. day-Window for night time)}
  \item{strideInDays}{step in days for shifting the window, for alligning usually a factor of winSizeRef}
  \item{isVerbose}{set to FALSE to suppress messages}
  \item{nRecInDay}{number of records within one day (for half-hourly data its 48)}
  \item{\dots}{further arguments to FUN}
}
\details{Assumes equidistant rows with nRecInDay records forming one day and reporting full days
i.e. all of the nRecInDay records are in the first day.

In order to have a common reference winSizeRefInDays is given so that results by a different window
size correspond to each window of shifting a window of winSizeRefInDays
Each window is anchord so that the center equals the center of the reference window.
This becomes important when selecting records at the edges of the series.

Usually indicate an invalid result by returning NULL.
If one still wants to store results but prevent updating the \code{prevRes} argument supplied to the next call
then return a list item (or dataframe column) \code{isValid=TRUE}.}
\value{a list with first component a data.frame with columns
\item{iWindow}{integer: counter of the window}
\item{dayStart}{integer: starting day of the window}
\item{dayEnd}{integer: ending day of the window}
\item{iRecStart}{integer: first record number of the window}
\item{iRecEnd}{integer: last record number of the window}
\item{iCentralRec}{integer: central record within the window assuming equidistant records
}

The second entry (\code{resFUN}) is a list (of length nrow(winInfo)) with each entry a result of a call to FUN.}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





