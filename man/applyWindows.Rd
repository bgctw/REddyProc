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
the third a list that can transport values from previous fits, i.e. the third item of its return value 
with first item a list (objects that cannot be rowbound) 
second item a single row data.frame (summary that can be row-bound)
third item a list that is will be provided to the call of this function on the next window as the second argument}
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
This becomes important when selecting records at the edges of the series.}
\value{a list with components that correspond to the three result components of FUN
\item{resOptList}{a list with restults of length nrow(ds)}
\item{summary}{a data.frame with nrow(ds) rows, also including window information (iWindow, dayStart, dayEnd, iRecStart, iRecEnd, iCentralRec)}
\item{prevRes}{last result of the values carried over between windows
}}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





