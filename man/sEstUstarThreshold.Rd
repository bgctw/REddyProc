\name{sEstUstarThreshold}
\alias{sEstUstarThreshold}
\title{sEddyProc$sEstUstarThreshold - Estimating ustar threshold}
\description{Calling \code{\link{usEstUstarThreshold}} for class data and storing results}
\usage{sEstUstarThreshold(UstarColName = "Ustar", NEEColName = "NEE", 
    TempColName = "Tair", RgColName = "Rg", ...)}
\arguments{
  \item{UstarColName}{column name for UStar}
  \item{NEEColName}{column name for NEE}
  \item{TempColName}{column name for air temperature}
  \item{RgColName}{column name for solar radiation for omitting night time data}
  \item{\dots}{further arguments to \code{\link{usEstUstarThreshold}}}
}

\value{restul of \code{\link{usEstUstarThreshold}}. In addition the result is stored
in class variable sUSTAR and the bins as additional columsn to sDATA}

\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





