\name{isValidNightRecord}
\alias{isValidNightRecord}
\title{isValidNightRecord}
\description{compute logical vector of each rows in ds is its a valid night record}
\usage{isValidNightRecord(ds)}
\arguments{
  \item{ds}{data.frame with columns isNight, NEE, Temp (degC)}
}
\details{For robustness, data is trimmed to conditions at temperature > 1 degC 
but only timmed if there are more at least 12 records left}
\value{a logical vector of length nrow(ds)}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





