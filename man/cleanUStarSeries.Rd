\name{cleanUStarSeries}
\alias{cleanUStarSeries}
\title{cleanUStarSeries}
\description{remove non-finite cases and omit night time data.}
\usage{cleanUStarSeries(ds, UstarColName = "Ustar", NEEColName = "NEE", 
    TempColName = "Tair", RgColName = "Rg", swThr = controlUstarSubsetting()$swThr)}\arguments{
  \item{ds}{data.frame with columns}
  \item{UstarColName}{
}
  \item{NEEColName}{
}
  \item{TempColName}{
}
  \item{RgColName}{
}
  \item{swThr}{
}
}

\value{ds with non-finite cases and cases with radiation < swThr removed.}

\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





