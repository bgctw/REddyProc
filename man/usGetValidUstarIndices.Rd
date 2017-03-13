\name{usGetValidUstarIndices}
\alias{usGetValidUstarIndices}
\title{usGetValidUstarIndices}
\description{getremove non-finite cases and omit night time data.}
\usage{usGetValidUstarIndices(ds, UstarColName = "Ustar", 
    NEEColName = "NEE", TempColName = "Tair", RgColName = "Rg", 
    swThr = usControlUstarSubsetting()$swThr)}
\arguments{
  \item{ds}{data.frame with columns}
  \item{UstarColName}{column name for UStar}
  \item{NEEColName}{column name for NEE}
  \item{TempColName}{column name for air temperature}
  \item{RgColName}{column name for solar radiation for omitting night time data}
  \item{swThr}{threshold below which data is acknowledged as night time respiration.}
}

\value{boolean vector with non-finite cases and cases with radiation < swThr set to FALSE.}

\author{TW
(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}





