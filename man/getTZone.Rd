\name{getTZone}
\alias{getTZone}
\title{getTZone}
\description{extracts the timezone attribute from POSIXct but returns argument default if this attribute is missing}
\usage{getTZone(x, default = "GMT")}
\arguments{
  \item{x}{POSIXct vector}
  \item{default}{time zone returned, if x has not timezone associated or attribute is the zero string}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
getTZone( as.POSIXct("2010-07-01 16:00:00", tz="etc/GMT-1") )
getTZone( as.POSIXct("2010-07-01 16:00:00") )
getTZone( Sys.time() )		# printed with local time zone, but actually has no tz attribute
}
