\name{binUstar}
\alias{binUstar}
\title{binUstar}
\description{Bin the NEE for a number of classes of UStar classes}
\usage{binUstar(NEE.v, Ustar.v, UstarClasses = controlUstarSubsetting()$UstarClasses, 
    isUStarSorted = FALSE)}
\arguments{
  \item{NEE.v}{vector with value of Net Ecosystem exchange}
  \item{Ustar.v}{vector with u* (friction velocity (m2/s)}
  \item{UstarClasses}{the number of binning classes}
  \item{isUStarSorted}{set to TRUE, if NEE and Ustar are already sorted by increasin Ustar values (performance gain)}
}



\author{(Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)}




\examples{
Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
EddyData.F <- ds <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
EddyDataWithPosix.F <- ds <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
dss <- subset(EddyDataWithPosix.F, DoY >= 150 & DoY <= 250)
(res <- binUstar(dss$NEE, dss$Ustar))
(resFW1 <- estUstarThresholdSingleFw1Binned(res))
(resFW2 <- estUstarThresholdSingleFw2Binned(res))
}
