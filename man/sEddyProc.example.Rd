\name{sEddyProc.example}
\alias{sEddyProc.example}
\title{sEddyProc - Example code}
\description{Dummy function to show code example for sEddyProc provided below}
\usage{
sEddyProc.example()
}



\author{AMM
(Antje M. Moffat, Kerstin Sickel, Thomas Wutzler)}




\examples{
#+++ Simple example code for using the sEddyProc reference class +++

if (FALSE) { #Do not always execute (e.g. on package installation)
  
  #+++ Load data from text file with one header and one unit row
  Dir.s <- paste(system.file(package='REddyProc'), '/data', sep='')
  EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
  
  #+++ Add time stamp in POSIX time format
  EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='Day', Hour.s='Hour')

  #+++ Initalize R5 reference class sEddyProc for processing of eddy data
  EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD'))
  
  #+++ Fill gaps of NEE with MDS gap filling algorithm
  EddyProc.C$sMDSGapFill('NEE', Verbose.b=T)
  
  #+++ Plot results
  EddyProc.C$sPlotHHFluxes('NEE_f')
  
  #+++ Export gap filled data to standard data frame
  FilledEddyData.F <- EddyProc.C$sExportData()
  
}
}
