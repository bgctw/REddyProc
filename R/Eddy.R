#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with sEddyProc R5 reference class definition and methods +++
#+++ Dependencies: DataFunctions.R, package 'methods' for R5 reference class
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ sEddyProc class: Initialization
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc <- setRefClass('sEddyProc', fields=list(
  ## R5 reference class for processing of site-level half-hourly eddy data
  ##author<<
  ## AMM, after example code of TW
  ##details<< with fields
  sID='character'       ##<< String with Site ID
  ,sDATA='data.frame'   ##<< Data frame with site data
  ,sINFO='list'         ##<< List with site information
  ,sTEMP='data.frame'   ##<< Data frame with temporary data
  # Note: The documenation of the class is not processed by 'inlinedocs'
))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  initialize = function(
    ##title<<
    ## sEddyProc$new - Initialization of sEddyProc
    ##description<<
    ## Creates the fields of the sEddyProc R5 reference class for processing of half-hourly eddy data
    ID.s                ##<< String with site ID
    ,Data.F             ##<< Data frame with at least three month of half-hourly site-level eddy data
    ,ColNames.V.s       ##<< Vector with selected column names, the less columns the faster the processing
    ,ColPOSIXTime.s='DateTime' ##<< Column name with POSIX time stamp
    ,...                ##<< ('...' required for initialization of class fields)
    ##author<<
    ## AMM
    # TEST: ID.s <- 'Tha', Data.F <- EddyData.F; ColPOSIXTime.s <- 'DateTime'; ColNames.V.s <- c('NEE','Rg', 'Tair', 'VPD')
  )
{
    'Creates the fields of the sEddyProc R5 reference class for processing of half-hourly eddy data'
    # Check entries
    if( !fCheckValString(ID.s) || is.na(ID.s) )
      stop('For ID, a character string must be provided!')
    fCheckColNames(Data.F, c(ColPOSIXTime.s, ColNames.V.s), 'fNewSData')
    
    ##details<<
    ## The time stamp must be provided in POSIX format, see also \code{\link{fConvertTimeToPosix}}.
    ## For required properties of the time series, see \code{\link{fCheckHHTimeSeries}}.
    fCheckHHTimeSeries(Data.F[,ColPOSIXTime.s])
    
    ##details<<
    ## Internally the half-hour time stamp is shifted to the middle of the measurement period (minus 15 minutes).
    Time.V.p <- Data.F[,ColPOSIXTime.s] - (15 * 60)  #15 minutes time offset in seconds
    
    ##details<<
    ## All other columns may only contain numeric data.
    ## Please use NA as a gap flag for missing data or low quality data not to be used in the processing.
    fCheckColNum(Data.F, ColNames.V.s, 'fNewSData')
    
    ##details<<
    ## sID is a string for the site ID.
    sID <<- ID.s
    ##details<<
    ## sDATA is a data frame with site data.
    sDATA <<- cbind(sDateTime=Time.V.p, Data.F[,ColNames.V.s])
    
    #Initialization of site data information from POSIX time stamp.
    YStart.n <- as.numeric(format(sDATA$sDateTime[1], "%Y"))
    YEnd.n <- as.numeric(format(sDATA$sDateTime[length(sDATA$sDateTime)], "%Y"))
    YNums.n <- (YEnd.n - YStart.n + 1)
    if (YNums.n > 1) {
      YName.s <- paste(substr(YStart.n,3,4), '-', substr(YEnd.n,3,4), sep='')
    } else {
      YName.s <- as.character(YStart.n)
    }
    
    ##details<<
    ## sINFO is a list containing the time series information.
    sINFO <<- list(
      DIMS=length(sDATA$sDateTime) # Number of data rows
      ,DTS=48                      # Daily time step (48 half-hours)
      ,Y.START=YStart.n            # Starting year
      ,Y.END=YEnd.n                # Ending year
      ,Y.NUMS=YNums.n              # Number of years
      ,Y.NAME=YName.s              # Name for years
    )
    
    ##details<<
    ## sTEMP is a data frame used only temporally.
    
    #Initialize class fields
    message('New sEddyProc class for site \'', ID.s,'\'')
    
    callSuper(...) # Required for initialization of class fields as last call of function
    ##value<< 
    ## Initialized fields of sEddyProc.
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ sEddyProc class: Data handling functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sGetData = function ()
  ##title<<
  ## sEddyProc$sGetData - Get internal sDATA data frame
  ##description<<
  ## Get class internal sDATA data frame
  ##author<<
  ## AMM
{
    'Get class internal sDATA data frame'
    sDATA
    ##value<< 
    ## Return data frame sDATA.
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sExportData = function ()
    ##title<<
    ## sEddyProc$sExportData - Export internal sDATA data frame
    ##description<<
    ## Export class internal sDATA data frame
    ##author<<
    ## AMM
  {
    'Export class internal sDATA data frame'
    sTEMP <<- sDATA
    sTEMP$sDateTime <<- sTEMP$sDateTime + (15 * 60)
    colnames(sTEMP) <<- c('DateTime', colnames(sTEMP)[-1])
    
    sTEMP
    ##value<< 
    ## Return data frame sDATA with time stamp shifted back to original.
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sPrintData = function ()
    ##title<<
    ## sEddyProc$sPrintData - Print internal sDATA data frame
    ##description<<
    ## Print class internal sDATA data frame
    ##author<<
    ## AMM
  {
    'Print class internal sDATA data frame'
    NumRows.n <- min(nrow(sDATA),100)

    print(sDATA[1:NumRows.n,])
    ##value<< 
    ## Print first 100 rows of data frame sDATA.
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc.example <- function() {
  ##title<<
  ## sEddyProc - Example code
  ##description<<
  ## Dummy function to show code example for sEddyProc provided below
  ##author<<
  ## AMM
  # Empty function jsut to write attribute with example code for documenation
} 
attr(sEddyProc.example,"ex") <- function(){
  #+++ Simple example code for using the sEddyProc reference class +++
  
  if (FALSE) { #Do not always execute (e.g. on package installation)
    
    #+++ Load data from text file with one header and one unit row
    Dir.s <- paste(system.file(package='REddyProc'), '/data', sep='')
    EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
    
    #+++ Add time stamp in POSIX time format
    EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='Day', Hour.s='Hour')

    #+++ Initalize R5 reference class sEddyProc for processing of eddy data
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD'))
    
    #+++ Plot data
    EddyProc.C$sPlotFingerprint('NEE')
    
    #+++ Fill gaps of NEE with MDS gap filling algorithm
    EddyProc.C$sMDSGapFill('NEE', Verbose.b=T)
    
    #+++ Plot results
    EddyProc.C$sPlotHHFluxes('NEE_f')
    EddyProc.C$sPlotFingerprint('NEE_f')
    EddyProc.C$sPlotDailySums('NEE_f','NEE_fsd')
    EddyProc.C$sPlotHHMeans('NEE_f')
    
    #+++ Export gap filled data to standard data frame
    FilledEddyData.F <- EddyProc.C$sExportData()
    
  }
}
