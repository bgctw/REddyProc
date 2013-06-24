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
    ,DTS.n=48           ##<< Daily time steps
    ,...                ##<< ('...' required for initialization of class fields)
    ##author<<
    ## AMM
    # TEST: ID.s <- 'Tha'; Data.F <- EddyDataWithPosix.F; ColPOSIXTime.s <- 'DateTime'; ColNames.V.s <- c('NEE','Rg', 'Tair', 'VPD'); DTS.n=48
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
    fCheckHHTimeSeries(Data.F[,ColPOSIXTime.s], DTS.n=DTS.n, 'sEddyProc.initialize')
    
    ##details<<
    ## Internally the half-hour time stamp is shifted to the middle of the measurement period (minus 15 minutes or 30 minutes).
    Time.V.p <- Data.F[,ColPOSIXTime.s] - (0.5 * 24/DTS.n * 60 * 60)  #half-period time offset in seconds
    
    ##details<<
    ## All other columns may only contain numeric data.
    ## Please use NA as a gap flag for missing data or low quality data not to be used in the processing.
    fCheckColNum(Data.F, ColNames.V.s, 'sEddyProc.initialize')
    
    ##details<<
    ## sID is a string for the site ID.
    sID <<- ID.s
    ##details<<
    ## sDATA is a data frame with the site data.
    sDATA <<- cbind(sDateTime=Time.V.p, Data.F[,ColNames.V.s])
    ##details<<
    ## sTEMP is a temporal data frame with the processing results.
    sTEMP <<- data.frame(sDateTime=Time.V.p)
    ## sDATA is a data frame with site data.
    sDATA <<- cbind(sDateTime=Time.V.p, Data.F[,ColNames.V.s, drop=FALSE])    
    #Initialization of site data information from POSIX time stamp.
    YStart.n <- as.numeric(format(sDATA$sDateTime[1], '%Y'))
    YEnd.n <- as.numeric(format(sDATA$sDateTime[length(sDATA$sDateTime)], '%Y'))
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
      ,DTS=DTS.n                   # Number of daily time steps (24 or 48)
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
    lDATA <- sDATA
    lDATA$sDateTime <- lDATA$sDateTime + (15 * 60)
    colnames(lDATA) <- c('DateTime', colnames(lDATA)[-1])
    
    lDATA
    ##value<< 
    ## Return data frame sDATA with time stamp shifted back to original.
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sEddyProc$methods(
  sExportResults = function ()
    ##title<<
    ## sEddyProc$sExportData - Export internal sTEMP data frame with result columns
    ##description<<
    ## Export class internal sDATA data frame
    ##author<<
    ## AMM
  {
    'Export class internal sTEMP data frame with result columns'
    
    sTEMP[,-1]
    ##value<< 
    ## Return data frame sTEMP with results.
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
attr(sEddyProc.example,'ex') <- function(){
  #+++ Simple example code for using the sEddyProc reference class +++
  
  if (FALSE) { #Do not always execute (e.g. on package installation)
    
    #+++ Load data with one header and one unit row from (tab-delimited) text file
    Dir.s <- paste(system.file(package='REddyProc'), '/data', sep='')
    EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
    #+++ If not provided, calculate VPD from Tair and rH
    EddyData.F <- cbind(EddyData.F,VPD=fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair))
    
    #+++ Add time stamp in POSIX time format
    EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='Day', Hour.s='Hour')

    #+++ Initalize R5 reference class sEddyProc for processing of eddy data with all variables needed for processing later
    EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD'))
    
    #+++ Generate plots of all data in directory \plots (of current R working dir)
    EddyProc.C$sPlotHHFluxes('NEE')
    EddyProc.C$sPlotFingerprint('Rg')
    EddyProc.C$sPlotDiurnalCycle('Tair')
    #+++ Plot individual years to screen (of current R graphics device)
    EddyProc.C$sPlotHHFluxesY('NEE', Year.i=1998)
    EddyProc.C$sPlotFingerprintY('NEE', Year.i=1998)
    
    #+++ Fill gaps in variables with MDS gap filling algorithm
    EddyProc.C$sMDSGapFill('NEE', Verbose.b=T)
    EddyProc.C$sMDSGapFill('Rg', Verbose.b=T)
    
    #+++ Generate plots of filled data in directory \plots (of current R working dir)
    EddyProc.C$sPlotHHFluxes('NEE_f')
    EddyProc.C$sPlotFingerprint('NEE_f')
    EddyProc.C$sPlotDailySums('NEE_f','NEE_fsd')
    EddyProc.C$sPlotDiurnalCycle('NEE_f')
    
    #+++ Plot other individual years/months to screen (of current R graphics device)
    EddyProc.C$sPlotDailySumsY('NEE_f','NEE_fsd', Year.s=1998)
    EddyProc.C$sPlotDiurnalCycleM('NEE_f', Month.i=1)
    
    #+++ Export gap filled data to standard data frame
    FilledEddyData.F <- EddyProc.C$sExportResults()
    
    #+++ Save results into (tab-delimited) text file in directory \out
    CombinedData.F <- cbind(EddyData.F, FilledEddyData.F)
    fWriteDataframeToFile(CombinedData.F, 'DE-Tha-Results.txt', 'out')
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Extra: Examples of extended usage for advanced users
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #+++ Add some (non-sense) example vectors:
    #+++ Quality flag vector (e.g. from applying ustar filter)
    EddyDataWithPosix.F <- cbind(EddyDataWithPosix.F, QF=rep(c(1,0,1,0,1,0,0,0,0,0),nrow(EddyData.F)/10))
    #+++ Step function vector to simulate e.g. high/low water table
    EddyDataWithPosix.F <- cbind(EddyDataWithPosix.F, Step= ifelse(EddyData.F$Day < 200 | EddyData.F$Day > 250, 0, 1))

    #+++ Initialize eddy processing class with more columns
    EddyTest.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE', 'LE', 'H', 'Rg', 'Tair', 'Tsoil', 'Rh', 'VPD', 'QF', 'Step'))

    #+++ Gap fill variable after applying quality flag QF with (non-default) variables and limits
    EddyTest.C$sMDSGapFill('LE', QFVar.s='QF', QFValue.n=0, V1.s='Rg', T1.n=30, V2.s='Tsoil', T2.n=2, 'Step', 0.1, Verbose.b=T)

    #+++ Try the gap filling subroutines individually for different window sizes and up to five variables and limits
    EddyTest.C$sFillInit('NEE') #Initalize to fill variable 'NEE'
    Result_Step1.F <- EddyTest.C$sFillLUT(3, 'Rg',50, 'Rh',15, 'Tair',2.5, 'Tsoil',1, 'Step',0.1)
    Result_Step2.F <- EddyTest.C$sFillLUT(6, 'Tair',2.5, 'VPD',3, 'Step',0.1)
    Result_Step3.F <- EddyTest.C$sFillMDC(3)
    EddyTest.C$sPlotHHFluxes('VAR_f') #Individual fill result columns are called 'VAR_...'
  
  }
}
