#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with functions for data input and output
#+++ Dependencies: DataFunctions.R
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Load ascii format data
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fLoadTXTIntoDataframe <- function(
  ##title<<
  ## Load text file with one header and one (discarded) unit row into data frame
  ##description<<
  ## If gaps with the flag -9999.0 exist, these are set to NA.
  FileName.s            ##<< File name
  ,Dir.s                ##<< Directory
  ) 
  ##author<<
  ## AMM
{
  InputFile.s <- fSetFile(FileName.s, Dir.s, T, 'fLoadTXTIntoDataframe')  

  # Read in header
  Header.F <- read.csv(InputFile.s, header=T, sep='', dec='.')
  # Skip unit row and read in data
  Data.F <- read.csv(InputFile.s, header=F, skip=2, sep='', dec='.')
  # Rename columns with header information
  names(Data.F) <- names(Header.F)
  message('Loaded file ', FileName.s, ' with the following headers:')
  message('*** ', paste(colnames(Data.F), collapse=' ', sep=''))
  # Convert gap flags to NA
  Data.F <- fConvertGapsToNA(Data.F)
  
  Data.F 
  ##value<<
  ## Data frame with data from text file.
}

attr(fLoadTXTIntoDataframe, 'ex') <- function() {
  # Example code
  if( file.exists('data/Example_DETha98.txt') )
    EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt','data')
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Load NetCDF data
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fLoadFluxNCIntoDataframe <- function(
  ##title<<
  ## Load specified variables and time stamp information from NetCDF file
  ##description<<
  ## The time stamp information needs to be provided as variables 'year', 'month', 'day', 'hour' (Fluxnet BGI format).
  VarList.V.s           ##<< Vector of variables to be read in 
  ,FileName.s           ##<< File name             
  ,Dir.s                ##<< Directory      
  ) 
  ##author<<
  ## AMM, KS
  # TEST: FileName.s <- 'Example_DE-Tha.1996.2006.hourly.nc'; Dir.s <- 'data'; VarList.V.s <- c('NEE', 'Rg', 'Rh', 'Tair', 'NEE_f')
{
  # Read in time variables
  Data.F <- fAddNCFVar(NULL, 'year', FileName.s, Dir.s)
  Data.F <- fAddNCFVar(Data.F, 'month', FileName.s, Dir.s)
  Data.F <- fAddNCFVar(Data.F, 'day', FileName.s, Dir.s)
  Data.F <- fAddNCFVar(Data.F, 'hour', FileName.s, Dir.s)
  
  # Convert time format to POSIX
  Data.F <- fConvertTimeToPosix(Data.F, 'YMDH', Year.s = 'year', Month.s='month', Day.s = 'day', Hour.s = 'hour')
  
  # Read in variables from a given list of needed variables 
  for (i in 1: length(VarList.V.s))  {
    Data.F <- fAddNCFVar(Data.F, VarList.V.s[i], FileName.s, Dir.s)
  }
  message('Loaded BGI Fluxnet NC file: ', FileName.s, ' with the following headers:')
  message('*** ', paste(colnames(Data.F), collapse=' ', sep=''))
  
  Data.F 
  ##value<<
  ## Data frame with data from nc file.
}
attr(fLoadFluxNCIntoDataframe, 'ex') <- function() {
  # Example code
  if( file.exists('inst/MDSdata/Example_DE-Tha.1996.2006.hourly.nc') )
    EddyNCData.F <- fLoadFluxNCIntoDataframe(c('NEE', 'Rg', 'NEE_f'), 'Example_DE-Tha.1996.2006.hourly.nc', 'inst/MDSdata')
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fAddNCFVar <- function(
  ##title<<
  ## Add variable from NetCDF file to data frame
  Data.F                ##<< Data frame
  ,Var.s                ##<< Variable name
  ,FileName.s           ##<< NetCDF file name
  ,Dir.s                ##<< Directory      
  )
  ##author<<
  ## AMM, KS
  # TEST: Data.F <- NULL; Var.s <- 'time'; FileName.s <- 'Example_DE-Tha.1996.2006.hourly.nc'; Dir.s <- 'data'
{
  InputNCF.s <- fSetFile(FileName.s, Dir.s, T, 'fAddNCFVar')
  
  if( !require(RNetCDF) )  
    stop('Required package RNetCDF could not be loaded!')   # for handling Fluxnet netcdf files
  NCFile.C <- open.nc(InputNCF.s)
  tryCatch({
    NewCol.M <- cbind(var.get.nc(NCFile.C,Var.s))
    # Rename columns with name of variable
    colnames(NewCol.M) <- Var.s
    # Set column to data frame    
    Data.F <- as.data.frame(cbind(Data.F,NewCol.M))
  }, 
   finally = close.nc(NCFile.C)
  )
  
  Data.F
  ##value<<
  ## Data frame with new nc variable added.
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Write data to file
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fWriteDataframeToFile <- function(
  ##title<<
  ## Write data frame to ASCII or NetCDF file
  Data.F                ##<< Data frame     
  ,BaseName.s           ##<< File base name
  ,Dir.s                ##<< Directory
  ,FileType.s='txt'     ##<< File output type
  ,OutputName.s='none'  ##<< Optional: Alternative complete file name
)
  ##author<<
  ## AMM, KS
  ##details<<
  ## 'txt' - for tab delimited text file
  ## 'nc' - for NetCDF file
  ## With missing values flagged as -9999.0
  # TEST: Data.F <- EddyData.F; BaseName.s <- 'OutputTest'; FileType.s='nc'; Dir.s <- 'data'; OutputName.s='none';
{
  # Set file name
  if( OutputName.s == 'none' ) {
    FileName.s <- paste(BaseName.s, '_', format(Sys.time(), '%y-%m-%d_%H-%M-%S'), '.', FileType.s, sep='')
    OutputFile.s <- fSetFile(FileName.s, Dir.s, F, 'fWriteDataframeToFile')
  } else {
    OutputFile.s <- fSetFile(OutputName.s, '', F, 'fWriteDataframeToFile')
  }
  
  # Convert NAs to gap flag
  Data.F <- fConvertNAsToGap(Data.F)
  
  # Write data to files
  if( FileType.s=='txt') {
    # Write tab delimited file
    # supressWarnings()
    write.table(Data.F, file=OutputFile.s, col.names=T, row.names=F, sep='\t', quote=F)
    message('Wrote tab separated textfile: ', OutputFile.s)
    
  } else if( FileType.s=='nc') {
    # Write NetCDF file
    if( !require(RNetCDF) )  
      stop('Required package RNetCDF could not be loaded!')   # for handling Fluxnet netcdf files
    NCFile.C <- create.nc(OutputFile.s, clobber=T, large=T, prefill=F)
    tryCatch({
      dim.def.nc(NCFile.C, 'time', unlim=TRUE)
      for (Var.i in 1:ncol(Data.F))  {
        if( is.numeric(Data.F[,Var.i]) ) 
        {
          VarType.s <- 'NC_DOUBLE'
          var.def.nc(NCFile.C, varname=names(Data.F)[Var.i], vartype=VarType.s, dimensions='time')
          var.put.nc(NCFile.C, variable=names(Data.F)[Var.i], data=Data.F[,Var.i])
          att.put.nc(NCFile.C, variable=names(Data.F)[Var.i], name='miss_val', type=VarType.s, value=-9999.0)
        }
        else next; #Skip non-numeric columsn for now !!!TODO
      }
    }, 
             finally = close.nc(NCFile.C)
    )
    message('Wrote numeric columns to nc file: ', OutputFile.s)
  }
  
  ##value<<
  ## Output of data frame written to file of specified type.
}

attr(fWriteDataframeToFile, 'ex') <- function() {
  # Example code
  if( file.exists('data/Example_DETha98.txt') ) {
    EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt','data')
    if (FALSE) { #Example code, do not always execute (e.g. on package installation)
      fWriteDataframeToFile(EddyData.F, 'OutputTest', 'out')
      fWriteDataframeToFile(EddyData.F, 'OutputTest', 'out', 'nc')
    }
  }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ File handling
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fInitFilesDir <- function(
  ##title<<
  ## Get all available files with specific file extension in directory
  Dir.s           ##<< Directory to be searched for files
  ,lFileExt.s     ##<< File extension specification
)
  ##author<<
  ## AMM
{
  # List files in path and grep files with specified file extension
  list.files(path=Dir.s)[grep(lFileExt.s, list.files(path=Dir.s))]
  ##value<< 
  ## Character vector with names of all available site files.
}

attr(fInitFilesDir, 'ex') <- function() {
  # Example code
  if( file.exists('data/Example_DETha98.txt') )
    fInitFilesDir('data','txt')
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fStripFileExtension <- function(
  ##title<<
  ## Strip file extension 
  lFiles.V.s     ##<< Vector with names of all available site files
)
  ##author<<
  ## AMM
{
  # RegExp: Search for first dot and replace rest of string with nothing
  sub('[.].*','',lFiles.V.s)
  ##value<< 
  ## Character vector containing the first part of file names (before first dot in file name).
}

attr(fStripFileExtension, 'ex') <- function() {
  # Example code
  if( file.exists('data/Example_DETha98.txt') )
    fStripFileExtension(fInitFilesDir('data','txt'))
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fSetFile <- function(
  ##title<<
  ## Set file name with path and check if directory and/or file exists
  FileName.s            ##<< File name
  ,Dir.s                ##<< Directory
  ,IO.b                 ##<< Input/output flag, TRUE for input, FALSE for output
  ,CallFunction.s=''    ##<< Name of function called from
)
  ##author<<
  ## AMM
  # TEST: Dir.s <- 'data'; FileName.s <- 'test'; FileName.s <- 'Example_DETha98.txt'; IO.b <- T
{
  # Check if string for directory provided
  Dir.b <- fCheckValString(Dir.s)

  # Check if directory exists
  if ( Dir.b && !file.exists(Dir.s) && IO.b )
    stop(CallFunction.s, '::: Directory does not exist: ', Dir.s)
  
  # Make directory if mode is output
  if( Dir.b && !file.exists(Dir.s) && !IO.b ) {
    dir.create(Dir.s)
    if( !file.exists(Dir.s) )
      stop(CallFunction.s, '::: Directory could not be created: ', Dir.s)
  }
  
  # Set file name accordingly
  File.s <- if( Dir.b ) {  paste(Dir.s, '/', FileName.s, sep='')
  } else { FileName.s }
  
  # If input file, check if file exists
  if ( IO.b && !file.exists(File.s))
    stop(CallFunction.s, '::: File does not exist: ', File.s)
  
  File.s
  ##value<< 
  ## Returns name of file with complete path.
}
