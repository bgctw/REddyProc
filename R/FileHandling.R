#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with functions for data input and output
#+++ Dependencies: DataFunctions.R
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Load ascii format data
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fLoadTXTIntoDataframe <- function(
  ##title<<
  ## Load text file with one header and one unit row into data frame
  ##description<<
  ## If gaps with the flag -9999.0 exist, these are set to NA.
  FileName.s            ##<< File name
  ,Dir.s                ##<< Directory
  ) 
  ##author<<
  ## AMM
  # TEST: FileName.s <- 'Example_DETha98.txt'; Dir.s <- 'data'
{
  InputFile.s <- fSetFile(FileName.s, Dir.s, T, 'fLoadTXTIntoDataframe')  

  # Read in header
  Header.V.s <- as.character(read.csv(InputFile.s, header=F, sep='', dec='.', nrows=1, stringsAsFactors=F))
  Units.V.s <- as.character(read.csv(InputFile.s, header=F, sep='', dec='.', skip=1, nrows=1, stringsAsFactors=F))
  if( length(Header.V.s) != length(Units.V.s) )
    stop('fLoadTXTIntoDataframe::: Entries in header row and unit row are not the same length: \n', 
         length(Header.V.s), ': ', paste(Header.V.s, collapse=' '), '\n', 
         length(Units.V.s), ': ', paste(Units.V.s, collapse=' '))
  # Skip unit row and read in data
  Data.F <- read.csv(InputFile.s, header=F, skip=2, sep='', dec='.')
  # Rename columns with header information
  names(Data.F) <- Header.V.s
  # Add units (and also varnames) as attribute
  for (Var.i in 1:length(Header.V.s)) {
    attr(Data.F[,Header.V.s[Var.i]], 'varnames') <- Header.V.s[Var.i]
    attr(Data.F[,Header.V.s[Var.i]], 'units') <- Units.V.s[Var.i]
  } 
  message('Loaded file ', FileName.s, ' with the following variables (units):')
  message('*** ', paste(colnames(Data.F), '(', as.character(lapply(Data.F, attr, which='units')), ')', collapse=' ', sep=''))
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
  ,Dir.s=''             ##<< Directory      
  ) 
  ##author<<
  ## AMM, KS
  # TEST: FileName.s <- 'Example_DE-Tha.1996.1998.hourly.nc'; Dir.s <- 'inst/MDSdata'; VarList.V.s <- c('NEE', 'Rg', 'rH', 'Tair', 'NEE_f')
{
  # Read in time variables
  Data.F <- fAddNCFVar(NULL, 'year', FileName.s, Dir.s, 'fLoadFluxNCIntoDataframe')
  Data.F <- fAddNCFVar(Data.F, 'month', FileName.s, Dir.s, 'fLoadFluxNCIntoDataframe')
  Data.F <- fAddNCFVar(Data.F, 'day', FileName.s, Dir.s, 'fLoadFluxNCIntoDataframe')
  Data.F <- fAddNCFVar(Data.F, 'hour', FileName.s, Dir.s, 'fLoadFluxNCIntoDataframe')
  
  # Convert time format to POSIX
  Data.F <- fConvertTimeToPosix(Data.F, 'YMDH', Year.s = 'year', Month.s='month', Day.s = 'day', Hour.s = 'hour')
  
  # Read in variables from a given list of needed variables 
  for (i in 1: length(VarList.V.s))  {
    Data.F <- fAddNCFVar(Data.F, VarList.V.s[i], FileName.s, Dir.s, 'fLoadFluxNCIntoDataframe')
  }
  message('Loaded BGI Fluxnet NC file: ', FileName.s, ' with the following headers:')
  message('*** ', paste(colnames(Data.F), '(', as.character(lapply(Data.F, attr, which='units')), ')', collapse=' ', sep=''))
  
  Data.F 
  ##value<<
  ## Data frame with data from nc file.
}
attr(fLoadFluxNCIntoDataframe, 'ex') <- function() {
  # Example code
  if( file.exists('inst/MDSdata/Example_DE-Tha.1996.1998.hourly.nc') )
    EddyNCData.F <- fLoadFluxNCIntoDataframe(c('NEE', 'Rg', 'NEE_f'), 'Example_DE-Tha.1996.1998.hourly.nc', 'inst/MDSdata')
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fAddNCFVar <- function(
  ##title<<
  ## Add variable from NetCDF file to data frame
  Data.F                ##<< Data frame
  ,Var.s                ##<< Variable name
  ,FileName.s           ##<< NetCDF file name
  ,Dir.s                ##<< Directory
  ,CallFunction.s=''    ##<< Name of function called from
  )
  ##author<<
  ## AMM, KS
  # TEST: Data.F <- NULL; Var.s <- 'NEE'; FileName.s <- 'Example_DE-Tha.1996.1998.hourly.nc'; Dir.s <- 'inst/MDSdata'
{
  InputNCF.s <- fSetFile(FileName.s, Dir.s, T, 'fAddNCFVar')
  
  RNetCDF.b <- suppressWarnings(require(RNetCDF))
  ncdf.b    <- suppressWarnings(require(ncdf))
  
  if ( !RNetCDF.b && !ncdf.b )
      stop(CallFunction.s, ':::fAddNCFVar::: Required package RNetCDF or ncdf could not be loaded!') # for handling BGI Fluxnet netcdf files
  
  if( RNetCDF.b ) {
    NCFile.C <- open.nc(InputNCF.s)
    tryCatch({
      NewCol.F <- data.frame(var.get.nc(NCFile.C, Var.s))
      names(NewCol.F)[[1]] <- Var.s
      attr(NewCol.F[[1]], 'varnames') <- Var.s
      attr(NewCol.F[[1]], 'units') <- att.get.nc(NCFile.C, Var.s, 'units')
      
      # Use c() instead of cbind() to be able to bind dataframe Data.F even if empty
      Data.F <- data.frame(c(Data.F, NewCol.F))
      #attr(Data.F[[1]], 'units')
    }, 
             finally = close.nc(NCFile.C)
    )
  } else if( ncdf.b ) {
    stop('not implemented')
    NCFile.C <- open.ncdf(InputNCF.s)
    tryCatch({
      NewCol.F <- data.frame(get.var.ncdf(NCFile.C, Var.s))
      names(NewCol.F)[[1]] <- Var.s
      attr(NewCol.F[[1]], 'varnames') <- Var.s
      attr(NewCol.F[[1]], 'units') <- att.get.ncdf(NCFile.C, Var.s, 'units')
      
      # Use c() instead of cbind() to be able to bind dataframe Data.F even if empty
      Data.F <- data.frame(c(Data.F, NewCol.F))
      #attr(Data.F[[1]], 'units')
    }, 
             finally = close.ncdf(NCFile.C)
    )
  } else {
    stop(CallFunction.s, ':::fAddNCFVar::: NC files could not be opened!')
  }
  
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
  ,FileName.s           ##<< File base name
  ,Dir.s=''             ##<< Directory
  ,FileType.s='txt'     ##<< File output type
)
  ##author<<
  ## AMM, KS
  ##details<<
  ## 'txt' - for tab delimited text file with header and unit row
  ## 'nc' - for very simple NetCDF file of numeric columns
  ## With missing values flagged as -9999.0
  # !!!TODO: NC file output: add non-numeric columns and units
  # TEST: Data.F <- EddyData.F; BaseName.s <- 'OutputTest'; FileType.s='nc'; Dir.s <- 'data'; OutputName.s='none';
{
  # Set file name
  OutputFile.s <- fSetFile(FileName.s, Dir.s, F, 'fWriteDataframeToFile')
  
  # Convert NAs to gap flag
  Data.F <- fConvertNAsToGap(Data.F)
  
  # Write data to files
  if( FileType.s=='txt') {
    # Write tab delimited file
    # supressWarnings()
    Lines.V.s <- vector(mode='character', length = 2)
    Lines.V.s[1] <- paste(colnames(Data.F), collapse='\t')
    Lines.V.s[2] <- paste(as.character(lapply(Data.F, attr, which='units')), collapse='\t')
    Lines.V.s[2] <- gsub('NULL', '--', Lines.V.s[2])
    write(Lines.V.s, file=OutputFile.s, append=F)
    write.table(format(Data.F, digits=5, drop0trailing=T, trim=T), file=OutputFile.s, col.names=F, row.names=F, sep='\t', quote=F, append=T)
    message('Wrote tab separated textfile: ', OutputFile.s)
    
  } else if( FileType.s=='nc') {
    # Write NetCDF file
    RNetCDF.b <- suppressWarnings(require(RNetCDF))
    ncdf.b    <- suppressWarnings(require(ncdf))
    
    if ( !RNetCDF.b && !ncdf.b )
      stop(CallFunction.s, ':::fWriteDataframeToFile::: Required package RNetCDF or ncdf could not be loaded!') # for handling BGI Fluxnet netcdf files
    
    if( RNetCDF.b ) {
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
          else next; #! Skips non-numeric columsn for now 
        }
      }, 
               finally = close.nc(NCFile.C)
      )
    } else if( ncdf.b ) {
      stop('!!! Error: not yet implemented !!!')
      tryCatch({
        NULL
      },
               finally = NULL
      )
    } else {
      stop(CallFunction.s, ':::fWriteDataframeToFile::: NC files could not be opened!')
    }
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
