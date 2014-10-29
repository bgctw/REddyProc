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
  ,Dir.s=''             ##<< Directory
) 
  ##author<<
  ## AMM
  # TEST: FileName.s <- 'Example_DETha98.txt'; Dir.s <- 'inst/examples'
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
  if( FALSE ) { #Do not always execute example code (e.g. on package installation)
    Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
    EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt',Dir.s)
  }
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Load NetCDF data
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fLoadFluxNCIntoDataframe <- function(
  ##title<<
  ## Load NetCDF file
  ##description<<
  ## Load specified variables and time stamp information from NetCDF file in Fluxnet BGI format.
  ## The time stamp information needs to be provided as variables 'year', 'month', 'day', 'hour'.
  VarList.V.s           ##<< Vector of variables to be read in 
  ,FileName.s           ##<< File name             
  ,Dir.s=''             ##<< Directory
  ,NcPackage.s='ncdf4'  ##<< Name of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')
) 
  ##author<<
  ## AMM, KS
  # TEST: FileName.s <- 'Example_DE-Tha.1996.1998.hourly.nc'; Dir.s <- 'inst/examples';
  # TEST: VarList.V.s <- c('NEE', 'Rg', 'rH', 'Tair', 'NEE_f'); NcPackage.s <- 'ncdf4'
{
  # Check for R NetCDF packages
  if( !(( NcPackage.s=='ncdf4' && suppressWarnings(require(ncdf4)) )
        || ( NcPackage.s=='RNetCDF' && suppressWarnings(require(RNetCDF)) )) )
    stop('fLoadFluxNCIntoDataframe::: Required package \'', NcPackage.s, '\' could not be loaded!')
  
  # Read in time variables
  Data.F <- fAddNCFVar(NULL, 'year', FileName.s, Dir.s, NcPackage.s, 'fLoadFluxNCIntoDataframe')
  Data.F <- fAddNCFVar(Data.F, 'month', FileName.s, Dir.s, NcPackage.s, 'fLoadFluxNCIntoDataframe')
  Data.F <- fAddNCFVar(Data.F, 'day', FileName.s, Dir.s, NcPackage.s, 'fLoadFluxNCIntoDataframe')
  Data.F <- fAddNCFVar(Data.F, 'hour', FileName.s, Dir.s, NcPackage.s, 'fLoadFluxNCIntoDataframe')
  
  # Convert time format to POSIX
  # !Attention: Use YMDH time format because julday and hour time stamps inconsistent at end of year
  Data.F <- fConvertTimeToPosix(Data.F, 'YMDH', Year.s = 'year', Month.s='month', Day.s = 'day', Hour.s = 'hour')
  
  # Read in variables from a given list of needed variables 
  for (i in 1: length(VarList.V.s))  {
    Data.F <- fAddNCFVar(Data.F, VarList.V.s[i], FileName.s, Dir.s, NcPackage.s, 'fLoadFluxNCIntoDataframe')
  }
  message('Loaded BGI Fluxnet NC file: ', FileName.s, ' with the following headers:')
  message('*** ', paste(colnames(Data.F), '(', as.character(lapply(Data.F, attr, which='units')), ')', collapse=' ', sep=''))
  
  Data.F 
  ##value<<
  ## Data frame with data from nc file.
}
attr(fLoadFluxNCIntoDataframe, 'ex') <- function() {
  if( FALSE ) { #Do not always execute example code (e.g. on package installation)
    Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
    EddyNCData.F <- fLoadFluxNCIntoDataframe(c('NEE', 'Rg', 'NEE_f'), 'Example_DE-Tha.1996.1998.hourly.nc', Dir.s)
  }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fAddNCFVar <- function(
  ##description<<
  ## Add variable from NetCDF file to data frame
  Data.F                ##<< Data frame
  ,Var.s                ##<< Variable name
  ,FileName.s           ##<< NetCDF file name
  ,Dir.s                ##<< Directory
  ,NcPackage.s          ##<< Name of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')
  ,CallFunction.s=''    ##<< Name of function called from
)
  ##author<<
  ## AMM, KS
  #TEST: Data.F <- NULL; Var.s <- 'NEE'; FileName.s <- 'Example_DE-Tha.1996.1998.hourly.nc'; Dir.s <- 'inst/examples'
  #TEST: NcPackage.s <- 'ncdf4'
{
  InputNCF.s <- fSetFile(FileName.s, Dir.s, T, 'fAddNCFVar')
  
  if( NcPackage.s=='RNetCDF' ) {
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
  } else if( NcPackage.s=='ncdf4' ) {
    NCFile.C <- nc_open(InputNCF.s, write=FALSE, readunlim=TRUE, verbose=FALSE)
    tryCatch({
      NewCol.F <- data.frame(ncvar_get(NCFile.C, Var.s))
      names(NewCol.F)[[1]] <- Var.s
      attr(NewCol.F[[1]], 'varnames') <- Var.s
      attr(NewCol.F[[1]], 'units') <- ncatt_get(NCFile.C, Var.s, 'units')$value
      Data.F <- data.frame(c(Data.F, NewCol.F))
    }, 
             finally = nc_close(NCFile.C)
    )
  } else {
    stop(CallFunction.s, ':::fAddNCFVar::: NC file ', InputNCF.s, ' could not be opened!')
  }
  
  Data.F
  ##value<<
  ## Data frame with new nc variable added.
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fLoadFluxNCInfo <- function(
  ##title<<
  ## Get site information from BGI NetCDF files
  ##description<<
  ## Load site information attributes such as latitude, longitude and others from BGI NetCDF files
  FileName.s            ##<< NetCDF file name
  ,Dir.s                ##<< Directory
  ,NcPackage.s='ncdf4'  ##<< Name of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')
  ,CallFunction.s=''    ##<< Name of function called from
)
  ##author<<
  ## AMM
  #TEST: FileName.s <- 'Example_DE-Tha.1996.1998.hourly.nc'; Dir.s <- 'inst/examples'
  #TEST: NcPackage.s <- 'ncdf4'
  #TEST: fLoadFluxNCInfo('Example_DE-Tha.1996.1998.hourly.nc','inst/examples','ncdf4')
{
  # Check for R NetCDF packages
  if( !(( NcPackage.s=='ncdf4' && suppressWarnings(require(ncdf4)) )
        || ( NcPackage.s=='RNetCDF' && suppressWarnings(require(RNetCDF)) )) )
    stop('fLoadFluxNCIntoDataframe::: Required package \'', NcPackage.s, '\' could not be loaded!')
  
  InputNCF.s <- fSetFile(FileName.s, Dir.s, T, 'fAddNCFVar')
  
  if( NcPackage.s=='RNetCDF' ) {
    NCFile.C <- open.nc(InputNCF.s)
    tryCatch({
      ##details<<
      ## Description of attribute list:
      ##describe<<
      SiteInfo.L <- list( 
        ID    = att.get.nc(NCFile.C,'NC_GLOBAL','Site_ID')                 ##<< SiteID
        ,DIMS = dim.inq.nc(NCFile.C,'time')$length                        ##<< Number of data rows
        ,LON  = as.numeric(att.get.nc(NCFile.C,'NC_GLOBAL','Longitude'))  ##<< Longitude
        ,LAT  = as.numeric(att.get.nc(NCFile.C,'NC_GLOBAL','Latitude'))   ##<< Latitude
        ,TZ   = as.numeric(att.get.nc(NCFile.C,'NC_GLOBAL','TimeZone'))   ##<< Time zone
        ,ELEV = as.numeric(att.get.nc(NCFile.C,'NC_GLOBAL','Elevation')) ##<< Elevation
        ,IGBP = att.get.nc(NCFile.C,'NC_GLOBAL','IGBP_class')            ##<< IGBP class
      )
    }, 
             finally = close.nc(NCFile.C)
    )
  } else if( NcPackage.s=='ncdf4' ) {
    NCFile.C <- nc_open(InputNCF.s, write=FALSE, readunlim=TRUE, verbose=FALSE)
    tryCatch({
      SiteInfo.L <- list( 
        ID    = ncatt_get(NCFile.C,0,'Site_ID')$value
        ,DIMS = NCFile.C$dim$time$len
        ,LON  = as.numeric(ncatt_get(NCFile.C,0,'Longitude')$value)
        ,LAT  = as.numeric(ncatt_get(NCFile.C,0,'Latitude')$value)
        ,TZ   = as.numeric(ncatt_get(NCFile.C,0,'TimeZone')$value)
        ,ELEV = as.numeric(ncatt_get(NCFile.C,0,'Elevation')$value)
        ,IGBP = ncatt_get(NCFile.C,0,'IGBP_class')$value
      )
    }, 
             finally = nc_close(NCFile.C)
    )
  } else {
    stop(CallFunction.s, ':::fLoadFluxNCInfo::: NC file ', InputNCF.s, ' could not be opened!')
  }
  
  SiteInfo.L
  ##value<<
  ## Attibute list
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Write data to file
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fWriteDataframeToFile <- function(
  ##description<<
  ## Write data frame to ASCII tab-separated text file
  Data.F                ##<< Data frame     
  ,FileName.s           ##<< File base name
  ,Dir.s=''             ##<< Directory
)
  ##author<<
  ## AMM, KS
  ##details<<
  ## Missing values are flagged as -9999.0
  # TEST: Data.F <- EddyData.F; FileName.s='none'; Dir.s <- 'inst/examples'; 
{
  # Set file name
  OutputFile.s <- fSetFile(FileName.s, Dir.s, F, 'fWriteDataframeToFile')
  
  # Convert NAs to gap flag
  Data.F <- fConvertNAsToGap(Data.F)
  
  # Write tab delimited file
  Lines.V.s <- vector(mode='character', length = 2)
  Lines.V.s[1] <- paste(colnames(Data.F), collapse='\t')
  Lines.V.s[1] <- gsub('DateTime', 'Date Time', Lines.V.s[1]) #If POSIX column replace name
  Lines.V.s[2] <- paste(as.character(lapply(Data.F, attr, which='units')), collapse='\t')
  Lines.V.s[2] <- gsub('NULL', '-', Lines.V.s[2])
  Lines.V.s[2] <- gsub('DateTime', 'Date Time', Lines.V.s[2])  #if POSIX column replace unit
  write(Lines.V.s, file=OutputFile.s, append=F)
  write.table(format(Data.F, digits=5, drop0trailing=T, trim=T), file=OutputFile.s, col.names=F, row.names=F, sep='\t', quote=F, append=T)
  message('Wrote tab separated textfile: ', OutputFile.s)
  
  ##value<<
  ## Output of data frame written to file of specified type.
}

attr(fWriteDataframeToFile, 'ex') <- function() {
  if( FALSE ) { #Do not always execute example code (e.g. on package installation)
    Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
    EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
    fWriteDataframeToFile(EddyData.F, 'OutputTest.txt', 'out')
  }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ File handling
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fInitFilesDir <- function(
  ##description<<
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

attr(fInitFilesDir, 'ex') <- function()   {
  if( FALSE ) { #Do not always execute example code (e.g. on package installation)
    Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
    fInitFilesDir(Dir.s, 'txt')
  }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fStripFileExtension <- function(
  ##description<<
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

attr(fStripFileExtension, 'ex') <- function()  {
  if( FALSE ) { #Do not always execute example code (e.g. on package installation)
    Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
    fStripFileExtension(fInitFilesDir(Dir.s, 'txt'))
  }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fSetFile <- function(
  ##description<<
  ## Set file name with path and check if directory and/or file exists
  FileName.s            ##<< File name
  ,Dir.s                ##<< Directory
  ,IO.b                 ##<< Input/output flag, TRUE for input, FALSE for output
  ,CallFunction.s=''    ##<< Name of function called from
)
  ##author<<
  ## AMM
  # TEST: Dir.s <- 'inst/examples'; FileName.s <- 'Example_DETha98.txt'; IO.b <- T; CallFunction.s <- 'test'
{
  # Check if string for directory provided
  Dir.b <- fCheckValString(Dir.s)
  
  # Check if directory exists
  if ( IO.b && Dir.b && (file.access(Dir.s, mode=4) != 0))
    stop(CallFunction.s, ':::fSetFile::: Directory does not exist: ', Dir.s)
  
  # Make directory if mode is output
  if(  !IO.b && Dir.b && (file.access(Dir.s, mode=0) != 0) ) {
    dir.create(Dir.s)
    message(CallFunction.s, ':::fSetFile::: Directory created: ', Dir.s)
    if( file.access(Dir.s, mode=2) != 0 )
      stop(CallFunction.s, ':::fSetFile::: Directory could not be created: ', Dir.s)
  }
  
  # Set file name accordingly
  File.s <- if( Dir.b ) {  paste(Dir.s, '/', FileName.s, sep='')
  } else { FileName.s }
  
  # If input file, check if file exists
  if ( IO.b && (file.access(File.s, mode=4) != 0) )
    stop(CallFunction.s, ':::fSetFile::: File does not exist or has no read permission: ', File.s)
  
  File.s
  ##value<< 
  ## Returns name of file with complete path.
}
