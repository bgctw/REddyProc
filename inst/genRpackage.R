#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Generation of code documentation with 'inlinedocs' +++
#+++ Parse R5 methods to functions +++
#+++ Run from package directory
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM

Develop.b <- F #True if in development mode

# Source settings for R environment
source('inst/setREnvir.R')

#Source file and data handling scripts
source("R/DataFunctions.R")
source("R/FileHandling.R")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Parse R5 methods to generate documentation with inlinedocs
CodeIn.V.s <- fInitFilesDir('R','Eddy')
CodeOut.V.s <- gsub('Eddy','Dummy', CodeIn.V.s)
# File.i <- 1
for (File.i in 1:length(CodeIn.V.s)) {
  #Read in script
  ScriptIn.s <- fSetFile(CodeIn.V.s[File.i], 'R', IO.b=T, 'genRpackage')
  #Parse reference class methods to normal function code
  Code.s <- readLines(ScriptIn.s)
  Code.s <- gsub('sEddyProc\\$methods\\(','', Code.s)
  Code.s <- gsub('initialize =','sEddyProc.new =', Code.s)
  Code.s <- gsub('\\}\\)','}',Code.s)
  Code.s <- gsub('<<-','<-',Code.s)
  # Delete definition of R5 class - need to complete setRefClass with a line starting with "))" and not use it before
  while( length(lineStart <- grep("\\ssetRefClass", Code.s)) ){
	  lineEnd <- grep("^\\)\\)", Code.s[-(1:lineStart)])
	  Code.s <- Code.s[-(lineStart:(lineStart+lineEnd))]
  }
  # Add new header
  Code.s <- c('#+++ !!! Auxiliary file only, generated for documentating methods of R5 class sEddyProc with inlinedocs !!!+++ \n', Code.s)
  # Write to file
  ScriptOut.s <- fSetFile(CodeOut.V.s[File.i], 'R', IO.b=F, 'genRpackage')
  writeLines(Code.s, con=ScriptOut.s)
  # For testing during development, sEddyProc methods can be loaded as standard functions
  if( Develop.b==T && File.i != 1) eval(parse(text=Code.s)) 
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Generate new inlinedocs documentation
require(inlinedocs)
system('rm -f man/*')		# twutz: depends on rm, maybe base on R unlink instead

# temporarily move R5 classes to avoid generation of nun-useful Docu
dir.create("tmp", showWarnings = FALSE)
#if( !all(file.rename(paste("R",CodeIn.V.s,sep="/"), rep("tmp",length(CodeIn.V.s)) )) )
if( !all(file.rename(paste("R",CodeIn.V.s,sep="/"), paste("tmp",CodeIn.V.s,sep="/") )) )
		stop("could not move Eddy files to tmp")

package.skeleton.dx('.') 	# produces *.Rd files in ./man directory

# Remove files generated only for the code documentation, and move original files back
system('rm -f R/Dummy*')
#file.rename(paste("tmp",CodeIn.V.s,sep="/"), rep("R",length(CodeIn.V.s)) )
file.rename(paste("tmp",CodeIn.V.s,sep="/"), paste("R",CodeIn.V.s,sep="/") )

# overwrite generated REddyProc-package.Rd by version from inst/genData
file.copy("inst/genData/REddyProc-package.Rd","man", overwrite = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# genRd(execInlinedocs = FALSE)		# using package twDev from twutz

# Install, build and reload package
if ( Sys.getenv('HOME') == "/Users/amoffat" ) { #AMM's local setup for generating the package
  Dir.s <- getwd()
  
  # Reinstall package, set for local libraries of AMM with build binary in gzp-file
  # Without library path, it gets installed to library ‘/Applications/RStudio.app/Contents/Resources/R/library’!
  # Check library paths with .libPaths()
  # For RStudio specific information see here http://www.rstudio.com/ide/docs/server/configuration
  system('R CMD INSTALL --build --html --library=/Library/Frameworks/R.framework/Versions/current/Resources/library ../REddyProc')
  
  if (FALSE) {
    # Windows compatible zip-file (for users withour R tools)
    setwd('/Library/Frameworks/R.framework/Versions/current/Resources/library')
    system('zip -rq REddyProc_0.31.zip REddyProc')
    system(paste('mv REddyProc_0.31.zip ', Dir.s, '/.', sep=''))
    #Reset working directory
    setwd(Dir.s)
  
  
    # Make zip file with code (and code structure)
    system('rm -fr REddyProc_0.31_Code')
    system('mkdir REddyProc_0.31_Code')
    system('mkdir REddyProc_0.31_Code/REddyProc')
    system('cp -r data REddyProc_0.31_Code/REddyProc')
    system('cp DESCRIPTION REddyProc_0.31_Code/REddyProc')
    system('cp -r inst REddyProc_0.31_Code/REddyProc')
    system('cp -r man REddyProc_0.31_Code/REddyProc')
    system('cp NAMESPACE REddyProc_0.31_Code/REddyProc')
    system('cp -r R REddyProc_0.31_Code/REddyProc')
    system('cp -r tests REddyProc_0.31_Code/REddyProc')
    system('zip -rq REddyProc_0.31_Code.zip REddyProc_0.31_Code/REddyProc')
    
    message('Generated REddyProc package archives.')
  }
  
  if (FALSE) { #Test installing package from archive files
    # Install package from gzp or zip file
    install.packages('REddyProc_0.31.gzp', repos=NULL)
    install.packages('REddyProc_0.31.zip', repos=NULL)
  }
  
  #Only load if REddyProc was already package loaded (sometimes not wanted during testing)
  if( sum(grepl("REddyProc", (.packages()))) == 1 ) {
    detach("package:REddyProc")
    require(REddyProc)
    # Dir.s <- system.file(package='REddyProc') # Path to package
  }
}
