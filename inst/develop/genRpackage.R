#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Developers' R script +++
#+++ Generation of code documentation with 'inlinedocs' (by parsing R5 methods to functions)
#+++ Generation of example data, original scripts, package archive
#+++ --> run from package directory
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM

Develop.b <- F #True if in development mode

# Source settings for R environment
source('inst/develop/setREnvir.R')

#Source file and data handling scripts
source('R/DataFunctions.R')
source('R/FileHandling.R')

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Parse R5 methods to generate documentation with inlinedocs
CodeIn.V.s <- fInitFilesDir('R','Eddy.*\\.R$')		# omit dir name REddyProc
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
  # Delete definition of R5 class - need to complete setRefClass with a line starting with '))' and not use it before
  while( length(lineStart <- grep('\\ssetRefClass', Code.s)) ){
	  lineEnd <- grep('^\\)\\)', Code.s[-(1:lineStart)])
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
system('rm -f man/*')		# Comment TW: depends on rm, maybe base on R unlink instead

# temporarily move R5 classes to avoid generation of nun-useful Docu
dir.create('tmp', showWarnings = FALSE)
#if( !all(file.rename(paste('R',CodeIn.V.s,sep='/'), rep('tmp',length(CodeIn.V.s)) )) )
if( !all(file.rename(paste('R',CodeIn.V.s,sep='/'), paste('tmp',CodeIn.V.s,sep='/') )) )
		stop('could not move Eddy files to tmp')

package.skeleton.dx('.') 	# produces *.Rd files in ./man directory

# Remove files generated only for the code documentation, and move original files back
system('rm -f R/Dummy*')
file.rename(paste('tmp',CodeIn.V.s,sep='/'), paste('R',CodeIn.V.s,sep='/') )

# Overwrite generated documentation by (self-written) version from inst/develop/genData
DocuIn.V.s <- fInitFilesDir('inst/develop/genDocu/','*.Rd')
file.copy(paste('inst/develop/genDocu',DocuIn.V.s,sep='/'),'man', overwrite=T)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Provide (update) example data for package from txt file
EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt','inst/examples')
save('EddyData.F', file='data/Example_DETha98.Rdata')

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Provide source code as R script files in inst/scripts with package
RFiles.V.s <- fInitFilesDir('R','.R')
system('rm -f inst/scripts/*')
file.copy(paste('R', RFiles.V.s, sep='/'), paste('inst/scripts', RFiles.V.s, sep='/'), overwrite=T)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# If package twDev from TW is used for documenation generation
###   genRd(execInlinedocs = FALSE)

# Install, build and reload package
if( Sys.getenv('HOME') == '/Users/amoffat' ) { #AMM's local setup for generating the package
  Dir.s <- getwd()
  
  # Reinstall package, set for local libraries of AMM with built binary in gzp-file
  # Without library path, it gets installed to library ‘/Applications/RStudio.app/Contents/Resources/R/library’!
  # Check library paths with .libPaths()
  # For RStudio specific information see here http://www.rstudio.com/ide/docs/server/configuration
  system('R CMD INSTALL --build --html --library=/Library/Frameworks/R.framework/Versions/current/Resources/library ../REddyProc')
  
  # Windows compatible zip-archive (use Rtools for building packages for R under Windows)
  setwd('/Library/Frameworks/R.framework/Versions/current/Resources/library')
  system('zip -rq REddyProc_0.5-1.zip REddyProc')
  system(paste('mv REddyProc_0.5-1.zip ', Dir.s, '/.', sep=''))
  #Reset working directory
  setwd(Dir.s)
  
  # To update to newest version on cluster:
  #   1. Update repository
  #   2. On pc026: R CMD INSTALL --build --html REddyProc
  #   3. Email TW since REddyProcWeb is based on this
  
  if( Develop.b ) {
    #Check package
    system('R CMD CHECK ../REddyProc')
  }

  # Restart of R console required to load new version of package
}
