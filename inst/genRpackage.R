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
for (File.i in 1:length(CodeIn.V.s)) {
  #Read in script
  ScriptIn.s <- fSetFile(CodeIn.V.s[File.i], 'R', IO.b=T, 'genRpackage')
  #Parse reference class methods to normal function code
  Code.s <- readLines(ScriptIn.s)
  Code.s <- gsub('sEddyProc\\$methods\\(','', Code.s)
  Code.s <- gsub('initialize =','sEddyProc.new =', Code.s)
  Code.s <- gsub('\\}\\)','}',Code.s)
  Code.s <- gsub('<<-','<-',Code.s)
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
system('rm -f man/*')
package.skeleton.dx('.') # produces *.Rd files in ./man directory

# Remove files generated only for the code documentation
system('rm -f R/Dummy*')

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Install, build and reload package
if ( Sys.getenv('HOME') == "/Users/amoffat" ) { #AMM's local setup for generating the package
  Dir.s <- getwd()
  
  # Reinstall package, set for local libraries of AMM with build binary in gzp-file
  # Without library path, it gets installed to library ‘/Applications/RStudio.app/Contents/Resources/R/library’!
  system('R CMD INSTALL --build --html --library=/Library/Frameworks/R.framework/Versions/2.13/Resources/library ../REddyProc')
  
  # Windows compatible zip-file (for users withour R tools)
  setwd('/Library/Frameworks/R.framework/Versions/2.13/Resources/library')
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
  
  if (FALSE) { #Test installing package from archive files
    # Install package from gzp or zip file
    install.packages('REddyProc_0.31.gzp', repos=NULL)
    install.packages('REddyProc_0.2.zip', repos=NULL)
  }
  
  #Only load if REddyProc was already package loaded (sometimes not wanted during testing)
  if( sum(grepl("REddyProc", (.packages()))) == 1 ) {
    detach("package:REddyProc")
    require(REddyProc)
    # Dir.s <- system.file(package='REddyProc') # Path to package
  }
}
