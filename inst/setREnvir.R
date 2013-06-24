#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Developers' R script to (re-)set user specific R environment +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: AMM

# Remove all objects in R environment (but Develop.b and Testing.b)
ls.b <- ls(all=TRUE) %in% c('Develop.b', 'LongTest.b')
rm(list = ls(all=TRUE)[!ls.b])
if( sum(grepl('REddyProc', (.packages()))) == 1 )
  detach('package:REddyProc')
message('Start with empty R environment with REddyProc detached.')

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Set user specific R working directory
# AMM
if ( Sys.getenv('HOME') == '/Users/amoffat' ) { 
  setwd('/Users/amoffat/Projects/REddy/REddyProc')
  DirFluxnet.s <- paste('/Volumes/BGI/data/DataStructureMDI/DATA/site/Fluxnet/halfhourly/level5_new_v2/Data/single_sites/')
# KS
} else if ( Sys.getenv('HOME') == '/Users/ksickel' ) { 
  setwd('/Users/ksickel/REddyProc')
  DirFluxnet.s <- paste('/Volumes/BGI/data/DataStructureMDI/DATA/site/Fluxnet/halfhourly/level5_new_v2/Data/single_sites/')
# Unknown
} else {
  stop('R working directory could not be set, unkown home directory.')
}
message('Working directory set to:', getwd())

