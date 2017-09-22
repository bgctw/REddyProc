# TODO: instead of distribution large example data, let it be installed on demand from a server
# 
# Author: twutz
###############################################################################

.tmp.f <- function(){
existsExampleFile <- function(
	### checks if given example filename is existing and if not tries to download it.
	filename = "Example_DETha98.txt"
	,dir = paste(system.file(package='REddyProc'), 'examples2', sep='/')
	,remoteDir = "https://github.com/bgctw/REddyProc/raw/master/inst/examples"
	,isTryDownload = TRUE
){
	fullname <- file.path(dir, filename)
	if( file.exists(fullname) ) return(fullname)
	if( isTRUE(isTryDownload) ){
		if( !dir.exists(dir) ) dir.create(dir)
		url <- file.path(remoteDir, filename)
		retCode <- download.file(url, fullname)
		if( retCode==0) return(fullname)
	}
	##value<< the full path name to the example data or if not available an zero-length character.
	## Allows to check for if( length(existsExampleFile()) ) ...
	return( character(0) )	
}


} #.tmp.f


