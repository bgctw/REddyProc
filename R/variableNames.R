renameVariablesInDataframe <- function(
		### Rename the column names of a data.frame according to a given mapping
		data.F 		##<< data.frame whose columns should be renamped
		,variableNameMapping = getLathuilleToBerkeleyVariableNameMapping()	##<< named character vector: 
			##<< specifying a renaming (name -> value)  
			##<< of the variables, see e.g. \code{\link{getBerkeleyVariableNameMapping}}
)
{
	if( length(names(variableNameMapping)) ){
		iTarget <- match( colnames(data.F), names(variableNameMapping)  )
		iMatch <- which(!is.na(iTarget))
		colnames(data.F)[iMatch] <- variableNameMapping[ iTarget[iMatch] ]  
	}
	data.F
}

getLathuilleToBerkeleyVariableNameMapping <- function(
		### Get a mapping of variable names of REddyProc defaults to names of the Berkeley 2016 release of the Fluxnet data		
		map = character()	##<< named character vector: additional mapping, that extends or overwrites defaults in \code{mapDefault} 
		,mapDefault = c(	##<< named character vector: default mapping
				Year="YEAR"
				,DoY='DOY'
				,Rg='SW_IN'
				,Tair='TA'
				,Tsoil='TS'
				,rH='RH'
				,VPD='VPD'
				,Ustar='USTAR'
				,NEE_orig='NEE_PI'
				,H_orig='H_PI'
				,LE_orig='LE_PI'
				,NEE_f='NEE_F'
				,H_f='H_F'
				,LE_f='LE_F'
				,NEE_fqc='NEE_QC'
				,H_fqc='H_QC'
				,LE_fqc='LE_QC'
		)
){
	##seealso<< \code{\link{renameVariablesInDataframe}}
	mapDefault[ names(map) ] <- map
	mapDefault
}
attr(getLathuilleToBerkeleyVariableNameMapping,"ex") <- function(){
	# adding mapping of foo, and overwriting mapping of DoY
	getLathuilleToBerkeleyVariableNameMapping(c(foo="FOO",DoY="doy"))
}


getBerkeleyToLathuilleVariableNameMapping <- function(
		### Get a mapping of variable names of the Berkeley 2016 release of the Fluxnet to of REddyProc defaults to names 		
		map = character()	##<< named character vector: additional mapping, that extends or overwrites defaults in \code{mapDefault} 
		,mapDefault = c(	##<< named character vector: default mapping
			YEAR='Year'
			,DOY='DoY'
			,NEE='NEE'
			,LE='LE'
			,H='H'
			,SW_IN='Rg'
			,TA='Tair'
			,TS='Tsoil'
			,RH='rH'
			,VPD='VPD'
			,USTAR='Ustar'
			,NEE_PI='NEE_orig'
			,H_PI='H_orig'
			,LE_PI='LE_orig'
			,NEE_F='NEE_f'
			,H_F='H_f'
			,LE_F='LE_f'
			,NEE_QC='NEE_fqc'
			,H_QC='H_fqc'
			,LE_QC='LE_fqc'
		)
){
	##seealso<< \code{\link{renameVariablesInDataframe}}
	mapDefault[ names(map) ] <- map
	mapDefault
}

createBerkeleyJulianDate <- function(
		### convert POSIXct to JulianDate format used in Berkeley release
		sDateTime
){
	##details<< 
	## In the Berkeley-Release of the fluxnet data, the time is stored as an integer
	## with base10-digits representing YYYYMMddhhmm
	recover()
	strftime(bla.new, format= "%Y-%m-%d %H:%M:%S")
}

.tmp.f <- function(){
	bla.new <- strptime(as.character(new_ds$TIMESTAMP_START),"%Y%m%d%H%M")
	zzz<-strftime(bla.new, format= "%Y-%m-%d %H:%M:%S")
	new_ds$COMMTIME<-zzz
	new_ds_sub_9607<-new_ds[c(which(new_ds$COMMTIME == "1996-01-01 00:00:00"):
							which(new_ds$COMMTIME == "2013-12-31 23:30:00")),]
	
}

