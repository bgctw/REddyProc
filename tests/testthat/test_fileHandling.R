#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Unit tests for fConvertTimeToPosix functions +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: TW
#require(testthat)
context("fileHandling")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test_that_ <- function(...){}	# for uncommenting tests

test_that_("fLoadFluxNCIntoDataframe",{
			Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
			for( ncPack in c("ncdf4","RNetCDF")){
				if( tmp <- requireNamespace(ncPack, quietly=TRUE) ){
					ds <- fLoadFluxNCIntoDataframe(c('NEE', 'Rg', 'NEE_f')
							, getExamplePath('Example_DE-Tha.1996.1998.hourly_selVars.nc')
							, NcPackage.s=ncPack
					)
					colNames <- c("DateTime", "year", "month", "day", "hour", "NEE", "Rg", "NEE_f")
					expect_true( nrow(ds) > 2000L )
					expect_true( all(colNames %in% names(ds)) )
					expect_true( inherits(ds$DateTime,"POSIXct") )
					colName <- colNames[2]
					for( colName in colNames[-1] ){
						expect_true( inherits(ds[[colName]], "numeric"))
					}
				} #if
			} # for
		})

