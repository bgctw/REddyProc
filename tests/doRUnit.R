#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ call testthat unit tests in directory inst\tests +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (require("testthat", quietly = TRUE)) {
	pkg <- "REddyProc"
	#library(pkg, character.only = TRUE)
	#test_package(pkg)
	test_check(pkg)
	if (testthat:::on_cran()) {
	  # delete the tmpdir()/REddyProdExamples to avoid note on detritus in the temp directory
	  # do not delete it outside cran, to avoid downloading several times
	  unlink(getREddyProcExampleDir(), recursive=TRUE)
	}
} else {
	warning("cannot run unit tests -- package testthat is not available")
}

