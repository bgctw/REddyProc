#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ call testthat unit tests in directory inst\tests +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(require("testthat", quietly=TRUE)) {
	library(REddyProc)	# need to call library to make lazyData objects available
	test_package("REddyProc")
} else {
	warning("cannot run unit tests -- package testthat is not available")
}