#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ call testthat unit tests in directory inst\tests +++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(require("testthat", quietly=TRUE)) {
	test_package("REddyProc")
} else {
	warning("cannot run unit tests -- package testthat is not available")
}