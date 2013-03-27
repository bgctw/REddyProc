## unit tests will not be done if RUnit is not available
if(require("testthat", quietly=TRUE)) {
	#pkg <- "PKG" # <-- Change to package name!
	pkg <- "REddyProc" # <-- Change to package name!
	test_package(pkg)
} else {
	warning("cannot run unit tests -- package testthat is not available")
}