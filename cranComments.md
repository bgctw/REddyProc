## Test environments
* local Mint 18, R 3.4.3
* Travis ci: Ubuntu 14.04.5 LTS on 
* win_builder: x86_64-w64-mingw32

## R CMD check results
Note on possibly wrong spelling in DESCRIPTION is not relevant
No warnings, nor errors

## usage of remote and intermediate files
There are some tests and examples on reading data in csv or netCDF files that require (automated) downloading example files. By default these tests/examples are skipped and only executed if environment variable NOT_CRAN=true.

Other tests require processed data (e.g. by getFilledExampleDETha98Data()). This data is cached in a file stored in R-session temporary directory tempdir().
