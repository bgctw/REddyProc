## Notes
in reply to Kurt Hornik: 
These seem to have undeclared package dependencies in their unit test
code (R files in tests subdirs).

We moved the profiling code and associated dependencies 'rprof' 
out of the distributed code.

## Test environments
* local Mint 18, R 3.4.4
* Travis ci: Ubuntu 14.04.5 LTS 
* win_builder: x86_64-w64-mingw32 (64-bit), r74674

## R CMD check results
No warnings, nor errors.

Note on: Possibly mis-spelled words in DESCRIPTION
They are correctly spelle.


