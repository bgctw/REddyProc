## Test environments
* local Mint 18, R 3.4.3
* Travis ci: Ubuntu 14.04.5 LTS 
* win_builder: x86_64-w64-mingw32

## R CMD check results
Note on possibly wrong spelling in DESCRIPTION is not relevant
No warnings, nor errors

## Reply to CRAN comments
Removed acronyms and method details from DESCRIPTION

All tests and examples writing by default to tempdir().
Automatic downloads only if environment variable NOT_CRAN=true (some larger 
test-files of specific input formats)

Usage of \donttest{}: Since this is not valid R-code its
difficult to use with automatically generating Rd example sections from  body of 
R-functions by inlinedocs.
Here, examples use an equivalent function donttest() that execute a block 
of code in interactive sessions. (See ?donttest)

## Author in DESCRIPTION
I would like to have the following author field in DESCRIPTION in future submission:
Author: (Department for Biogeochemical Integration at MPI-BGC, Jena, Germany)
to avoid displaying the verbouse full Authors@R in the generated Rd files. 
However, this generates a note that prevents passing the CRAN pre-test, although
I explained this note in optional comment. What is the advice?

