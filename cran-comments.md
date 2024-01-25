## Notes
Dear CRAN maintainers,

following your email that noted empty documentation for some of the function 
arguments, we submit a new version of the REddyProc package.

In addition to fixing the empty comments, we include a new features:
- exporting the results in an additional format requested by a users
- an experimental implementation of the gap-filling procedure by Vekuri et. al 2023

## Test environments
* local Linux-Mint, R 4.3.2
* Github actions, current Ubuntu
* (win_builder was not reachable by curl from devtools::check_win_devel())
* r_hub: all default platforms

## R CMD check results
No warnings, nor errors.

1 Note:
"checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    ‘REddyProcExamples’"
REddyProcExamples is there on purpose: It is created on a first call to
getExamplePath(exampleId) and then reused by several tests.

