## Notes
Dear CRAN maintainers,

following up the email from Prof. Ripley 3/6/2022, we converted the strong
dependency on package mlegp, which might be archived from CRAN, to a weak 
dependency (SUGGESTS). 
The parts that use mlegp now use "requireNamespace" and in case display
an error requesting users to install a version of mlegp by hand.

Could you, please, have a look at the current version and tell us if
this weak dependency is ok with CRAN.

Please, do not publish this version on CRAN yet. We still hope that 
mlegp gets fixed.
Otherwise we will submit this version again on Fri 18th of March, i.e.
shortly before REddyProc would be archived due to strong dependendy on mlegp.

## Test environments
* local Linux-Mint, R 4.1.2
* Github actions, current Ubuntu
* current win_builder
* r_hub: all default platforms

## R CMD check results
No warnings, nor errors.

1 Note:
REddyProcExamples in the inst folder is on purpose and not detritus

