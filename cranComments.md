## Test environments
* local Mint 18, R 3.4.3
* R-devel-san docker image
* Travis ci: Ubuntu 14.04.5 LTS 
* win_builder: x86_64-w64-mingw32

## R CMD check results
Note on possibly mis-spelled words: they are correct.
No further notes, warnings, nor errors.

## Reply to CRAN comments
In following up an email from Prf. Ripley:
"several serious 'Additional issues', 
your package leaves behind a directory /tmp/REddyProcExamples"

Regarding the directory:
I identified the causing example code and removed it.

Regarding 'Additional issues':
I fixed a bug in C function whichValueGreaterEqualC that was reading post the 
upper index of an array but did not use the value. 
This caused the tests to run well but the C-checkers to report
additional issues.
I now tested the package additionally on using an R binary compiled with 
Address Sanitizer in the r-devel-san docker image and did not get any
warnings. 
When running with option "--use-valgrind" I got errors telling: 
"All heap blocks were freed -- no leaks are possible ...
ERROR SUMMARY: 0 errors from 0 contexts (suppressed: 0 from 0)"
I hope that resolves the 'Additional issues'.
Please, be patient with me if I am not there yet. I am learning.

Long running vignette:
In addition, the vignette useCase.Rmd was adapted intending to not
execute R code outside devtools::check() or devtools::build(). In two of the CRAN logs
it failed building with message "killed". I hope this works now:
https://stackoverflow.com/questions/28961431/computationally-heavy-r-vignettes


