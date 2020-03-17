## Notes
Dear CRAN maintainers,

We fixed an issue of relying on strptime returning NA for implausible doy 
(day-of-year).
This caused a warning in some architectures. We now explicitely check 
for leap-years and compare each doy exceeding 365 or 366 respectively.

Could you, please, publish the most recent version of the REddyProc package
on CRAN?

## Test environments
* local Mint 18, R 3.6.2
* Travis ci: Ubuntu 14.04.5 LTS 
* win_builder: x86_64-w64-mingw32 (64-bit)
* r_hub: x86_64-pc-linux-gnu (64-bit)

## R CMD check results
No warnings, nor errors.

1 Note:
Possibly mis-spelled words in DESCRIPTION:
  Wutzler (15:4)
  
Its spelled correctly:
Its an author name. Its my name.


