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
* Travis ci: Ubuntu 16.04.6 LTS
* win_builder: x86_64-w64-mingw32 (64-bit)
* r_hub: Debian Linux, R-devel, GCC (debian-gcc-devel)

## R CMD check results
No warnings, nor errors.

1 Note:
on maintainer 

