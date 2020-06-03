# cutpointr 1.0.2

## R CMD check results

We received one ERROR on Rhub Windows Server 2008 R2 SP1, R-devel, 32/64 bit, because the package could not be installed. We did not receive any ERRORs, WARNINGs, or NOTEs on other platforms or services. 

### Local
* R version 3.6.3 (2020-02-29) Platform: x86_64-w64-mingw32/x64 (64-bit) Running under: Windows 10 x64 (build 18363): No WARNINGs, ERRORS or NOTEs

### R-Hub
* Debian Linux, R-devel, GCC ASAN/UBSAN: No ERRORs, WARNINGs or NOTEs
* Fedora Linux, R-devel, clang, gfortran: No ERRORs, WARNINGs or NOTEs
* Ubuntu Linux 16.04 LTS, R-release, GCC: No ERRORs, WARNINGs or NOTEs
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit: 1 ERROR: 

Error in inDL(x, as.logical(local), as.logical(now), ...) : 
  unable to load shared object 'C:/Users/USEReDToiYufWA/AppData/Local/Temp/RtmpqONAh9/working_dir/RtmpcbXrhk/RLIBS_146c3b9f34fc/Rcpp/libs/x64/Rcpp.dll':
  LoadLibrary failure:  The specified procedure could not be found.

### Win Builder
* using R Under development (unstable) (2020-05-29 r78617) using platform: x86_64-w64-mingw32 (64-bit): No ERRORs, WARNINGs or NOTEs

### AppVeyor
* R version 4.0.0 Patched (2020-05-25 r78571) Platform: x86_64-w64-mingw32/x64 (64-bit) Running under: Windows Server 2012 R2 x64 (build 9600): No ERRORs, WARNINGs or NOTEs

### Travis CI
* R version 4.0.0 (2020-04-24) Platform: x86_64-pc-linux-gnu (64-bit) Running under: Ubuntu 16.04.6 LTS: No ERRORs, WARNINGs or NOTEs