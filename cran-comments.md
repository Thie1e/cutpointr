## Test environments
* local Ubuntu 16, R 3.4.4
* win-builder (R Under development (unstable) (2018-03-18 r74422))
* ubuntu 14.04 (on travis-ci), R 3.4.4
* Appveyor R version 3.4.4 Patched (2018-03-17 r74422) Platform: i386-w64-mingw32/i386 (32-bit) Running under: Windows Server 2012 R2 x64 (build 9600)

## R CMD check results
There were no ERRORs or WARNINGs. One NOTE was generated because I'm a new maintainer.
I tried to reproduce the failed test that occured on CRAN's Linux system with 
the previous version of cutpointr (0.7.1). I could unfortunately not reproduce the
error on my Ubuntu system, but I rewrote the test so that it hopefully passes now.

## devtools::revdep_check()
No ERRORs or WARNINGs found
