# cutpointr 1.1.0

## R CMD check results

There were no NOTEs, WARNINGs or ERRORS apart from the NOTE on Win Builder Devel that there were misspelled words in the Description, which are two names of authors.

### Local
* R version 4.0.3, Platform: x86_64-w64-mingw32/x64 (64-bit) Running under: Windows 10 x64 (build 18363): no NOTEs, WARNINGs, or ERRORs.

### Win Builder
* R Under development (unstable) (2020-12-16 r79643), using platform: x86_64-w64-mingw32 (64-bit): NOTE: Possibly mis-spelled words in DESCRIPTION (which are the author names). No WARNINGs or ERRORs.

### AppVeyor
* R version 4.0.3 Patched (2021-02-03 r79933), Platform: x86_64-w64-mingw32/x64 (64-bit), Running under: Windows Server 2012 R2 x64 (build 9600): no NOTEs, WARNINGs, or ERRORs.

### Travis CI
* Ubuntu 16.04.6 LTS, R version 4.0.2 (2020-06-22), Platform: x86_64-pc-linux-gnu (64-bit): no NOTEs, WARNINGs, or ERRORs.


## Downstream dependencies

I have also run R CMD check on downstream dependencies of cutpointr using the
revdepcheck package. The one downstream package passed the check.