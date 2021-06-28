# cutpointr 1.1.1

## R CMD check results

There were no NOTEs, WARNINGs or ERRORS apart from the NOTE on Win Builder Devel that the DOI in DESCRIPTION may be incorrect. The DOI in the CITATION and DESCRIPTION is for a new JSS publication that will be registered after
publication on CRAN.

### Local
* R version 4.1.0, Platform: x86_64-w64-mingw32/x64 (64-bit) Running under: Windows 10 x64 (build 19043): no NOTEs, WARNINGs, or ERRORs.

### Win Builder
* using R Under development (unstable) (2021-06-22 r80544) using platform: x86_64-w64-mingw32 (64-bit): There was one NOTE regarding a possibly incorrect DOI, see above.

### AppVeyor
* R version 4.1.0 Patched (2021-06-26 r80567) Platform: x86_64-w64-mingw32/x64 (64-bit) Running under: Windows Server 2012 R2 x64 (build 9600): no NOTEs, WARNINGs, or ERRORs.

## Downstream dependencies

I have also run R CMD check on downstream dependencies of cutpointr using the
revdepcheck package. The one downstream package passed the check.