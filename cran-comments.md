## Test environments
* local Ubuntu 16 install, R 3.4.2
* local MacOS 10.12.6 install, R 3.4.2
* win-builder (devel 2017-09-12 r73242)
* ubuntu 14.04 (on travis-ci), R 3.4.2
* Appveyor

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* Unexported object imported by a ':::' call: ‘tibble:::shrink_mat’

To nicely print the cutpointr object the print.tbl_df behaviour of tibble versions < 1.3.0 is needed. As soon as it is restored in a later version of tibble, the call to ::: will be removed and the print.cutpointr method will be simplified. See the corresponding issue at https://github.com/tidyverse/tibble/issues/268

## devtools::revdep_check()

No ERRORs or WARNINGs found
