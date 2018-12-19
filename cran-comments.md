## Test environments

* local OS X install, R 3.4.3
* ubuntu 14.04 (on travis-ci), R devel
* win-builder (devel)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* Possibly mis-spelled words in DESCRIPTION:
  Scikit (3:58)
  learn's (11:45)
  scikit (11:38)
 
  It is not a spelling mistake. It's the name of a python library.
  This is my first submission on cran.
  
 ## Downstream dependencies
 
I have also run R CMD check on downstream dependencies.
All packages that I could install passed successfully. 
