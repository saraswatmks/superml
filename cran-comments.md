## Test environments

* local OS X install, R 3.5.1 (local)
* ubuntu 14.04, R devel (on travis-ci),
* win-builder (devel)
* Windows Server 2012 R2 x64 (build 9600) (on appvoyer)
* Rhub
    * Ubuntu Linux 16.04 LTS, R-release, GCC
    * Windows Server 2008 R2 SP1, R-devel, 32/64 bit


## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE (found only on Ubuntu Linux 16.04 LTS, R-release, GCC):

* Possibly mis-spelled words in DESCRIPTION:
  Scikit (3:58)
  learn's (11:45)
  scikit (11:38)
 
  It is not a spelling mistake. It's the name of a python library.
  This is my second submission on cran.
  
 ## Downstream dependencies
 
I have also run R CMD check on downstream dependencies.
All packages that I could install passed successfully. 
