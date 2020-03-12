I have run R CMD CHECK on: 
* local Mac OS: R 3.6.1
* travis-ci: R 3.1, R 3.2, R 3.3, R 3.4, R-oldrel, R-release, R-devel, R-devel ATC
* win-builder: R-release, R-oldrel, R-devel

There were no ERROR or WARNINGs and 1 NOTE:
checking installed package size ... NOTE
    installed size is  5.1Mb
    sub-directories of 1Mb or more:
      java   4.4Mb

I checked the 2 reverse dependencies (ggdemetra and rjdqa) and there were no ERROR or WARNING. 
