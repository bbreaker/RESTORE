# RESTORE
functions and scripts used for the RESTORE project conducted within the USGS Lower Mississippi-Gulf Water Science Center

Software
--------

If R (version 3.3 or greater) is not already installed on your computer, download and install the latest binary distribution from the Comprehensive R Archive Network ([CRAN](https://cloud.r-project.org/)). R can also be run from RStudio, which is a nice interface for working with R and provides a console. In order to use RStudio, R must be installed first. The latest version of RStudio is available [here](https://www.rstudio.com/products/rstudio/download/). R and RStudio should be accessible from the start menu, a desktop shortcut, or via command shell. Assuming you installed R on a 64-bit operating system (OS), two versions of R are made available, that is, a 32-bit and 64-bit version. The 64-bit version of R (R x64) is preferred. In order to add the USGS Geological Survey R Archive Network, or the [GRAN](https://owi.usgs.gov/R/gran.html), as a repository from which to download USGS R packages;

``` r
rprofile_path = file.path(Sys.getenv("HOME"), ".Rprofile")
write('\noptions(repos=c(getOption(\'repos\'),
    CRAN=\'https://cloud.r-project.org\',
    USGS=\'https://owi.usgs.gov/R\'))\n',
      rprofile_path, 
      append =  TRUE)
```

Disclaimer
----------

Software created by USGS employees along with contractors and grantees (unless specific stipulations are made in a contract or grant award) are to be released as Public Domain and free of copyright or license. Contributions of software components such as specific algorithms to existing software licensed through a third party are encouraged, but those contributions should be annotated as freely available in the Public Domain wherever possible. If USGS software uses existing licensed components, those licenses must be adhered to and redistributed.

Although this software has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to accuracy and functionality, nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.
