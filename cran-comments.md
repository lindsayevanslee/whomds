## Resubmission

This is a resubmission. In this version I have changed the example used in the documentation for table_basicstats() in order to reduce its runtime to <5s.

Package (v1.0.1) was previously archived on CRAN after downstream dependency CDM was archived. Changes since this archived version are summarized in NEWS.md.


## Test environments
* macOS Monterey 12.3.1 (local install); R 4.2.0
* macOS 11.6.5 (GitHub Actions); R 4.2.0
* Microsoft Windows Server 2022 10.0.20348 (GitHub Actions); R 4.2.0 and R 3.6.3
* ubuntu 18.04.6 (Github Actions);  R devel r82390, R 4.2.0, R 4.1.3, R 4.0.5, R 3.6.3
* win-builder (devel and release)


## R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE when run on macOS locally:

* checking package dependencies ... NOTE
  
  "Imports includes 22 non-default packages..."
  
  I have tried to be careful to minimize the dependencies, and removed dependencies since the last version.

There were 3 NOTEs when testing on macOS, windows, and ubuntu through GitHub Actions:

* checking package dependencies ... NOTE

  "Imports includes 22 non-default packages..."
  
  I have tried to be careful to minimize the dependencies, and removed dependencies since the last version.
  
* checking for hidden files and directories ... NOTE
  
  "Found the following hidden files and directories: .github" 
  
  Folder listed is included in .Rbuildignore
  
* checking top-level files ... NOTE

  "Non-standard files/directories found at top level" 
  
  All files listed are ones included in .Rbuildignore
    
There was 1 NOTE when run through win-builder:

* checking CRAN incoming feasibility ... NOTE

  "Maintainer: 'Lindsay Lee <lindsayevanslee@gmail.com>'

  New submission
  
  Package was archived on CRAN
  
  Possibly misspelled words in DESCRIPTION"
  
  This package is an updated to the previously archived version. The words possibility-misspelled are names or acronyms.


## Reverse dependencies 

* There are currently no reverse dependencies for this package.
