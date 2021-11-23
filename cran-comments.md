## Resubmission

This is a resubmission. In this version I addressed the following comments from CRAN:

* Removed examples from unexported functions helper_checkrow() and helper_installation()

* Made writing results to the user's machine the non-default behavior for all relevant functions (via parameter print_results = FALSE). Checked examples, tests and vignettes for export behavior, and none was found.

Additionally I made the following improvements:

* Corrected a typo in the documentation for rasch_split()

* Updated image link for travis-ci status icon


## Test environments
* macOS Big Sur 11.6 (local install), R 4.0.3
* ubuntu 16.04.7 LTS (on travis-ci), R 4.0.2
* macOS Big Sur 10.16 (on travis-ci), R 4.1.2
* Windows Server 2012 R2 x64 (on appveyor), R 4.1.2 Patched
* win-builder (devel and release)

## R CMD check results

* This is a new release.

There were no ERRORs or WARNINGs. 

There was 1 NOTE when run on macOS locally:

* checking package dependencies ... NOTE
  
  "Imports includes 29 non-default packages..."
  
  I have tried to be careful to minimize the dependencies.

There were 3 NOTEs when testing on macOS and ubuntu through travis-ci and on Windows through appveyor:

* checking package dependencies ... NOTE

  "Imports includes 29 non-default packages..."
  
  I have tried to be careful to minimize the dependencies.
  
* checking for hidden files and directories ... NOTE
  
  "Found the following hidden files and directories: .travis.yml" 
  
  File listed is included in .Rbuildignore
  
* checking top-level files ... NOTE

  "Non-standard files/directories found at top level" 
  
  All files listed are ones included in .Rbuildignore
    
There was 1 NOTE when run through win-builder:

* checking CRAN incoming feasibility ... NOTE
  "Maintainer: 'Lindsay Lee <lindsayevanslee@gmail.com>' New submission"
  
  "Non-FOSS package license (file LICENSE)"
  
  This is the license that has been checked by the World Health Organization legal department.
  
  "Possibly mis-spelled words in DESCRIPTION:   Andrich (12:25) MDS (6:43) Rasch (12:5)" 
  
  "Andrich" is an author name in one of the included references,  "MDS" is an abbreviation of the survey whose data will be analyzed with this package, and "Rasch" is the name of the method this package implements. 



## Downstream dependencies 

* There are currently no downstream dependencies for this package.