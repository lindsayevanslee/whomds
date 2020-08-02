## Test environments
* macOS Catalina 10.15.6 (local install), R 3.6.1
* ubuntu 16.04.6 LTS (on travis-ci), R 4.0.0
* macOS High Sierra 10.13.6 (on travis-ci), R 4.0.2
* Windows Server 2012 R2 x64 (on appveyor), R 4.0.2
* win-builder (devel and release)

## R CMD check results

* This is a new release.

There were no ERRORs or WARNINGs. 

There were 0 NOTEs when run on macOS locally.

There were 3 NOTEs when testing on macOS and ubuntu through travis-ci and on Windows through appveyor:

* checking package dependencies ... NOTE

  "Imports includes 28 non-default packages..."
  
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
  
  "License components with restrictions and base license permitting such: GPL-3 + file LICENSE"... 
  
  This is the license that has been checked by the World Health Organization legal department.
  
  "Possibly mis-spelled words in DESCRIPTION: MDS (6:43)" 
  
  This is an abbreviation of the survey whose data will be analyzed with this package.



## Downstream dependencies 

* There are currently no downstream dependencies for this package.