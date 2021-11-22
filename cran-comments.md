## Resubmission

This is a resubmission. In this version I addressed the following comments from CRAN:

* Added reference links to the Description field of DESCRIPTION

* Referred only to the file LICENSE in the License field of DESCRIPTION

Additionally, further improvements were made:

* Implement ability to calculate a specific version of a cutoff (parameter LIDcutoff) in rasch_model.R

* Change which functions' parameters are inherited for documentation in rasch_quality_children_print.R

* Updated dplyr::select statements in rasch_mds.R and rasch_factor.R to suppress tidyverse message about ambiguous column selection

* Updated rasch_mds.R to export files that indicate the specifications for the parameters testlet_strategy, recod_strategy, split_strategy, and drop_vars

* Fixed a bug in rasch_model.R where local item independence (LID) results were not printing according to specified cut-off (parameter LIDcutoff)

* Added psych package to Depends field of DESCRIPTION, because necessary internal function was not able to be found when calling rasch_factor() if psych was in Imports

* Imported necessary function from GPArotation in rasch_factor()

* Change package used to print figure outputted by fig_LID.R

* Replaced deprecated dplyr::funs() with list(~....) throughout package functions

* Updated title in figures outputted by fig_LID.R

* Updated rasch_mds.R to explicitly call packages for several tidyverse functions

* Fixed typo in documentation title in rasch_model.R

* Fixed typo in fig_LID.R documentation

* Fixed a typo in a comment in fig_LID.R

* Added more objects to whomds.R to suppress "no visible binding for global variable..." NOTE

* Updated URLs in startup message, README and rasch_model(), and whomds.R

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