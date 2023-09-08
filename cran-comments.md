## Test environments
* macOS Ventura 13.3.1(a) (local install); R 4.2.0
* macOS 12.6.7 (GitHub Actions); R 4.3.1
* Microsoft Windows Server 2022 10.0.20348 (GitHub Actions); R 4.3.1
* ubuntu 22.04.3 (Github Actions);  R devel r85079, R 4.3.1, R 4.2.3
* win-builder (devel and release)


## R CMD check results

There were no ERRORs or WARNINGs. 

There were 2 NOTEs when run on macOS locally:

* checking package dependencies ... NOTE
  
  "Imports includes 22 non-default packages..."
  
  I have tried to be careful to minimize the dependencies.

* checking CRAN incoming feasibility ... NOTE
  
  Found the following (possibly) invalid DOIs:
    DOI: 10.1586/erp.11.59
      From: DESCRIPTION
      Status: Forbidden
      Message: 403
      
  I have manually checked this doi, and it does direct you to the appropriate paper.

There were 3 NOTEs when testing on macOS, windows, and ubuntu through GitHub Actions:

* checking package dependencies ... NOTE

  "Imports includes 22 non-default packages..."
  
  I have tried to be careful to minimize the dependencies.
  
* checking for hidden files and directories ... NOTE
  
  "Found the following hidden files and directories: .github" 
  
  Folder listed is included in .Rbuildignore
  
* checking top-level files ... NOTE

  "Non-standard files/directories found at top level" 
  
  All files listed are ones included in .Rbuildignore
    
There were 0 NOTEs when run through win-builder.

## Reverse dependencies 

* There are currently no reverse dependencies for this package.
