# Package Update - xgb2sql

## Resubmission
This is a resubmission of existing package, resolving the build issue https://cran.r-project.org/web/checks/check_results_xgb2sql.html

In this version I have:
* Implemented fixes on utility function.
* Revised main function, vignette and README following breaking changes on package xgboost.
* Note that the package is still compatible with history version of package xgboost.

## Old submission notes
* This is a resubmission. In this version I have:
- Added single quotes to the word XGBoost as requested by the reviewer. 
And according to other packages on CRAN, word SQL stands for the language, same as R.
So single quotes were not added to the word SQL.

* This is a resubmission. In this version I have:
- Revised URL of the CRAN page for a task view in canonical form
- Changed title case
- Added reference citation to Description for the related methods

## Test environments
* local Windows 8 x64, R 3.5.1, R devel
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.5.2

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.

