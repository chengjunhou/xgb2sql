# xgb2sql 0.1.2

* This is the first version on CRAN.
* Avaible functions are `onehot2sql()` and `booster2sql()`.


# xgb2sql 0.1.3
* Fix issue for `onehot2sql()` processing data.frame with only one categorical feature.
* Stop `onehot2sql()` and add stop message when there is no categorical feature in the input data.
* Fix example, vignette, and README following breaking changes on package xgboost.
* When missing `input_table_name` for `onehot2sql()`, the query will be printed to console.


# xgb2sql 0.1.4.9000
