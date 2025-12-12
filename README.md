# xgb2sql

#### CRAN Release

[![CRAN version](http://www.r-pkg.org/badges/version/xgb2sql)](https://cran.r-project.org/package=xgb2sql)

#### Build Status

[![Travis build status](https://app.travis-ci.com/chengjunhou/xgb2sql.svg?branch=master)]( https://app.travis-ci.com/chengjunhou/xgb2sql)

#### Total Downloads

[![](https://cranlogs.r-pkg.org/badges/grand-total/xgb2sql)](https://cran.r-project.org/package=xgb2sql)

#### Pacakge Vignettes

[Deploy XGBoost Model as SQL Query](https://cran.r-project.org/package=xgb2sql)

#### Description

This pacakge enables in-database scoring of XGBoost models built in R, by translating trained model objects into SQL query.



## Installation

Latest CRAN release:
```r
install.packages("xgb2sql")
```

Development version:
```r
devtools::install_github("chengjunhou/xgb2sql")
```



## Function

```
onehot2sql(data, meta=NULL, sep="_", ws_replace=TRUE, ws_replace_with="",
           unique_id=NULL, output_file_name=NULL, input_table_name=NULL)
```

Function `onehot2sql()` performs full one-hot encoding for all the categorical features inside the training data,
with all NAs inside both categorical and numeric features preserved.
Other than outputting a matrix `model.matrix` which is the data after processing,
it also outputs `meta` information keeping track of all the transformation the function performs,
while SQL query for the transformation is kept in output `sql` and write to the file specified by `output_file_name`.
If `meta` is specified as input to the function, the transformation and the corresponding SQL query will
follow what is kept in `meta` exactly.

```
booster2sql(xgbModel, print_progress=FALSE, unique_id=NULL,
            output_file_name=NULL, input_table_name=NULL, input_onehot_query=NULL)
```

Function `booster2sql()` generates SQL query for in-database scoring of XGBoost models,
providing a robust and efficient way of model deployment. It takes in the trained XGBoost model `xgbModel`,
name of the input database table `input_table_name`,
and name of a unique identifier within that table `unique_id` as input,
writes the SQL query to a file specified by `output_file_name`.
Note that the input database table should be generated from the raw table using the one-hot encoding query output by `onehot2sql()`,
or to provide the one-hot encoding query as input `input_onehot_query` to this function, 
working as sub-query inside the final model scoring query.

Current supported booster is `booster="gbtree"`, supported `objective` options are:
- `reg:linear`: linear regression.
- `reg:logistic`: logistic regression.
- `binary:logistic`: logistic regression for binary classification, output probability.
- `binary:logitraw`: logistic regression for binary classification, output score before logistic transformation.
- `binary:hinge`: hinge loss for binary classification. This makes predictions of 0 or 1, rather than producing probabilities.
- `count:poisson`: poisson regression for count data, output mean of poisson distribution.
- `reg:gamma`: gamma regression with log-link, output mean of gamma distribution. It might be useful, e.g., for modeling insurance claims severity, or for any outcome that might be gamma-distributed.
- `reg:tweedie`: Tweedie regression with log-link. It might be useful, e.g., for modeling total loss in insurance, or for any outcome that might be Tweedie-distributed.



## Sample Code
```r
### load pacakge and data
library(data.table)
library(xgboost)
library(xgb2sql)
df <- data.frame(ggplot2::diamonds)
head(df)
#>   carat       cut color clarity depth table price    x    y    z
#> 1  0.23     Ideal     E     SI2  61.5    55   326 3.95 3.98 2.43
#> 2  0.21   Premium     E     SI1  59.8    61   326 3.89 3.84 2.31
#> 3  0.23      Good     E     VS1  56.9    65   327 4.05 4.07 2.31
#> 4  0.29   Premium     I     VS2  62.4    58   334 4.20 4.23 2.63
#> 5  0.31      Good     J     SI2  63.3    58   335 4.34 4.35 2.75
#> 6  0.24 Very Good     J    VVS2  62.8    57   336 3.94 3.96 2.48


### data processing
out <- onehot2sql(df)
head(out$model.matrix)
#>   (Intercept) carat clarity_I1 clarity_IF clarity_SI1 clarity_SI2
#> 1           1  0.23          0          0           0           1
#> 2           1  0.21          0          0           1           0
#> 3           1  0.23          0          0           0           0
#> 4           1  0.29          0          0           0           0
#> 5           1  0.31          0          0           0           1
#> 6           1  0.24          0          0           0           0
#>   clarity_VS1 clarity_VS2 clarity_VVS1 clarity_VVS2 color_D color_E
#> 1           0           0            0            0       0       1
#> 2           0           0            0            0       0       1
#> 3           1           0            0            0       0       1
#> 4           0           1            0            0       0       0
#> 5           0           0            0            0       0       0
#> 6           0           0            0            1       0       0
#>   color_F color_G color_H color_I color_J cut_Fair cut_Good cut_Ideal
#> 1       0       0       0       0       0        0        0         1
#> 2       0       0       0       0       0        0        0         0
#> 3       0       0       0       0       0        0        1         0
#> 4       0       0       0       1       0        0        0         0
#> 5       0       0       0       0       1        0        1         0
#> 6       0       0       0       0       1        0        0         0
#>   cut_Premium cut_VeryGood depth price table    x    y    z
#> 1           0            0  61.5   326    55 3.95 3.98 2.43
#> 2           1            0  59.8   326    61 3.89 3.84 2.31
#> 3           0            0  56.9   327    65 4.05 4.07 2.31
#> 4           1            0  62.4   334    58 4.20 4.23 2.63
#> 5           0            0  63.3   335    58 4.34 4.35 2.75
#> 6           0            1  62.8   336    57 3.94 3.96 2.48
cat(out$sql)
#> SELECT ROW_KEY, [carat], [depth], [table], [price], [x], [y], [z], 
#> (case when [cut] IS NULL then NULL when [cut] = 'Fair' then 1 else 0 end) AS [cut_Fair], 
#> (case when [cut] IS NULL then NULL when [cut] = 'Good' then 1 else 0 end) AS [cut_Good], 
#> (case when [cut] IS NULL then NULL when [cut] = 'Very Good' then 1 else 0 end) AS [cut_VeryGood], 
#> (case when [cut] IS NULL then NULL when [cut] = 'Premium' then 1 else 0 end) AS [cut_Premium], 
#> (case when [cut] IS NULL then NULL when [cut] = 'Ideal' then 1 else 0 end) AS [cut_Ideal], 
#> (case when [color] IS NULL then NULL when [color] = 'D' then 1 else 0 end) AS [color_D], 
#> (case when [color] IS NULL then NULL when [color] = 'E' then 1 else 0 end) AS [color_E], 
#> (case when [color] IS NULL then NULL when [color] = 'F' then 1 else 0 end) AS [color_F], 
#> (case when [color] IS NULL then NULL when [color] = 'G' then 1 else 0 end) AS [color_G], 
#> (case when [color] IS NULL then NULL when [color] = 'H' then 1 else 0 end) AS [color_H], 
#> (case when [color] IS NULL then NULL when [color] = 'I' then 1 else 0 end) AS [color_I], 
#> (case when [color] IS NULL then NULL when [color] = 'J' then 1 else 0 end) AS [color_J], 
#> (case when [clarity] IS NULL then NULL when [clarity] = 'I1' then 1 else 0 end) AS [clarity_I1], 
#> (case when [clarity] IS NULL then NULL when [clarity] = 'SI2' then 1 else 0 end) AS [clarity_SI2], 
#> (case when [clarity] IS NULL then NULL when [clarity] = 'SI1' then 1 else 0 end) AS [clarity_SI1], 
#> (case when [clarity] IS NULL then NULL when [clarity] = 'VS2' then 1 else 0 end) AS [clarity_VS2], 
#> (case when [clarity] IS NULL then NULL when [clarity] = 'VS1' then 1 else 0 end) AS [clarity_VS1], 
#> (case when [clarity] IS NULL then NULL when [clarity] = 'VVS2' then 1 else 0 end) AS [clarity_VVS2], 
#> (case when [clarity] IS NULL then NULL when [clarity] = 'VVS1' then 1 else 0 end) AS [clarity_VVS1], 
#> (case when [clarity] IS NULL then NULL when [clarity] = 'IF' then 1 else 0 end) AS [clarity_IF] 
#> FROM INPUT_TABLE


### model training
x <- out$model.matrix[,colnames(out$model.matrix)!='price']
y <- out$model.matrix[,colnames(out$model.matrix)=='price']
bst <- xgboost(x = x,
               y = y,
               max_depth = 2,
               learning_rate = .3,
               nrounds = 2,
               objective = 'reg:squarederror')
#> [1]  train-rmse:4095.421387 
#> [2]  train-rmse:3074.222412


### generate XGBoost SQL script
booster2sql(bst, output_file_name='xgb.txt')
#> query is written to file with row unique id named as ROW_KEY
#> query is written to file with input table named as MODREADY_TABLE
cat(readChar('xgb.txt', file.info('xgb.txt')$size))
#> SELECT  ROW_KEY , 0.5 + SUM(ONETREE) AS XGB_PRED
#> FROM (   
#>  SELECT ROW_KEY ,
#>  (CASE WHEN [carat] < 0.995000005 THEN 
#>  (CASE WHEN [y] < 5.53499985 THEN 317.401001
#>   WHEN  [y] >= 5.53499985 THEN 922.349731
#>   WHEN  [y] IS NULL THEN 317.401001 END)
#>   WHEN  [carat] >= 0.995000005 THEN 
#>  (CASE WHEN [y] < 7.19499969 THEN 1841.06018
#>   WHEN  [y] >= 7.19499969 THEN 3696.24292
#>   WHEN  [y] IS NULL THEN 1841.06018 END)
#>   WHEN  [carat] IS NULL THEN 
#>  (CASE WHEN [y] < 5.53499985 THEN 317.401001
#>   WHEN  [y] >= 5.53499985 THEN 922.349731
#>   WHEN  [y] IS NULL THEN 317.401001 END) END) AS ONETREE FROM  MODREADY_TABLE 
#>  UNION ALL 
#>  
#>  SELECT ROW_KEY ,
#>  (CASE WHEN [y] < 6.69499969 THEN 
#>  (CASE WHEN [carat] < 0.824999988 THEN 289.332123
#>   WHEN  [carat] >= 0.824999988 THEN 1056.4021
#>   WHEN  [carat] IS NULL THEN 289.332123 END)
#>   WHEN  [y] >= 6.69499969 THEN 
#>  (CASE WHEN [y] < 7.65499973 THEN 1814.65881
#>   WHEN  [y] >= 7.65499973 THEN 3217.57129
#>   WHEN  [y] IS NULL THEN 1814.65881 END)
#>   WHEN  [y] IS NULL THEN 
#>  (CASE WHEN [carat] < 0.824999988 THEN 289.332123
#>   WHEN  [carat] >= 0.824999988 THEN 1056.4021
#>   WHEN  [carat] IS NULL THEN 289.332123 END) END) AS ONETREE FROM  MODREADY_TABLE   
#> ) AS TREES_TABLE GROUP BY  ROW_KEY
```



## Under Development

Items under development are:
- Support for `booster="gblinear`.
- Support for other `objective`.
- Support for customized loss function.

- Support for pacakge **sparkxgb**, which is a [sparklyr](https://spark.posit.co/) extension 
that provides an interface to [XGBoost](https://github.com/dmlc/xgboost) on Spark.




