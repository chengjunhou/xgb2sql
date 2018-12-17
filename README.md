This package enables in-database scoring of XGBoost model built in R. 
To use this core functionality, call `xgb2sql()` and pass the trained XGBoost model, 
name of the ouput SQL file, name of the input database table, and name of a unique identifier within the raw table. 
Output of the function is a SQL script suitable for all SQL engines.

We are still working on improving the computation efficiency. However, all functions should be working correctly.


## Installation
Development version:
```r
devtools::install_github("chengjunhou/tree2sql")
```


## Sample Code
```r
### load data
df = data.frame(ggplot2::diamonds)
df$ID = seq(1, dim(df)[1])
head(df)

### data processing
out <- tree2sql::onehot2sql(df, output_file_name='onehot.txt')
# sql one-hot script is outputed to onehot.txt
x <- out$model.matrix[,-which(colnames(out$model.matrix)=='price')]
y <- out$model.matrix[,which(colnames(out$model.matrix)=='price')]

### model training
bst <- xgboost(data = x,
               label = y,
               max.depth = 3,
               eta = .3,
               nround = 5,
              nthread = 2,
               objective = 'reg:linear',
              eval_metric = 'mae')

### generate model scoring SQL script
tree2sql::xgb2sql(bst, output_file_name='xgb.txt', input_table_name='df_diamonds', unique_id='ID')
# sql model scoring script is outputed to xgb.txt
# note that there must be a unique identifier column inside the input table
```

