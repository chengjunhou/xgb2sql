library(data.table)
library(xgboost)
library(xgb2sql)
df <- data.frame(ggplot2::diamonds)
head(df)




out <- onehot2sql(df)
print(out$meta)
head(out$model.matrix)


x <- out$model.matrix[,colnames(out$model.matrix)!='price']
y <- out$model.matrix[,colnames(out$model.matrix)=='price']
bst <- xgboost(x = x,
               y = y,
               max_depth = 2,
               learning_rate = .3,
               nrounds = 2,
               objective = 'reg:squarederror')

if (is.null(bst$params)) {
  xgbParams = attributes(bst)$params
} else {
  xgbParams = bst$params
}

booster2sql(bst, output_file_name='xgb.txt')



devtools::install("xgb2sql")


xgb_dump <- xgboost::xgb.dump(bst)
###### get dump feature name ######
### old
zxc_dump = "0:[f1<0.995000005] yes=1,no=2,missing=1"
getinfo(bst, "feature_name")[as.numeric(regmatches(zxc_dump,regexec("f(.*?)[<]",zxc_dump))[[1]][2])+1]
### new
zxc_dump = "0:[carat<1] yes=1,no=2,missing=2"
regmatches(zxc_dump,regexec("[[](.*?)[<]",zxc_dump))[[1]][2]



cat(readChar('xgb.txt', file.info('xgb.txt')$size))

