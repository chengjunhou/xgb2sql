library(xgboost)
source('pad/onehot-script.R')
source('pad/script.R')

df = data.frame(ggplot2::diamonds)
df$ID = seq(1, dim(df)[1])
head(df)


out <- fun_data_prep(df[,-11],output_file_name='pad/onehot.txt')
x <- out$model.matrix[,-which(colnames(out$model.matrix)=='price')]
y <- out$model.matrix[,which(colnames(out$model.matrix)=='price')]

bst <- xgboost(data = x,
               label = y,
               max.depth = 2,
               eta = .3,
               nround = 2,
               nthread = 2,
               objective = 'reg:linear',
               eval_metric = 'mae')
xgb.dump(bst)
#dt <- xgb.model.dt.tree(model = bst)
#xgb_dump <- xgboost::xgb.dump(bst)

y.pred = predict(bst, x)
View(cbind(df, y.pred))

fun_xgboost_to_sql(bst,unique_id='ID',output_file_name='pad/xgb.txt',
                   input_table_name='df_diamonds_onehot')

fun_xgboost_to_sql(bst,unique_id='ID',output_file_name='pad/xcv.txt',
                   input_onehot_query='select x,y,z,\nzxc,\nxcv,\ncvb from df_diamonds_onehot')



lo1 <- xgboost(data = x,
               label = as.numeric(y>1000),
               max.depth = 2,
               eta = .3,
               nround = 2,
               nthread = 2,
               objective = 'reg:logistic',
               eval_metric = 'mae')
xgb.dump(lo1)

lo2 <- xgboost(data = x,
               label = as.numeric(y>1000),
               max.depth = 2,
               eta = .3,
               nround = 2,
               nthread = 2,
               objective = 'binary:logistic',
               eval_metric = 'mae')
xgb.dump(lo2)

lo3 <- xgboost(data = x,
               label = as.numeric(y>1000),
               max.depth = 2,
               eta = .3,
               nround = 2,
               nthread = 2,
               objective = 'binary:logitraw',
               base_score = 0.6)
xgb.dump(lo3)
head(predict(lo3, x))
fun_xgboost_to_sql(lo3,unique_id='ID',output_file_name='pad/xgb.txt',
                   input_table_name='df_diamonds_onehot')


