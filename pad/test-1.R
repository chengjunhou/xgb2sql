library(xgboost)
source('pad/onehot-script.R')
source('pad/script.R')

df = data.frame(ggplot2::diamonds)
df$ID = seq(1, dim(df)[1])
head(df)


out <- fun_data_prep(df,output_file_name='pad/onehot.txt')
x <- out$model.matrix[,-which(colnames(out$model.matrix)=='price')]
y <- out$model.matrix[,which(colnames(out$model.matrix)=='price')]

bst <- xgboost(data = x,
               label = y,
               max.depth = 3,
               eta = .3,
               nround = 5,
               nthread = 2,
               objective = 'reg:linear',
               eval_metric = 'mae')

#dt <- xgb.model.dt.tree(model = bst)
#xgb_dump <- xgboost::xgb.dump(bst)

y.pred = predict(bst, x)
View(cbind(df, y.pred))

fun_xgboost_to_sql(bst,output_file_name='pad/xgb.txt',
                   input_table_name='df_diamonds_onehot',unique_id='ID')

