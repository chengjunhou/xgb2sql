library(xgboost)
data(agaricus.train, package='xgboost')

sum(is.na(agaricus.train$data))
##[1] 0

bst <- xgboost(data = agaricus.train$data,
               label = agaricus.train$label,
               max.depth = 4,
               eta = .01,
               nround = 5,
               nthread = 2,
               objective = "binary:logistic")


dt <- xgb.model.dt.tree(model = bst)
all(dt$Missing == dt$Yes,na.rm = T)


y.pred = predict(bst, agaricus.train$data)
dd = as.matrix(agaricus.train$data)

bst <- xgboost(data = dd,
               label = agaricus.train$label,
               max.depth = 4,
               eta = .01,
               nround = 5,
               nthread = 2,
               objective = "binary:logistic")

y.pred = predict(bst, dd)
dd = dd[,order(colnames(dd))]
y.dd = predict(bst, dd)
