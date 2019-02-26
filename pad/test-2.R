library(xgboost)
source('pad/onehot-script.R')
source('pad/script.R')

df = data.frame(ggplot2::diamonds)
df$ID = seq(1, dim(df)[1])
head(df)

out <- fun_data_prep(df[,-11],output_file_name='pad/onehot.txt')
x <- out$model.matrix[,-which(colnames(out$model.matrix)=='price')]
y <- out$model.matrix[,which(colnames(out$model.matrix)=='price')]

###### b0 + sum(score)
rea <- xgboost(data = x,
               label = y,
               max.depth = 2,
               eta = .3,
               nround = 1,
               nthread = 2,
               objective = 'reg:linear')
xgb.dump(rea)
table(predict(rea, x))

reb <- xgboost(data = x,
               label = y,
               max.depth = 2,
               eta = .3,
               nround = 1,
               nthread = 2,
               objective = 'reg:linear',
               base_score=0.6)
xgb.dump(reb)
table(predict(reb, x))


###### log(p0/(1-p0)) + sum(score)
loa <- xgboost(data = x,
               label = as.numeric(y>1000),
               max.depth = 2,
               eta = .3,
               nround = 1,
               nthread = 2,
               objective = 'binary:logistic')
xgb.dump(loa)
table(predict(loa, x))
1/(1+exp(-(-0.515068054)))

lob <- xgboost(data = x,
               label = as.numeric(y>1000),
               max.depth = 2,
               eta = .3,
               nround = 1,
               nthread = 2,
               objective = 'binary:logistic',
               base_score=0.6)
xgb.dump(lob)
table(predict(lob, x))
1/(1+exp(-(log(0.6/(1-0.6)) +0.494501412)))


###### log(p0/(1-p0)) + sum(score)
lra <- xgboost(data = x,
               label = as.numeric(y>1000),
               max.depth = 2,
               eta = .3,
               nround = 1,
               nthread = 2,
               objective = 'binary:logitraw')
xgb.dump(lra)
table(predict(lra, x))

lrb <- xgboost(data = x,
               label = as.numeric(y>1000),
               max.depth = 2,
               eta = .3,
               nround = 1,
               nthread = 2,
               objective = 'binary:logitraw',
               base_score=0.6)
xgb.dump(lrb)
table(predict(lrb, x))
log(0.6/(1-0.6)) +0.494501412


###### check leaf+base>0 then 1 else 0
lha <- xgboost(data = x,
               label = as.numeric(y>1000),
               max.depth = 2,
               eta = .3,
               nround = 2,
               nthread = 2,
               objective = 'binary:hinge')
xgb.dump(lha, with_stats=TRUE)
table(predict(lha, x))
11098+1134+70+950  # 0 counts

lhb <- xgboost(data = x,
               label = as.numeric(y>1000),
               max.depth = 3,
               eta = .3,
               nround = 1,
               nthread = 2,
               objective = 'binary:hinge',
               base_score=0.1)
xgb.dump(lhb, with_stats=TRUE)
table(predict(lhb, x))
11098+1134+70+950  # 0 counts


###### log(mu0) + sum(score)
gma <- xgboost(data = x,
               label = y,
               max.depth = 2,
               eta = .3,
               nround = 1,
               nthread = 2,
               objective = 'reg:gamma')
xgb.dump(gma)
table(predict(gma, x))
exp(log(0.5) + 0.299980313)

gmb <- xgboost(data = x,
               label = y,
               max.depth = 2,
               eta = .3,
               nround = 1,
               nthread = 2,
               objective = 'reg:gamma',
               base_score=2)
xgb.dump(gmb)
table(predict(gmb, x))
exp(log(2) + 0.299921185)


###### mu0 + sum(score)
twa <- xgboost(data = x,
               label = y,
               max.depth = 2,
               eta = .3,
               nround = 1,
               nthread = 2,
               objective = 'reg:tweedie')
xgb.dump(twa)
table(predict(twa, x))
exp(log(0.5) + 0.599900246)

twb <- xgboost(data = x,
               label = y,
               max.depth = 2,
               eta = .3,
               nround = 1,
               nthread = 2,
               objective = 'reg:tweedie',
               base_score=2)
xgb.dump(twb)
table(predict(twb, x))
exp(log(2) + 0.599802852)

