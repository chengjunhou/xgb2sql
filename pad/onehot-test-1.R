library(data.table)
source('pad/onehot-script.R')

df = data.frame(ggplot2::diamonds)
head(df)

d1 = data.frame(ggplot2::diamonds)
d1[1,2] = NA  # NA on 1st row cut
d1[2,5] = NA  # NA on 2nd row depth
head(d1)

d2 = data.table(ggplot2::diamonds)
d2[, cut:=factor(cut, ordered=FALSE)]
d2[, clarity:=as.character(clarity)]
d2[, tsdt:=as.IDate('2017-01-05')]
d2[1:3, tsdt:=tsdt-1]
head(d2)

# out is obtained when training
out <- fun_data_prep(df)
out1 <- fun_data_prep(d1)  # NA has no influence
out2 <- fun_data_prep(d2)  # other catg class other than factor


# perform same transformation for newdata when contrasts.arg is given
# test-1: new data has column class change
newdata = df[1:5,]
newdata$cut = as.character(newdata$cut)
fun_data_prep(newdata, meta=out$meta)$model.matrix

# test-2: new data has NA
newdata = df[1:5,]
newdata[1,1]=NA; newdata[2,1]=NA; newdata[3,2]=NA; newdata[3,3]=NA; newdata[5,4]=NA
fun_data_prep(newdata, meta=out$meta)$model.matrix

# test-3: newdata has column with new elements
newdata = d2[1:5,]
newdata[5,clarity:='NEW']; newdata[1,tsdt:=as.IDate('2017-05-01')]
fun_data_prep(newdata, meta=out2$meta)$model.matrix

# test-4: newdata has new columns
newdata = d2[1:5,]
newdata[,new_col:=1]
fun_data_prep(newdata, meta=out2$meta)$model.matrix

# test-5: newdata is lacking some columns
newdata = d2[1:5,]
newdata[,cut:=NULL]
fun_data_prep(newdata, meta=out2$meta)$model.matrix





zdf = data.frame(ggplot2::diamonds)[,c(2,5,6,7)]
zdt = data.table(ggplot2::diamonds)[,c(2,5,6,7),with=FALSE]

out1 <- fun_data_prep(zdf)
head(out1$model.matrix)
out2 <- fun_data_prep(zdt)
head(out2$model.matrix)

fun_data_prep(zdf[1:3,], meta=out1$meta)$model.matrix
fun_data_prep(zdt[1:3,], meta=out1$meta)$model.matrix





