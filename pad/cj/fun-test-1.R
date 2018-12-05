library(data.table)

df = data.frame(ggplot2::diamonds)
dt = data.table(ggplot2::diamonds)

model.matrix(~ ., data=df[1:3,],
             contrasts.arg = lapply(df[,2:4], contrasts, contrasts=FALSE))

df_na = df
df_na[1,2] =NA
df_na[2,5] =NA
model.matrix(~ ., model.frame(~ ., df_na[1:3,], na.action=na.pass),
             contrasts.arg = lapply(df[,2:4], contrasts, contrasts=FALSE))

df_na = df
df_na[1:3,2] =NA
df_na[2,5] =NA
model.matrix(~ ., model.frame(~ ., df_na[1:3,], na.action=na.pass),
             contrasts.arg = lapply(df[,2:4], contrasts, contrasts=FALSE))

data = df
#data$cut=factor(data$cut,ordered = F)
#data = data[1:3,]
data$cut =as.character(data$cut)
data$cut[1] = "NEW"
data$cut =as.factor(as.character(data$cut))
data$cut = as.character(data$cut); data$cut=factor(data$cut,levels=levels(df$cut))
model.matrix(~ ., model.frame(~ ., data[1:10,], na.action=na.pass),
             contrasts.arg = lapply(df[,2:4], contrasts, contrasts=FALSE))





data = df
data$cut=factor(data$cut,ordered = F)
data$color=as.character(data$color)
data$clarity=as.character(data$clarity)
data = data.table(data)
data[, (changeclass.index):=lapply(.SD,as.factor), .SDcols=changeclass.index]

zxc = as.character(df$cut)
zxc = factor(zxc, levels=(levels(df$cut)[-1]))
zxc = as.character(df$cut[1:5])
zxc = factor(zxc, levels=(levels(df$cut)))

zxc = df$cut
zxc = factor(zxc, levels=(levels(df$cut)[-1]))
zxc = df$cut[1:5]
zxc = factor(zxc, levels=(levels(df$cut)))




class.lst <- lapply(df, class)
#class.vec <- sapply(class.lst, function(x) paste(x,collapse=' '))
num.vec <- names(class.lst)[class.lst%in%c('numeric','integer')]
catg.vec <- names(class.lst)[!class.lst%in%c('numeric','integer')]
catg.index <- which(names(df)%in%catg.vec)
factor.index <- which(class.lst%like% "factor")
changeclass.index <- catg.index[!catg.index%in%factor.index]

contra.lst <- lapply(df[,catg.index], contrasts, contrasts=FALSE)


onehot_sql <- 'put one hot encoding script here'

names(df)[names(df)%in%catg.vec] <- paste0(names(df)[names(df)%in%catg.vec],'.')
df[,changeclass.index] <- lapply(df[,changeclass.index], as.factor)
data[, (changeclass.index):=lapply(.SD,as.factor), .SDcols=changeclass.index]

x <- df[,catg.index]
df[,catg.index] <- lapply(seq_along(x), function(i)
                          factor(x[[i]],levels=rownames(contra.lst[[names(x)[[i]]]])))

x <- dt[,catg.index,with=FALSE]
dt[, (catg.index):=lapply(seq_along(x), function(i)
  factor(x[[i]],levels=rownames(contra.lst[[names(x)[[i]]]])[-1]))]

table(dt$cut);table(dt$color);table(dt$clarity)
dt[, (catg.index):=lapply(seq_along(.SD),function(i)
  factor(.SD[[i]],levels=rownames(contra.lst[[names(.SD)[[i]]]])[-1])), .SDcols=catg.index]


data[, (catg.index):=lapply(.SD,as.factor), .SDcols=catg.index]



df.mat <- model.matrix(~., model.frame(~., df, na.action=na.pass),
                       contrasts.arg=contra.lst)
attr(df.mat,'assign') <- NULL
attr(df.mat,'contrasts') <- NULL

out.lst <- list()
out.lst[['meta']] <- list(catg.vec, num.vec)
out.lst[['contrasts']] <- contra.lst
out.lst[['sql']] <- onehot_sql
out.lst[['model.matrix']] <- df.mat





