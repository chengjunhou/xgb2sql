

df = data.frame(ggplot2::diamonds)
dt = data.table(ggplot2::diamonds)

model.matrix(~ ., data=df[1:3,],
             contrasts.arg = lapply(df[,2:4], contrasts, contrasts=FALSE))

df_na = df
df_na[1,2] =NA
df_na[2,5] =NA

model.matrix(~ ., model.frame(~ ., df_na[1:3,], na.action=na.pass),
             contrasts.arg = lapply(df[,2:4], contrasts, contrasts=FALSE))



class.lst <- lapply(df, class)
#class.vec <- sapply(class.lst, function(x) paste(x,collapse=' '))
num.vec <- names(class.lst)[class.lst%in%c('numeric','integer')]
catg.vec <- names(class.lst)[!class.lst%in%c('numeric','integer')]
catg.index <- which(names(df)%in%catg.vec)
contra.lst <- lapply(df[,catg.index], contrasts, contrasts=FALSE)

onehot_sql <- 'put one hot encoding script here'

names(df)[names(df)%in%catg.vec] <- paste0(names(df)[names(df)%in%catg.vec],'.')
df.mat <- model.matrix(~., model.frame(~., df, na.action=na.pass),
                       contrasts.arg=contra.lst)
attr(df.mat,'assign') <- NULL
attr(df.mat,'contrasts') <- NULL

out.lst <- list()
out.lst[['meta']] <- list(catg.vec, num.vec)
out.lst[['contrasts']] <- contra.lst
out.lst[['sql']] <- onehot_sql
out.lst[['model.matrix']] <- df.mat





