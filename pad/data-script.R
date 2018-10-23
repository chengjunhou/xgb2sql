
df = data.frame(ggplot2::diamonds)
head(df)
dt = data.table(ggplot2::diamonds)
dt[1,2] = NA  # NA on 1st row cut
dt[2,5] = NA  # NA on 2nd row depth
head(dt)

# out is obtained when training
out <- fun_data_prep(df)
out <- fun_data_prep(dt)  # NA has no influence

# perform same transformation for newdata when contrasts.arg is given
# test-1: new data has limited catg levels
newdata = df[1:3,]
fun_data_prep(newdata, contrasts.arg=out$contrasts)$model.matrix

# test-2: new data has NA
newdata = df[1:5,]
newdata[1,1]=NA; newdata[2,1]=NA; newdata[3,2]=NA; newdata[3,3]=NA; newdata[5,4]=NA
fun_data_prep(newdata, contrasts.arg=out$contrasts)$model.matrix







fun_data_prep <- function(data, contrasts.arg=NULL, sep='.') {

  ### prepare meta info ###
  class.lst <- lapply(data, class)
  #class.vec <- sapply(class.lst, function(x) paste(x,collapse=' '))
  num.vec <- names(class.lst)[class.lst%in%c('numeric','integer')]
  catg.vec <- names(class.lst)[!class.lst%in%c('numeric','integer')]
  catg.index <- which(names(data)%in%catg.vec)

  ### add sep for catg var ###
  if (!is.null(sep)) {
    names(data)[names(data)%in%catg.vec] <- paste0(names(data)[names(data)%in%catg.vec], sep)
  }
  ### generate contrasts ###
  if (is.null(contrasts.arg)) {
    if (class(data)[1]=='data.table') {
      contra.lst <- lapply(data[,catg.index,with=FALSE], contrasts, contrasts=FALSE)
    } else {
      contra.lst <- lapply(data[,catg.index], contrasts, contrasts=FALSE)
    }
  } else {
    contra.lst <- contrasts.arg
  }

  ### generate one hot sql ###
  onehot_sql <- 'put one hot encoding script here'

  ### model matrix ###
  data.mat <- model.matrix(~., model.frame(~., data, na.action=na.pass),
                         contrasts.arg=contra.lst)
  attr(data.mat,'assign') <- NULL
  attr(data.mat,'contrasts') <- NULL

  ### output ###
  out.lst <- list()
  out.lst[['meta']] <- list(num.vec=num.vec, catg.vec=catg.vec)
  out.lst[['contrasts']] <- contra.lst
  out.lst[['sql']] <- onehot_sql
  out.lst[['model.matrix']] <- data.mat

  return(out.lst)
}
