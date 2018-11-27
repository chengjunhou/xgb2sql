
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
fun_data_prep(newdata, contrasts.arg=out$contrasts)$model.matrix

# test-2: new data has NA
newdata = df[1:5,]
newdata[1,1]=NA; newdata[2,1]=NA; newdata[3,2]=NA; newdata[3,3]=NA; newdata[5,4]=NA
fun_data_prep(newdata, contrasts.arg=out$contrasts)$model.matrix

# test-3: newdata has new elements
newdata = d2[1:5,]
newdata[5,clarity:='NEW']; newdata[1,tsdt:=as.IDate('2017-05-01')]
fun_data_prep(newdata, contrasts.arg=out2$contrasts)$model.matrix




fun_data_prep <- function(data, contrasts.arg=NULL, sep='.') {

  ### prepare meta info ###
  class.lst <- lapply(data, class)
  #class.vec <- sapply(class.lst, function(x) paste(x,collapse=' '))
  num.vec <- names(class.lst)[class.lst%in%c('numeric','integer')]
  catg.vec <- names(class.lst)[!class.lst%in%c('numeric','integer')]
  catg.index <- which(names(data)%in%catg.vec)
  factor.index <- which(class.lst%like% "factor")

  ### add sep for catg var ###
  if (!is.null(sep)) {
    names(data)[names(data)%in%catg.vec] <- paste0(names(data)[names(data)%in%catg.vec], sep)
  }

  ### if contrasts.arg not given: change to factor & generate contrasts ###
  if (is.null(contrasts.arg)) {
    # col index to be turned into factor
    changeclass.index <- catg.index[!catg.index%in%factor.index]
    if (class(data)[1]=='data.table') {
      if (length(changeclass.index)>0) {
        data[, (changeclass.index):=lapply(.SD,as.factor), .SDcols=changeclass.index]
      }
      contra.lst <- lapply(data[,catg.index,with=FALSE], contrasts, contrasts=FALSE)
    } else {
      if (length(changeclass.index)>0) {
        data[,changeclass.index] <- lapply(df[,changeclass.index], as.factor)
      }
      contra.lst <- lapply(data[,catg.index], contrasts, contrasts=FALSE)
    }
  ### if contrasts.arg given: change to factor with forced levels ###
  } else {
    contra.lst <- contrasts.arg
    if (class(data)[1]=='data.table') {
      x <- data[, catg.index, with=FALSE]
      data[, (catg.index):=lapply(seq_along(.SD),function(i)
        factor(.SD[[i]],levels=rownames(contra.lst[[names(.SD)[[i]]]]))), .SDcols=catg.index]
    } else {
      x <- data[, catg.index]
      data[,catg.index] <- lapply(seq_along(x), function(i)
                                  factor(x[[i]],levels=rownames(contra.lst[[names(x)[[i]]]])))
    }
    # catg feature with new level
    notin.list <- lapply(
      seq_along(x), function(i)
        as.character(unique(x[[i]]))[ !
                     as.character(unique(x[[i]]))%in%rownames(contra.lst[[names(x)[i]]]) ])
    names(notin.list) <- paste0(catg.vec, sep)
    notin.vec <- sapply(notin.list, length)
    notin.vec <- notin.vec[notin.vec>0]
  }

  ### generate one hot sql ###
  onehot_sql <- 'put one hot encoding script here'

  ### model matrix ###
  data.mat <- model.matrix(~., model.frame(~., data, na.action=na.pass),
                           contrasts.arg=contra.lst)
  attr(data.mat,'assign') <- NULL
  attr(data.mat,'contrasts') <- NULL
  if (exists("notin.vec")) {
    if (length(notin.vec)>0) {
      for (i in 1:length(notin.vec)) {
        data.mat[as.character(x[[names(notin.vec)[i]]])%in%notin.list[[names(notin.vec)[i]]],
                 grep(paste0(names(notin.vec)[i],sep),colnames(data.mat))] <- 0
      }
    }
  }

  ### output ###
  out.lst <- list()
  out.lst[['meta']] <- list('num.vec'=num.vec, 'catg.vec'=catg.vec)
  out.lst[['contrasts']] <- contra.lst
  out.lst[['sql']] <- onehot_sql
  out.lst[['model.matrix']] <- data.mat

  return(out.lst)
}


