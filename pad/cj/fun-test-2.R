
df = data.frame(ggplot2::diamonds)
x <- df[,catg.index]
x$cut <- as.character(x$cut)
x[1,1] = 'NEW'

notin.list <- lapply(
  seq_along(x), function(i)
    as.character(unique(x[[i]]))[ !
      as.character(unique(x[[i]]))%in%rownames(contra.lst[[names(x)[i]]]) ]
  )
names(notin.list) <- catg.vec
notin.vec <- sapply(notin.list, length)
notin.vec <- notin.vec[notin.vec>0]


