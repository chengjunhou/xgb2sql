
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


catg.lvec = sapply(contra.lst, nrow)
names(catg.lvec) <- substr(names(catg.lvec),1,nchar(names(catg.lvec))-nchar(sep))
sql.df <- data.frame(matrix(1, ncol=10, nrow=sum(catg.lvec)))
sql.df[['X1']] <- "(case when "
sql.df[['X3']] <- " IS NULL then NULL when "
sql.df[['X5']] <- " = '"
sql.df[['X7']] <- "' then 1 else 0 end) AS '"
sql.df[['X9']] <- "', \n"
index <- 0
for (i in 1:length(catg.lvec)) {
  itemp <- names(catg.lvec)[i]
  sql.df[['X2']][(index+1):(index+catg.lvec[i])] <- itemp
  sql.df[['X4']][(index+1):(index+catg.lvec[i])] <- itemp
  for (j in 1:catg.lvec[i]) {
    jtemp <- rownames(contra.lst[[i]])[j]
    sql.df[['X6']][index+1] <- jtemp
    sql.df[['X8']][index+1] <- paste0(itemp,sep,jtemp)
    index = index + 1
  }
}
sql.df[['X9']][index] <- "' \n"
sql.df[['X10']] <- paste0(sql.df[['X1']],sql.df[['X2']],sql.df[['X3']],sql.df[['X4']],
                          sql.df[['X5']],sql.df[['X6']],sql.df[['X7']],sql.df[['X8']],
                          sql.df[['X9']])

onehot_sql <- paste0("SELECT ",
                     paste(num.vec,collapse=', '), ", \n",
                     paste(sql.df$X10,collapse=''),
                     "FROM ", input_table_name)




