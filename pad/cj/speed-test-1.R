
xx = df
xx$cut = as.character(xx$cut)
xx$color = as.character(xx$color)
xx$clarity = as.character(xx$clarity)
xx = xx[,catg.index]
nnn = names(xx)

now.ts = Sys.time()
for (i in 1:1000) {
  df[,catg.index] <- lapply(seq_along(df[,catg.index]), function(i)
    factor(xx[[i]],levels=rownames(contra.lst[[names(xx)[i]]])[-1]))
}
Sys.time() - now.ts

now.ts = Sys.time()
for (i in 1:1000) {
  df[,catg.index] <- lapply(seq_along(df[,catg.index]), function(i)
    factor(xx[[i]],levels=rownames(contra.lst[[nnn[i]]])[-1]))
}
Sys.time() - now.ts



now.ts = Sys.time()
for (i in 1:1000) {
  df[,catg.index] <- lapply(seq_along(df[,catg.index]), function(i)
    factor(df[,catg.index][[i]],levels=rownames(contra.lst[[names(df[,catg.index])[[i]]]])[-1]))
}
Sys.time() - now.ts


now.ts = Sys.time()
for (i in 1:1000) {
  for (j in 1:dim(df[,catg.index])[2]) {
    df[,catg.index][j] <- factor(xx[[j]],levels=rownames(contra.lst[[names(xx)[[j]]]])[-1])
  }
}
Sys.time() - now.ts



now.ts = Sys.time()
for (i in 1:1000) {
  dt[, (catg.index):=lapply(seq_along(.SD),function(i)
    factor(.SD[[i]],levels=rownames(contra.lst[[names(.SD)[[i]]]])[-1])), .SDcols=catg.index]
}
Sys.time() - now.ts





