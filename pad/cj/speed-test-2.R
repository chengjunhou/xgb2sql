


time.now <- Sys.time()
sink('pad/zxc.txt',type = "output")
for (i in 1:50000) {
  cat(i,'\n CASE WHEN')
}
sink()
Sys.time() - time.now


time.now <- Sys.time()
sink('pad/xcv.txt',type = "output")
for (i in 1:50000) {
  cat(sprintf("%.1f",i),'\n CASE WHEN')
}
sink()
Sys.time() - time.now



time.now <- Sys.time()
sink('pad/xcv.txt',type = "output")
for (i in 1:50000) {
  cat(i,' CASE WHEN')
}
sink()
Sys.time() - time.now




time.now <- Sys.time()
fileConn<-file("pad/xcv.txt")
for (i in 1:5000) {
  writeLines(c(i,'\n'), fileConn)
}
close(fileConn)
Sys.time() - time.now




time.now <- Sys.time()
for (i in 1:50000) {
  if (i==1) {
    temp = i
  } else {
    temp = paste0(temp,'\n',i)
  }
}
sink('pad/xcv.txt',type = "output")
cat(temp)
sink()
Sys.time() - time.now




source('pad/script.R')
time.now <- Sys.time()
fun_xgboost_to_sql(bst,unique_id='ID',output_file_name='pad/xgb.txt',
                   input_table_name='df_diamonds_onehot')
Sys.time() - time.now


source('pad/scriptt.R')
time.now <- Sys.time()
fun_xgboost_to_sql(bst,unique_id='ID',output_file_name='pad/xgb.txt',
                   input_table_name='df_diamonds_onehot')
Sys.time() - time.now



data = df
catg.index = c(2,3,4)
time.now <- Sys.time()
for (i in 1:10000) {
  contra.lst <- lapply(data[,catg.index], contrasts, contrasts=FALSE)
}
Sys.time() - time.now


data = df
catg.index = c(2,3,4)
time.now <- Sys.time()
for (i in 1:10000) {
  contra.lst <- lapply(data.frame(data[,catg.index]), contrasts, contrasts=FALSE)
}
Sys.time() - time.now

