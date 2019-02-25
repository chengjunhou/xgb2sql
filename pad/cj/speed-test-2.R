


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

