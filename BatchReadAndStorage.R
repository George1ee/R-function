X<-c(78.1,72.4,76.2,74.3,77.4)
###Batch storage
for (i in 1:5) {
   write.table(X[i],file = paste(as.character(i),".txt",sep = ""))
}
a<-0
###Batch read
for (i in 1:5) {
   a[i]<-read.table(file = paste(as.character(i),".txt",sep = ""),header = T)
}