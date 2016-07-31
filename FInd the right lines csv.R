
a<-rep(100000000,61)
b<-dir()
for (i in 1:61) {
   c<-read.csv(b[i],header = F)
   a[i]<-nrow(c)-60000
}
a[which(a<0)]<-1000000000
b[which.min(a)]

