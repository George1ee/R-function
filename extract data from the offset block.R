#提取my数据框下name元素中在dataroad第一列数据中出现的元素
myr<-read.table("myr.txt");dataroad<-read.table("dataroad.txt")
test<-subset(myr,is.element(myr$name, dataroad[,1]))
