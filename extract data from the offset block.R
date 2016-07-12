#提取my数据框下name元素中在dataroad第一列数据中出现的元素
test<-subset(myr,is.element(myr$name, dataroad[,1]))
