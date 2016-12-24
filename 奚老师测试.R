Alp<-c(1,0);Bet<-c(1/2,sqrt(3)/2)##定义alpha,beta
##计算所有可能的ki,kj
K<-matrix(rep(0,17*17),17)
KK<-matrix(rep(0,2*225),ncol=2)
n<-1
for (i in -8:8) {
   for (j in -8:8) {
      if(sum((i*Alp+j*Bet)^2)<=64){
         K[i+9,j+9]<-1
         KK[n,1]<-i;KK[n,2]<-j
         n<-n+1}
   }
}
colnames(K)<- -8:8;rownames(K)<- -8:8
##定义两个映射id,g。1代表id,-1代表g
myf<-function(i,a){
   #i代表种类，a代表数据
   if(i==1){
      return(a)
   }
   else{
      if(a==c(1,0)){return(c(0,1));break}
      if(a==c(0,1)){return(c(1,0));break}
      if(a==c(8,0)){return(c(0,8));break}
      if(a==c(0,8)){return(c(8,0));break}
      if(a==c(0,0)){return(c(0,0));break} 
   }
}
##定义一个所有顶点的编码方式
#(ki,kj,i,j),从左往右依次从小到大
#共225*4种
#定义Bi,gi
bi<-list(b1=c(0,0),b2=c(1,0),b3=c(8,0),b4=c(0,8))
gi<-c(1,1,1,-1)

##通过KK构造点(ki,kj,i,j)的表
KKK<-matrix(rep(0,900*4),ncol = 4)
KKK[,1]<-rep(KK[,1],each=4);KKK[,2]<-rep(KK[,2],each=4)
KKK[,3]<-rep(c(-1,-1,1,1),225);KKK[,4]<-rep(c(-1,1,-1,1),225)
##构建临接矩阵
A<-matrix(rep(0,900*900),900)
for (i in 1:900) {
   x<-KKK[i,1:2]
   for (j in 1:4) {
      for (k in 1:4) {
         if(j!=k){
            #计算变化后x'
            y<-3*x+myf(KKK[i,3],bi[[j]])-myf(KKK[i,4],bi[[k]])
            #计算变化后为id还是g
            t<-c(gi[j]*KKK[i,3],gi[k]*KKK[i,4])
            #寻找在里面的点，即i可以直接到达的点
            index<-which((KKK[,1]==y[1])&(KKK[,2]==y[2])&
                            (KKK[,3]==t[1])&(KKK[,4]==t[2]))
            if(length(index)!=0){A[i,index]<-1}
         }
      }
   }
}
##计算可达矩阵
S<-A;P<-matrix(rep(0,900*900),900)
for (i in 1:899) {
   P<-P+S
   S<-S%*%A
   S[which(S>1)]<-1
   P[which(P>1)]<-1
   if(i%%50==0){print(i)}
}
write.table(P,"可达矩阵p.txt")
##构造起点B
B<-matrix(rep(0,12*4),ncol = 4)
n<-1
for (i in 1:4) {
   for (j in 1:4) {
      if(i!=j){
         B[n,1]<-(bi[[i]]-bi[[j]])[1]
         B[n,2]<-(bi[[i]]-bi[[j]])[2]
         B[n,3]<-gi[i];B[n,4]<-gi[j]
         n<-n+1
      }
   }
}
Ind1<-rep(0,12)
for (i in 1:12) {
   for (j in 1:900) {
      if(sum((B[i,]-KKK[j,])^2)==0){Ind1[i]<-j;break}
   }
}
##终点
C<-matrix(c(0,0,1,1,0,0,-1,-1),nrow = 2,byrow = T)
Ind2<-rep(0,2)
for (i in 1:2) {
   for (j in 1:900) {
      if(sum((C[i,]-KKK[j,])^2)==0){Ind2[i]<-j;break}
   }
}
##最终矩阵
Fi<-matrix(rep(-1,2*12),nrow=12)
for (i in 1:12) {
   for (j in 1:2) {
      Fi[i,j]<-P[Ind1[i],Ind2[j]]
   }
}
write.table(Fi,"最终矩阵.txt")