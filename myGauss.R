source("SloveTr.R")
myGauss<-function(A,b,EPS = 2.2204*10^(-6)){
   ##Using main element elimination method to slove Ax=b
   n<-nrow(A)
   A<-cbind(A,b)
   for (k in 1:(n-1)) {
      Max<-A[which.max(abs(A[,k])),]#Storage the line of the main element
      A[which.max(abs(A[,k])),]<-A[k,]
      A[k,]<-Max
      if(abs(A[k,k])<=EPS){print("Method failed");break}
      for (i in (k+1):n) {
         A[i,k]<-A[i,k]/A[k,k]
         for (j in (k+1):(n+1)) {
            A[i,j]<-A[i,j]-A[i,k]*A[k,j]
         }
      }
   }
   if(abs(A[n,n])<=EPS){print("det(A)=0")}
   else{
      B<-matrix(rep(0,n*n),n)
      for (i in 1:n) {
         B[i,i:n]<-A[i,i:n]
      }
      x<-SloveTr(B,A[,n+1])
      return(x)
   }
}



##test##
A<-matrix(c(10^(-8),2,3,-1,3.712,4.623,-2,1.072,5.643),nrow = 3,byrow = T)
b<-c(1,2,3)
myGauss(A,b)