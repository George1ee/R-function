source("SloveTr.R")
myGauss<-function(A,b,EPS = 2.2204*10^(-6)){
   ##Using main element elimination method to slove Ax=b
   n<-nrow(A)
   A<-cbind(A,b)
   for (k in 1:(n-1)) {
      M<-A[k:n,]
      m<-which.max(abs(M[,k]))[1]+k-1
      if(m!=k){
         H<-A[k,]
         A[k,]<-A[m,]
         A[m,]<-H
      }
      if(abs(A[k,k])<=EPS){print("Method failed");break}
      for (i in (k+1):n) {
         A[i,k] <- A[i,k]/A[k,k]
         for (j in (k+1):(n+1)) {
            A[i,j] <- A[i,j]-A[i,k]*A[k,j]
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