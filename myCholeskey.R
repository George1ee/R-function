source("SloveTr.R")
myCholeshey<-function(A,b,EPS = 2.2204*10^(-6)){
   n<-nrow(A)
   for (r in 1:n) {
      if(r>1){
         for (i in r:n) {
            A[i,r]<-A[i,r]-A[i,1:(r-1)]%*%A[r,1:(r-1)]
         }
      }
      A[r,r]<-sqrt(A[r,r])
      if(r<n){
         for (i in (r+1):n) {
            A[i,r]<-A[i,r]/A[r,r]
         }
      }
   }
   B<-matrix(rep(0,n*n),n)
   for (i in 1:n) {
      B[i,1:i]<-A[i,1:i]
   }
   y<-SloveTr(B,b)
   x<-SloveTr(t(B),y)
   return(x)
}


##test##
A<-matrix(c(4,-1,1,-1,17/4,11/4,1,11/4,3.5),nrow = 3,byrow = T)
b<-c(0,1,0)
myCholeshey(A,b)