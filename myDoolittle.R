source("SloveTr.R")
myDoolittle<-function(A,b,EPS = 2.2204*10^(-6)){
   ##Using Doolittle method to slove Ax=b
   n<-nrow(A)
   A<-cbind(A,b)
   for (r in 1:n) {
      for (i in r:(n+1)) {
         if(r>1){A[r,i]<-A[r,i]-A[r,1:(r-1)]%*%A[1:(r-1),i]}
      }
      if(abs(A[r,r])<=EPS){print("Decompose failed");break}
      if(r<n){
         for (i in (r+1):n) {
            if(r==1){A[i,r]<-A[i,r]/A[r,r]}
            else{A[i,r]<-(A[i,r]-A[i,1:(r-1)]%*%A[1:(r-1),r])/A[r,r]}
         }
      }
   }
   B<-matrix(rep(0,n*n),n)
   for (i in 1:n) {
      B[i,i:n]<-A[i,i:n]
   }
   x<-SloveTr(B,A[,n+1])
   return(x)
}

##test##
A<-matrix(c(2,10,0,-3,-3,-4,-12,13,1,2,3,-4,4,14,9,-13),nrow = 4,byrow = T)
b<-c(10,5,-2,7)
myDoolittle(A,b)