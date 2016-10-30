SloveTr<-function(A,B){
##A is the upper triangular matrix. The function is established to solve Ax=B
   n <- nrow(A);x<-rep(0,n)
   if(sum(A[,1]==0)==(n-1)){
      x[n] <- B[n]/A[n,n]
      for (i in (n-1):1) {
         x[i] <- (B[i]-sum(x[(i+1):n]*A[i,(i+1):n]))/A[i,i]
       }
   }
   else{
      x[1]<-B[1]/A[1,1]
      for (i in 2:n) {
         x[i]<-(B[i]-A[i,1:(i-1)]%*%x[1:(i-1)])/A[i,i]
      }
   }
   return(x)
}
