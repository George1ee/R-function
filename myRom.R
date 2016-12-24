myRom<-function(a,b,EPS=10^(-6),M=30,f){
   h<-b-a;R<-matrix(rep(0,M*M),M)
   R[1,1]<-h*(f(a)+f(b))/2
   for (k in 2:M) {
      t<-0
      for (i in 1:2^(k-2)) {
         t<-t+f(a+(i-0.5)*h)
      }
      R[k,1]<-(R[k-1,1]+h*t)/2
      for (j in 2:k) {
         R[k,j]=R[k,j-1]+(R[k,j-1]-R[k-1,j-1])/(4^(j-1)-1)
      }
      if(abs(R[k,k]-R[k-1,k-1])<=EPS){
         return(list(Integral_value=R[k,k],Times=k))
         break
      }
      h<-h/2
   }
   if(i==M){print(paste0("经过",M,"次对分区间仍未求得满足精度要求的积分近似解"))}
}