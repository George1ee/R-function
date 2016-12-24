mytra<-function(a,b,EPS=10^(-6),M=20,f){
   h<-b-a;t1<-(b-a)*(f(a)+f(b))/2
   for (i in 1:M) {
      S<-0
      for (j in 1:2^(i-1)) {
         S<-S+f(a+(2*j-1)*h/2)
      }
      t2<-(t1+h*S)/2
      if(abs(t1-t2)<=EPS){
         return(list(Integral_value=t2,Times=i))
         break
      }
      h<-h/2;t1<-t2
   }
   if(i==M){print(paste0("经过",M,"次对分区间仍未求得满足精度要求的积分近似解"))}
}