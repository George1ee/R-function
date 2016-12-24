mySim<-function(a,b,EPS=10^(-6),M=30,f){
   h <- b-a;t<-f(a)+f(b);old<-f(a+h/2)
   S1=h*(t+4*old)/6;new<-0
   for (i in 1:M) {
      old<-old+new;h=h/2;new<-0
      for (j in 1:2^i) {
         new<-new+f(a+(2*j-1)*h/2)
      }
      S2<-h*(t+2*old+4*new)/6
      if(abs(S2-S1)<=EPS){
         return(list(Integral_value=S2,Times=i))
         break
      }
      S1=S2
   }
   if(i==M){print(paste0("经过",M,"次对分区间仍未求得满足精度要求的积分近似解"))}
}