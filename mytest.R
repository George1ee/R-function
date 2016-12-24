mytest<-function(fun,a,b,n,myf){
   k<-rep(0,n);In<-rep(0,n)
   for (i in 1:n) {
      c<-runif(1,a,b)
      R1<-fun(a,c,f=myf);R2<-fun(c,b,f=myf)
      In[i]<-R1$Integral_value + R2$Integral_value
      k[i]<-max(R1$Times,R2$Times)
   }
   return(list(Integral_value=In,Times=k))
}

