Ind <- function(P){
        k<-sum(P)
        n<-length(P)
        a<-0;d<-c(rep(0,n-1))
        for(i in 1:(n-1)){
                for(j in (P[i]+1):(k-a)){
                        d[i]<-d[i]+choose(k-j+n-i-1-a,k-j-a)  
                }
                a<-a+P[i]
        }
        m<-n;t<-0
        repeat{
                if(P[m]>0) break
                m<-m-1
                t<-t+1
        }
        y<-sum(d)+1-t
        return(y)
}
