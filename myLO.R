## m is the number of the variables, n = sum(m)
##print all Lexicographic Order
myLO<-function(n,m){
        ##n is the degree of this polynomial
        k<-choose(m+n-1,n)##k is the number decomposion
        A<-matrix(rep(0,m*k),ncol = m)
        npolys<-c(rep(n,m))
        ind<-c(rep(0,m))
        i<-1
        ##得到所有的重复组合记作A
        repeat{
        while(ind[m] <= npolys[m]){
                if(sum(ind)==n){
                        A[i,]<-ind
                        i <- i+1
                }
                ind[m]<-ind[m]+1
        }
        j<-m-1
        if(j<1){break}
        while(ind[j]==npolys[j]){
                j<-j-1
                if(j<1){break}
        }
        if(j<1){break}
        ind[j]<-ind[j]+1
        for(p in (j+1):m){
                ind[p]<-0
        }
        }
        B<-A
        for(i in 1:k){
                A[i,]<-B[k+1-i,]
        }
        return(A)
}
