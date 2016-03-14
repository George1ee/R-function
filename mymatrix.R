## m is the number of the variables, n = sum(m)
mymatrix<-function(T,n,m){
        ##n is the degree of this polynomial
        n<-n/2
        k<-choose(m+n-1,n)##k is the number decomposion
        A<-matrix(rep(0,m*k),ncol = m)
        npolys<-c(rep(n,m))
        ind<-c(rep(0,m))
        i<-1
        ##get all the result about x1+x2+..+xm=n
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
        ##Get different position corresponding to the monomial and its position
        a<-matrix(c(rep(1:k, each = k)),ncol = 1)
        b<-matrix(c(rep(1:k, k)),ncol = 1)
        C<-matrix(rep(0, (m*k*k)),ncol = m)
        for(i in 1:(k*k)){
                C[i,]<-A[a[i,1],] + A[b[i,1],]
        }
        source("Ind.R")
        d<-matrix(c(rep(0,k*k)),ncol = 1)
        for(i in 1:(k*k)){
                d[i,1]<-Ind(C[i,])
        }
        D<-cbind(a,b,C,d)
        ##Get the number of occurrences of each position
        h<-max(D[,ncol(D)])
        e<-matrix(1:h,ncol = 1)
        f<-matrix(c(rep(0,h)),ncol = 1)
        g<-cbind(e,f)
        for(i in 1:h){
                for(j in 1:(k*k)){
                        if(D[j,ncol(D)]==i){g[i,2]<-g[i,2]+1}
                }
        }
        ##creat the output matrix
        E<-matrix(rep(0,k*k),ncol = k)
        for(i in 1:k){
                for(j in 1:k){
                        E[i,j]<-T[D[((i-1)*k+j),ncol(D)]]/g[D[((i-1)*k+j),ncol(D)],2]
                }
        }
        return(E)##g is the times of different place of the function
}
