##This is a function about how to get the adjacent matrix
## A is a matrix about the distance, the unkonwn dist is replaced by a big number.
admatrix<-function(A){
        source("floyd.R")
        ##"floyd.R" is other function creatde by myself which is shared in Github
        for(k in 1:28){
                for(l in 1:28){
                        A[k,l]<-floyd(A,k,l)
                }
        }
        B<-t(A)
        for(m in 1:28){
                for(n in 1:28){
                        if(A[m,n]>B[m,n]){
                                A[m,n]<-B[m,n]
                        }
                }
        }
        for(k in 1:28){
                for(l in 1:28){
                        A[k,l]<-floyd(A,k,l)
                }
        }
        return(A)
}