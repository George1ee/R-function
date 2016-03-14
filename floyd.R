floyd<-function(a,sb,db){
n<-length(a[1,])
mypath<-matrix(rep(0,n*n),nrow = n,ncol = n)
for(k in 1:n){
        for(i in 1:n){
                for(j in 1:n){
                        if(a[i,j]>a[i,k]+a[k,j]){
                                a[i,j]=a[i,k]+a[k,j]
                                mypath[i,j]=k
                        }
                }
        }
}
if(sb!=db){
        mydist=a[sb,db]
}else{
        mydist=0
}
return(mydist)
}
