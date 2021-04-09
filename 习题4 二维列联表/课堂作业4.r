
x<-matrix(c(0,349,114,395,0,320,159,447,0),nrow=3);x
rsum=rowSums(x)
csum=colSums(x)
m=matrix(rep(0,nrow(x)*ncol(x)),nrow=nrow(x))
m[x>0]=1
d=1
k=1
while(d>0.1){
  cat('\n第',k,'次迭代\n')
  k=k+1
  temp=m
  temp[m>=0]=0;temp
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      temp[i,j]=m[i,j]*rsum[i]/rowSums(m)[i]
    }
  }
  m=temp
  
  temp1=m
  temp1[m>=0]=0
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      temp1[i,j]=m[i,j]*csum[j]/colSums(m)[j]
    }
  }
  d=sum((m-temp1)^2)
  m=temp1
  
  print(m)
}
m


z=sum((x[x>0]-m[m>0])^2/(m[m>0]));z  #卡方统计量
p_value=1-pchisq(z,(nrow(x)-1)*(ncol(x)-1)-length(x[x==0]));p_value
#p_value>0.1,接受原假设，即接受拟独立性
#这意味着如果儿子没有继承父亲的社会地位，则他的社会地位不受到父亲社会地位的影响















