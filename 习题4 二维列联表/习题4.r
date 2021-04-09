#第一题 独立性检验
x<-matrix(c(90,170,135,3,18,6,7,7,9),nrow=3)
chisq.test(x,correct = F)
#p-value = 0.1027>0.1,接受原假设，即供应商与零件质量独立


#书本80页例题
congruence_test=function(x,alternative="twoside")
  #适用于列联表的相合性检验问题
  #x为列联表矩阵；alternative对应于备择假设
{
  n=sum(x)
  G=0;H=0
  r=nrow(x)
  c=ncol(x)
  r1=r-1;c1=c-1
  for (i in 1:r1){
    for (j in 1:c1){
      G=G+x[i,j]*sum(x[(i+1):r,(j+1):c])
    }
  }
  for (i in 1:r1){
    for (j in 2:c){
      H=H+x[i,j]*sum(x[(i+1):r,1:(j-1)])
    }
  }
  z=G-H
  TA=sum(rowSums(x)*(rowSums(x)-1)/2)
  TB=sum(colSums(x)*(colSums(x)-1)/2)
  #TAB=G+H+TA+TB-n*(n-1)/2
  Cn2=n*(n-1)/2
  #计算各系数的值
  Kendall_TAO=z/sqrt((Cn2-TA)*(Cn2-TB))
  Gamma=(G-H)/(G+H)
  d_BA=(G-H)/(Cn2-TA)
  d_AB=(G-H)/(Cn2-TB)
  
  #近似公式,表示sigma的平方
  sigma_2=(n^3-sum(rowSums(x)^3))*(n^3-sum(colSums(x)^3))/(9*n^3) 
  
  #构建U统计量
  U=z/sqrt(sigma_2)
  if(alternative=="twoside")
  {p_value=1-pchisq(U^2, 1)}
  else 
  {
    if(alternative=="greater")
    {p_value=pnorm(-U)}
    else if(alternative=="less")
    {p_value=pnorm(U)}
    else{cat("please input:\n alternative= 'twoside','greater',or'less'")}
  }
  cat('【各种相关系数】\n')
  cat('Kendall_TAO=',Kendall_TAO,'\n')
  cat('Gamma=',Gamma,'\n')
  cat('d_BA=',d_BA,'\n')
  cat('d_AB=',d_AB,'\n\n')
  cat('【相合性检验】\n')
  cat('U检验统计量的值',U,'\n')
  cat('p_value=',p_value)
}

x<-matrix(c(195,20,26,93,27,39,34,27,39),nrow=3);x
congruence_test(x)

#第4题
x<-matrix(c(352,293,284,133,717,210),nrow=2);x
congruence_test(x)
#p_value趋近于0，且U<0,负相合




consistency_test=function(x)
  #适用于列联表的一致性检验问题
  #H0:偶然一致 <-> H1:不是偶然一致的，即相对一致  
  #x为方表矩阵（行列必须相等）
{
  n=sum(x)
  q1=sum(diag(x))/n
  q2=sum(rowSums(x)*colSums(x))/(n*n)
  Kappa=(q1-q2)/(1-q2)     #计算Kappa系数
  if(Kappa<=0){cat('由于Kappa=',Kappa,'<=0，无需计算p-value\n')
    cat('检验结果是偶然一致的')
  }else{    
    #只有当Kappa>0时，才进行一致性检验
    var_Kappa=(q2+q2^2-sum(rowSums(x)*colSums(x)*(rowSums(x)+colSums(x)))/(n*n*n))/((n-1)*(1-q2)^2)
    sd_Kappa=sqrt(var_Kappa)
    U=Kappa/sd_Kappa 
    p_value=pnorm(-U)
    cat('【Kappa参考值】\n')
    cat('0.0~0.2  极低的一致性(slight)\n')
    cat('0.21~0.4 一般的一致性(fair)\n')
    cat('0.41~0.6 中等的一致性(moderate)\n')
    cat('0.61~0.8 高度的一致性(substantial)\n')
    cat('0.81~1   几乎完全一致(almost perfect)\n')
    return(list(Kappa=Kappa,U=U,p_value=p_value))
    }
}

x<-matrix(c(17,5,10,4,12,3,8,0,13),nrow=3)
consistency_test(x)


#第7题
x<-matrix(c(25,1,1,3,9,2,1,0,15),nrow=3)
consistency_test(x)
#p_value<0.001,拒绝原假设，不是偶然一致的




x<-matrix(c(0,349,114,395,0,320,159,447,0),nrow=3);x
#x<-matrix(c(1413,0,1029,548,2240,1287,0,346),nrow=2);x
rsum=rowSums(x);rsum
csum=colSums(x);csum
m=matrix(rep(0,nrow(x)*ncol(x)),nrow=nrow(x))
m[x>0]=1;m
d=1
k=1
while(d>0.01){
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
















