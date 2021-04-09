congruence_test=function(x,alternative="twoside")
  #适用于列联表的相合性检验问题
  #x为列联表矩阵；alternative对应于备择假设'twoside'相合,'greater'正相合,or'less'负相合
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
