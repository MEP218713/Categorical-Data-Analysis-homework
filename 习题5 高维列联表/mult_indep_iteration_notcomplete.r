#高维不完备列联表独立性检验，迭代求解
mult_indep_iteration_notcomplete=function(x){
x=c(6,7,17,0,3,10,0,0,13,8,3,36,0,1,5,0,0,10)
dim(x)=c(3,3,2);x
  r=dim(x)[1];r    #行
  c=dim(x)[2];c    #列
  t=dim(x)[3];t    #层
  n=sum(x)
  m0=rep(0,r*c*t);m0
  dim(m0)=c(r,c,t);m0  #初始化m
  temp1=m0;temp1  #初始化中间变量矩阵，值全为0
  temp2=temp1
  temp3=temp1
  m0[x>0]=1;m0
  
  d=1  #初始化误差值
  f=1 #迭代次数
  while (d>0.1) {
    for (i in 1:r) {
      for (j in 1:c){
        for (k in 1:t){
          nij_=sum(x[i,j,])
          if(sum(m0[i,j,])!=0){
            temp1[i,j,k]=nij_*m0[i,j,k]/sum(m0[i,j,])
          } 
        }
      }
    }
    for (i in 1:r) {
      for (j in 1:c){
        for (k in 1:t){
          ni_k=sum(x[i,,k])
          if(sum(temp1[i,,k])!=0){
            temp2[i,j,k]=ni_k*temp1[i,j,k]/sum(temp1[i,,k])
          }
        }
      }
    }
    for (i in 1:r) {
      for (j in 1:c){
        for (k in 1:t){
          n_jk=sum(x[,j,k])
          if(sum(temp2[,j,k])!=0){
            temp3[i,j,k]=n_jk*temp2[i,j,k]/sum(temp2[,j,k])
          }
        }
      }
    }
    cat('\n第',f,'次迭代\n')
    f=f+1
    d=sum((m0-temp3)^2)
    m0=temp3
    print(m0)
  }
  chisq_T=sum((x[x>0]-m0[m0>0])^2/(m0[m0>0]));z  #卡方统计量
  p_value=1-pchisq(chisq_T,r*c*t-r-c-t+2-length(x[x==0]));p_value
  cat('\n Pearson卡方检验统计量：',chisq_T)
  cat('\n p-value：           ',p_value)
  
  likelihood_T=-2*sum(x[x>0]*log(m0[m0>0]/x[x>0]))
  p_value=1-pchisq(likelihood_T,r*c*t-r-c-t+2-length(x[x==0]));p_value
  cat('\n 似然比检验统计量：',likelihood_T)
  cat('\n p-value：      ',p_value)
}

  






