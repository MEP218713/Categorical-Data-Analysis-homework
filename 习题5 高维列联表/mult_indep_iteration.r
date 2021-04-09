
#��ά����������Լ��飬�������
mult_indep_iteration=function(x){
  r=dim(x)[1];r    #��
  c=dim(x)[2];c    #��
  t=dim(x)[3];t    #��
  n=sum(x)
  m0=rep(1,r*c*t);m0
  dim(m0)=c(r,c,t);m0  #��ʼ��m
  
  temp1=m0
  temp1[temp1>0]=0;temp1 #��ʼ���м��������ֵȫΪ0
  temp2=temp1
  temp3=temp1
  d=1  #��ʼ�����ֵ
  f=1 #��������
  while (d>0.1) {
    for (i in 1:r) {
      for (j in 1:c){
        for (k in 1:t){
          ni__=sum(x[i,,])
          n_j_=sum(x[,j,])
          n__k=sum(x[,,k])
          n_jk=sum(x[,j,k])
          ni_k=sum(x[i,,k])
          nij_=sum(x[i,j,])
          temp1[i,j,k]=nij_*m0[i,j,k]/sum(m0[i,j,])
        }
      }
    }
    for (i in 1:r) {
      for (j in 1:c){
        for (k in 1:t){
          ni__=sum(x[i,,])
          n_j_=sum(x[,j,])
          n__k=sum(x[,,k])
          n_jk=sum(x[,j,k])
          ni_k=sum(x[i,,k])
          nij_=sum(x[i,j,])
          temp2[i,j,k]=ni_k*temp1[i,j,k]/sum(temp1[i,,k])
        }
      }
    }
    for (i in 1:r) {
      for (j in 1:c){
        for (k in 1:t){
          ni__=sum(x[i,,])
          n_j_=sum(x[,j,])
          n__k=sum(x[,,k])
          n_jk=sum(x[,j,k])
          ni_k=sum(x[i,,k])
          nij_=sum(x[i,j,])
          temp3[i,j,k]=n_jk*temp2[i,j,k]/sum(temp2[,j,k])
        }
      }
    }
    cat('\n��',f,'�ε���\n')
    f=f+1
    d=sum((m0-temp3)^2)
    m0=temp3
    print(m0)
  }
  likelihood_T=-2*sum(x*log(m0/x))
  cat('\n ��Ȼ�ȼ���ͳ������',likelihood_T)
  cat('\n p-value��      ',1-pchisq(likelihood_T, (r-1)*(c-1)*(t-1)))
  
  chisq_T=sum((x-m0)^2/m0)
  cat('\n Pearson��������ͳ������',chisq_T)
  cat('\n p-value��           ',1-pchisq(chisq_T, (r-1)*(c-1)*(t-1)))
  
}





