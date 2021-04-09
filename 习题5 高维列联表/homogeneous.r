#���ֲ��������Լ���ó����㶼��������ϣ�����ϣ�ʱ���������Ը�������Գ̶Ƚ��м���
#�����Լ���
#x=c(741,882,2829,4945,453,248,1169,1032)  #�α�133��5.3
#dim(x)=c(2,2,2);x

homogeneous=function(x){
  r=dim(x)[1];r    #��
  c=dim(x)[2];c    #��
  t=dim(x)[3];t    #��
  a=1/x[,1,1]+1/x[,1,2]+1/x[,2,1]+1/x[,2,2]
  theta=x[,1,1]*x[,2,2]/(x[,1,2]*x[,2,1])
  Eta=log(theta)
  Eta_bar=sum(Eta/a)/sum(1/a)
  chisq_T=sum((Eta-Eta_bar)^2/a)
  p_value=1-pchisq(chisq_T, (r-1))
  cat('\n ����ͳ����',chisq_T)
  cat('\n p_value ',p_value)
}



