#当分层进行相合性检验得出各层都呈现正相合（或负相合）时，接下来对各层相合性程度进行检验
#即齐性检验
#x=c(741,882,2829,4945,453,248,1169,1032)  #课本133例5.3
#dim(x)=c(2,2,2);x

homogeneous=function(x){
  r=dim(x)[1];r    #行
  c=dim(x)[2];c    #列
  t=dim(x)[3];t    #层
  a=1/x[,1,1]+1/x[,1,2]+1/x[,2,1]+1/x[,2,2]
  theta=x[,1,1]*x[,2,2]/(x[,1,2]*x[,2,1])
  Eta=log(theta)
  Eta_bar=sum(Eta/a)/sum(1/a)
  chisq_T=sum((Eta-Eta_bar)^2/a)
  p_value=1-pchisq(chisq_T, (r-1))
  cat('\n 卡方统计量',chisq_T)
  cat('\n p_value ',p_value)
}



