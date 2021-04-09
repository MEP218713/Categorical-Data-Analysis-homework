
#高维列联表独立性检验
mut_independence=function(x){
  r=dim(x)[1];r    #行
  c=dim(x)[2];c    #列
  t=dim(x)[3];t    #层
  n=sum(x)
  temp11=0
  temp21=0
  temp22=0
  temp23=0
  temp31=0
  temp32=0
  temp33=0
  for (i in 1:r) {
    for (j in 1:c){
      for (k in 1:t){
        ni__=sum(x[i,,])
        n_j_=sum(x[,j,])
        n__k=sum(x[,,k])
        n_jk=sum(x[,j,k])
        ni_k=sum(x[i,,k])
        nij_=sum(x[i,j,])
        temp11=temp11+x[i,j,k]*log(ni__*n_j_*n__k/(x[i,j,k]*n^2))
        
        temp21=temp21+x[i,j,k]*log(ni__*n_jk/(x[i,j,k]*n))
        temp22=temp22+x[i,j,k]*log(n_j_*ni_k/(x[i,j,k]*n))
        temp23=temp23+x[i,j,k]*log(n__k*nij_/(x[i,j,k]*n))
        
        temp31=temp31+x[i,j,k]*log(ni_k*nij_/(x[i,j,k]*ni__))
        temp32=temp32+x[i,j,k]*log(nij_*n_jk/(x[i,j,k]*n_j_))
        temp33=temp33+x[i,j,k]*log(ni_k*n_jk/(x[i,j,k]*n__k))
        
      }
    }
  }
  A_B_C=-2*temp11;A_B_C
  A_BC=-2*temp21;A_BC
  B_AC=-2*temp22;B_AC
  C_AB=-2*temp23;C_AB
  AB_AC=-2*temp31;AB_AC
  BA_BC=-2*temp32;BA_BC
  CA_CB=-2*temp33;CA_CB
  
  cat('\n(A,B,C) p值：',1-pchisq(A_B_C, r*c*t-r-c-t+2))
  cat('\n(A,BC)  p值：',1-pchisq(A_BC, (r-1)*(c*t-1)))
  cat('\n(B,AC)  p值：',1-pchisq(B_AC, (c-1)*(r*t-1)))
  cat('\n(C,AB)  p值：',1-pchisq(C_AB, (t-1)*(r*c-1)))
  cat('\n(AB,AC) p值：',1-pchisq(AB_AC, r*(c-1)*(t-1)))
  cat('\n(BA,AC) p值：',1-pchisq(BA_BC, c*(r-1)*(t-1)))
  cat('\n(CA,CB) p值：',1-pchisq(CA_CB, t*(r-1)*(c-1)))
}