#第一题:压缩列联表
A1=matrix(c(5,33,36,645),nrow=2)
A2=matrix(c(10,19,58,518),nrow=2)
A=A1+A2
chisq.test(A,correct = F)    #独立性检验
#p-value =  1.516e-05<0.001,拒绝原假设，即相关
chisq.test(A1,correct = F)    #分层独立性检验
chisq.test(A2,correct = F)    #分层独立性检验
source('congruence_test.r')  #相合性检验
congruence_test(A,alternative="greater")#拒绝原假设，认为是正相合
congruence_test(A1,alternative="greater")#拒绝原假设，认为是正相合
congruence_test(A2,alternative="greater")#拒绝原假设，认为是正相合

#第四题
#x=c(741,882,2829,4945,453,248,1169,1032)  #课本133例5.3
x=c(22,16,2,19,17,4,19,61,20,15,72,13)
dim(x)=c(3,2,2);x
source('mut_independence.r')#高维列联表独立性检验
mut_independence(x)
#(B,AC)  p值： 0.57733>0.1  不独立，B与AC相关


#第五题
source('mult_indep_iteration.r')
x=c(741,882,2829,4945,453,248,1169,1032)
dim(x)=c(2,2,2);x
mult_indep_iteration(x)


#第六题
x=c(37,11,26,23,30,31,43,11)
dim(x)=c(2,2,2);x
source('mut_independence.r')#高维列联表独立性检验
mut_independence(x)        
#各个独立性检验的p值都很小，故这三个属性相互之间仅有相关关系
#下对年龄进行分层，讨论相合性
A1=x[,,1]
A2=x[,,2]
source('congruence_test.r')  #相合性检验
congruence_test(A1,alternative="greater")
#拒绝原假设，认为是正相合，即年轻人中，男性偏好饮料A，女性偏好饮料B
congruence_test(A2,alternative="less")
#拒绝原假设，认为是负相合，及老年人中，男性偏好饮料B，女性偏好饮料A


#第七题
x=c(2368,123,131,81,293,1247,3,255,307,359,12,75)
dim(x)=c(2,2,3);x
source('mut_independence.r')#高维列联表独立性检验
mut_independence(x) 
#各个独立性检验的p值都很小，故这三个属性相互之间仅有相关关系
A1=x[,,1]   #王主任
A2=x[,,2]   #张主任
A3=x[,,3]   #李主任
A=A1+A2+A3  #按主任合并，压缩列联表
source('congruence_test.r')  #相合性检验
congruence_test(A1,alternative="greater")
#拒绝原假设，认为是正相合
congruence_test(A2,alternative="greater")
#拒绝原假设，认为是正相合
congruence_test(A3,alternative="greater")
#拒绝原假设，认为是正相合
congruence_test(A,alternative="greater")
#拒绝原假设，认为是正相合，即内销产品合格率相对更高，而外销产品的不合格率相对更高
#故产品类别(内销_外销)是评判各主任不及格率的混杂因素，将内销和外销合并后作比较是【有偏比较】
#由于存在混杂因素，以下根据内销、外销分层进行比较
B1=x[1,,];B1
B2=x[2,,];B2
congruence_test(B1,alternative="less")
#拒绝原假设，认为是负相合
#即在内销方面，李主任的合格率相对最高，张主任次之，而王主任合格率最低
congruence_test(B2,alternative="less")
#拒绝原假设，认为是负相合
#即在外销方面，李主任的合格率相对最高，张主任次之，而王主任合格率最低


#第八题
c(1921.4,44963,33959.9,924.3)/c(563254,1865556,695114,101112)
#未婚的死亡率最低，其次是离婚、有配偶，而丧偶的死亡率最高
#这样的比较是有偏比较，混杂因素是年龄
live=c(318869,135589,52427,21781,9242,6441,5847,4343,3423,5292,
      973796,1495515,1354019,926994,603685,562182,680892,536601,377263,354609,
      944,2738,5383,9703,11666,24099,57015,89970,123005,372391,
      6165,16333,17901,12343,7870,7191,9103,8768,7615,7820)
die=c(349.1,329.2,213.9,127.5,68.7,86.4,99.4,92.9,119.5,434.8,
      417.1,877.8,1268.8,1299.2,1357.1,2107.4,4255.2,5868.7,7240.7,20271,
      4.1,11.6,16,31.5,45.4,130.1,446.1,1082.7,2351.4,29842,
      11.4,25.5,26,27.9,26.3,38.3,77.4,117.6,159.6,414.3)
all=live+die
x=c(live,die)
dim(x)=c(10,4,2);x   #构造年龄*婚姻*死亡 的列联表
dim(all)=c(10,4);all #消去死亡因素，压缩为二维列联表（年龄*婚姻）
source('mut_independence.r')#高维列联表独立性检验
mut_independence(x) 
#各个独立性检验的p值都很小，故这三个属性相互之间仅有相关关系
source('congruence_test.r')  #相合性检验
congruence_test(all,alternative="greater")
#拒绝原假设，认为是正相合,即为有偏抽样，各年龄段的不同婚姻人数差异较大
#若将各年龄组的居民人数和死亡人数合并在一起，再计算各类婚姻状况的死亡率，这样的比较为有偏比较。
da=die/all
install.packages("pheatmap")
library(pheatmap)
pheatmap(da,cluster_row = FALSE,cluster_col=FALSE)  #数据可视化
#从图像可以看出，随着年龄的增长，未婚的死亡率最高，其次是丧偶、离婚，最低的是有配偶


#第9题 四维列联表转化为三维列联表
x=c(99,35,161,237,92,41,141,265,358,35,107,27,374,40,113,32)
dim(x)=c(2,2,4);x
source('mut_independence.r')#高维列联表独立性检验
mut_independence(x) 
#各个独立性检验的p值都很小，故这三个属性相互之间仅有相关关系
A1=x[,,1]   #40-49男
A2=x[,,2]   #40-49女
A3=x[,,3]   #20-29男
A4=x[,,4]   #20-29女
A=A1+A2+A3+A4  #压缩列联表
source('congruence_test.r')  #相合性检验
congruence_test(A1,alternative="greater")
#拒绝原假设，认为是正相合
congruence_test(A2,alternative="greater")
#拒绝原假设，认为是正相合
congruence_test(A3,alternative="greater")
#拒绝原假设，认为是正相合
congruence_test(A4,alternative="greater")
#拒绝原假设，认为是正相合
congruence_test(A,alternative="greater")     #检验条件相合性
#拒绝原假设，认为无论子女年龄、性别如何，父亲在世和死亡与母亲在世和死亡都有正相合的关系

#第二问，消去年龄，压缩列联表为（性别,母亲生死,父亲生死）
B1=A1+A3
B2=A2+A4
B=c(B1,B2)
dim(B)=c(2,2,2)
source('congruence_test.r')  #相合性检验
congruence_test(B1,alternative="greater")
#拒绝原假设，认为是正相合
congruence_test(B2,alternative="greater")
#拒绝原假设，认为是正相合
source('homogeneous.r')
homogeneous(B)
#p_value=0.8334939>0.05,接受原假设
#即认为[子女为男的父母生死的相合性]与[子女为女的父母生死的相合性]程度没有差异

#第三问，消去性别，压缩列联表为（年龄,母亲生死,父亲生死）
C1=A1+A2
C2=A3+A4
C=c(C1,C2)
dim(C)=c(2,2,2)
congruence_test(C1,alternative="greater")
#拒绝原假设，认为是正相合
congruence_test(C2,alternative="greater")
#拒绝原假设，认为是正相合
source('homogeneous.r')
homogeneous(C)
#p_value=0.05608898>0.05,接受原假设
#即认为[子女年轻的父母生死的相合性]与[子女年长的父母生死的相合性]程度没有差异


#第10题  暂时无解
x=c(6,7,17,0,3,10,0,0,13,8,3,36,0,1,5,0,0,10)
dim(x)=c(3,3,2);x









