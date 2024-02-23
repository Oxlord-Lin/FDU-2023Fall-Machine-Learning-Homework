# 准备工作
# 将列名转换为列索引
NameToCol <- function(colName){
  if (colName == '年龄'){
    return(2)
  }
  if(colName == '工作'){
    return(3)
  }
  if(colName == '房子'){
    return(4)
  }
  if(colName =='信贷情况'){
    return(5)
  }
  if(colName == '贷款审批'){
    return(6)
  }
}

# 计算熵的函数，data是数据集，C是分类指标（因变量）
H <- function(data,C){
  # print('开始计算熵')
  col.index = NameToCol(C)
  S = nrow(data)
  p = table(data[,col.index])/S
  # print(sum(-p*log(p)))
  return(sum(-p*log2(p)))
}

# 计算条件熵的函数，data是数据集，C是分类指标，A是特征
H.cond <- function(data,C,A){
  # print('开始计算条件熵')
  S = nrow(data)
  A.index = NameToCol(A)
  H = 0
  chara = unique(data[,A.index])
  # chara = as.character(chara[,1])
  for( i in 1:dim(chara)[1]){
   x = chara[i,1]
   x = as.character(x)
   # print(x)
   sub = data[(data[,A.index]==x),]  # 以特征A对data进行分类
   portion = nrow(sub) /S  # 该子类的占比
   # print(H(sub,C))
   H = H + portion*H(sub,C) 
  }
  return(H)
}

# 计算信息增益的函数，data是数据集，C是分类指标，A是特征
g <- function(data,C,A){
  # print('开始计算信息增益')
  gCA = H(data,C) - H.cond(data,C,A)
  # print(gCA)
  return(gCA)
}

# 计算信息增益比的函数，data是数据集，C是分类指标，A是特征
g.R <- function(data,C,A){
  # print('开始计算信息增益比')
  gr = g(data,C,A)/H(data,A)
  print(gr)
  return(gr)
}
  

library(readxl)
data <- read_excel("第二题数据.xlsx")


# 分别计算各个特征的信息增益比
A1 = '年龄'
A2 = '工作'
A3 = '房子'
A4 = '信贷情况'
C = '贷款审批'
gR1 = g.R(data,C,A1) # 0.0523719
gR2 = g.R(data,C,A2) # 0.3524465
gR3 = g.R(data,C,A3) # 0.4325381
gR4 = g.R(data,C,A4) # 0.2318539

# gR3的信息增益比最大，因此用A3(房子)作为分类指标
data.1 = subset(data,data$房子=='是') # 只有一类，data.1结束迭代
data.2 = subset(data,data$房子=='否')

# data.1中只有一类（贷款审批结果为“是”），data.1结束迭代
# 继续对data.2进行迭代
gR1 = g.R(data.2,C,A1) # 0.1644105
gR2 = g.R(data.2,C,A2) # 1
gR4 = g.R(data.2,C,A4) # 0.3403745

# gR2的信息增益比最大，因此用A2(工作)作为分类指标
data.2.1 = subset(data.2, data.2$工作=='是')
data.2.2 = subset(data.2, data.2$工作=='否')

# data.2.1与data.2.2都已只有一类，结束迭代！
# 展示分类结果
print(data.1)
#       ID 年龄  工作  房子  信贷情况 贷款审批
# 1     4 青年  是    是    一般     是      
# 2     8 中年  是    是    好       是      
# 3     9 中年  否    是    非常好   是      
# 4    10 中年  否    是    非常好   是      
# 5    11 老年  否    是    非常好   是      
# 6    12 老年  否    是    好       是 
print(data.2.1)
#       ID 年龄  工作  房子  信贷情况 贷款审批
# 1     3 青年  是    否    好       是      
# 2    13 老年  是    否    好       是      
# 3    14 老年  是    否    非常好   是 
print(data.2.2)
#       ID 年龄  工作  房子  信贷情况 贷款审批
# 1     1 青年  否    否    一般     否      
# 2     2 青年  否    否    好       否      
# 3     5 青年  否    否    一般     否      
# 4     6 中年  否    否    一般     否      
# 5     7 中年  否    否    好       否      
# 6    15 老年  否    否    一般     否 
