library(readr)
data <- read_table("D:/大三上学习资料/统计（机器）学习/hw5/第一题数据.txt", col_names = FALSE)

data = as.matrix(data)
data = t(data)
colnames(data) = c('X1','X2','Y')

X1 = as.numeric(data[,1])
X2 = data[,2]
Y = as.numeric(data[,3])

# 计算先验概率的极大似然估计
PY = table(Y)/length(Y)
print(PY)

# 计算条件概率的极大似然估计
## X1的条件概率的MLE
PX1.cond = matrix(rep(0,2*3),nrow=2)
colnames(PX1.cond) = c('1','2','3')
rownames(PX1.cond) = c('-1','1')
for(i in 1:2){
  for(j in 1:3){
  PX1.cond[i,j] = sum((X1==j) * (Y==round(2*i-3)))/sum(Y==2*i-3)
  }}
print(PX1.cond)
# 计算X2的条件概率MLE
PX2.cond = matrix(rep(0,2*3),nrow=2)
X2.type = c('S','M','L')
colnames(PX2.cond) = X2.type
rownames(PX2.cond) = c('-1','1')
for(i in 1:2){
  for(j in 1:3){
    PX2.cond[i,j] = sum((X2==X2.type[j]) * (Y==round(2*i-3)))/sum(Y==2*i-3)
  }}
print(PX2.cond)

# 计算x = (2, M)' 的类标记
## 对于Y = -1
p_neg1 = PY[1]*PX1.cond[1,2]*PX2.cond[1,2]
print(p_neg1) # 0.08
## 对于Y = 1
p_pos1 = PY[2]*PX1.cond[2,2]*PX2.cond[2.2]
print(p_pos1) # 0.04
## 结论：Y = -1

# 计算条件概率的贝叶斯估计
## X1的条件概率的贝叶斯估计
PX1.Bayes = matrix(rep(0,2*3),nrow=2)
colnames(PX1.cond) = c('1','2','3')
rownames(PX1.cond) = c('-1','1')
for(i in 1:2){
  for(j in 1:3){
    PX1.Bayes[i,j] = (sum((X1==j) * (Y==round(2*i-3)))+1)/(sum(Y==2*i-3)+3)
  }}
print(PX1.Bayes)
# 计算X2的条件概率的贝叶斯估计
PX2.Bayes = matrix(rep(0,2*3),nrow=2)
X2.type = c('S','M','L')
colnames(PX2.Bayes) = X2.type
rownames(PX2.Bayes) = c('-1','1')
for(i in 1:2){
  for(j in 1:3){
    PX2.Bayes[i,j] = (sum((X2==X2.type[j]) * (Y==round(2*i-3))) +1)/(sum(Y==2*i-3)+3)
  }}
print(PX2.Bayes)
# 计算x = (2, M)' 的类标记
## 对于Y = -1
p_neg1 = PY[1]*PX1.Bayes[1,2]*PX2.Bayes[1,2]
print(p_neg1) # 0.0625
## 对于Y = 1 # 0.04733728 
p_pos1 = PY[2]*PX1.Bayes[2,2]*PX2.Bayes[2.2]
print(p_pos1)
## 结论：Y = -1
