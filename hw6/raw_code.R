library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(adabag)
library(pROC)
#导入数据
library(readr)
data <- read_csv("D:/大三上学习资料/统计（机器）学习/hw6/lc_data.csv")
data = as.data.frame(data)
# 对某些列重新命名，否则之后做随机森林会出问题
newNames <- c( "linked_list","math","hash_table","string","binary_search",
"divide_and_conquer", "dynamic_programming","greedy","two_pointers","sorting",                 
"backtracking","heap_priority_queue", "simulation","depth_first_search",      
"binary_tree","breadth_first_search","union_find","graph",                   
"geometry","database","topological_sort","prefix_sum","shortest_path" )
colnames(data)[21:43] <- newNames

# 创建EASY变量
data$EASY = 1*(data$difficulty=='EASY')
# 对分类变量设置基准组
# 其中EASY, quesclass, example的基准组分别设置为0、题库、1
data$EASY = factor(data$EASY)
data$EASY = relevel(data$EASY,'0')
data$quesClass = factor(data$quesClass)
data$quesClass = relevel(data$quesClass,'题库')
data$example = factor(data$example,levels = c(1,2,3,4,5,6))
data$difficulty = factor(data$difficulty,levels=c('EASY','MEDIUM','HARD'))
data$ifcom = factor(data$ifcom)
# 分类标签统一变成0/1变量
data$Algorithms = data$Algorithms>0
data$Common.Data.Structures = data$Common.Data.Structures>0
data$Advanced.Data.Structures = data$Advanced.Data.Structures>0
data$Techniques = data$Techniques>0
data$Math = data$Math>0
data$Other = data$Other>0
# # 最后将分类变量统一变成因子型数据
# cols = c(5:12,20:45,48)
# for(i in cols){
#   data[,i] = factor(data[,i])
# }
# data = na.omit(data)
  
# 描述性分析
## 三种不同题目难度占比分布直方图
difficult.table = table(data$difficulty)/nrow(data)
barplot(difficult.table,main='不同题目难度占比分布',xlab = '题目难度',ylab = '占比')
## 不同题目难度的题目要求文本长度箱线图
boxplot(requireLen~difficulty,data=data,col=c('aliceblue','seashell','pink'),xlab='题目难度',ylab='要求文本长度',cex.lab=1,cex.axis=1,las=1)
## 原始数据右偏，效果不好，于是取对数
boxplot(log(requireLen)~difficulty,data=data,col=c('aliceblue','seashell','pink'),xlab='题目难度',ylab='要求文本长度（取对数）',cex.lab=1,cex.axis=1,las=1)
## 不同题目难度的题目给出的示例数量占比柱状图
subset.EASY = subset(data,difficulty=='EASY')
subset.MEDIUM = subset(data,difficulty=='MEDIUM')
subset.HARD = subset(data,difficulty=='HARD')
difficult.example.table = rbind(table(subset.EASY$example)/nrow(subset.EASY),table(subset.MEDIUM$example)/nrow(subset.MEDIUM),table(subset.HARD$example)/nrow(subset.HARD))
rownames(difficult.example.table) = c('EASY','MEDIUM','HARD')
barplot(difficult.example.table, beside = T, col = c('aliceblue','seashell','pink'), main = "不同难度题目的示例数量占比", xlab = '示例数量',ylab='占比',ylim = c(0,0.5))
legend("topright", legend = rownames(difficult.example.table), fill =  c('aliceblue','seashell','pink'), cex = 0.8)
## 不同公司的题目数量柱状图
companies = c("huawei","bytedance","microsoft","google","amazon","tencent","otherCom")
company.problem.table = numeric(7)
for(i in 1:7){
  company = companies[i]
  company.problem.table[i] = sum(data[company])
}
barplot(company.problem.table,names.arg=companies,col='lightgreen',las=1,main='不同公司的题目数量',cex.names = 0.75)
# 对不同的分类标签，绘制简单题所占的比例直方图
label = c("Algorithms","Common.Data.Structures","Advanced.Data.Structures","Techniques","Math","Other")
label.easy.table = numeric(6)
for(i in 1:6){
  s = subset(data,data[label[i]]!=0)
  print(label[i])
  label.easy.table[i] = nrow(subset(s,EASY==1))/nrow(s)
}
label.easy.table = as.data.frame(label.easy.table)
perf = ggplot(data=label.easy.table,aes(x=label, y=label.easy.table))+
  geom_bar(stat='identity',fill="lightgreen")+ylab('占比')+xlab('分类标签')+theme_light()+
  theme(axis.text.x = element_text(angle=60, hjust=1,size=8),
        plot.title = element_text(color="black", size=15, face="bold",hjust=0.5),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"))+
        ggtitle('不同分类标签的简单题占比')
perf

# 建模数据预处理
## 重采样，使用自定义函数
set.seed(2023)
subset.NOT.EASY = subset(data,EASY==0)
nsample = sample(x = dim(subset.NOT.EASY)[1], 
                 size = dim(subset.NOT.EASY)[1]*0.380304, replace = F)
subset.NOT.EASY.sample = subset.NOT.EASY[nsample, ]
data.balanced = rbind(subset.EASY,subset.NOT.EASY.sample)
table(data.balanced$EASY) # 展示平衡数据

## 分割
set.seed(2023)
## 有一些属性在接下来的训练中不再使用，先删掉: difficulty,require,tips,Fundamentals
data.balanced = data.balanced[,-c(6,45,46,47)]
nsample = sample(x = dim(data.balanced)[1], size = dim(data.balanced)[1]*0.7, replace = F)
## 重新划分训练集和测试集
data.train = data.balanced[nsample, ] 
data.test = data.balanced[-nsample, ]


# 建模
## 决策树
### 建立CART决策树模型，找出具有较高的相对重要性的变量
result.rp = rpart(EASY~.,data = data.train,method='class')
importance = summary(result.rp)$variable.importance
opar <- par(mfrow=c(1,1),mar=c(8, 4, 2, 0.5))
barplot(importance,las=2,cex.names = 0.6,main='决策树模型的变量重要性')
rpart.plot(result.rp,type=2)
## 随机森林
result.rf <- randomForest(EASY~., data = data.train, importance=TRUE,type='classification',ntree=1000)
varImpPlot(result.rf,main='随机森林模型的变量重要性',cex=0.7)

## adaboost
result.adaboost <- boosting(EASY~., data=data.train, boos=F,mfinal = 20)
importanceplot(result.adaboost,las=2,cex.name=0.7)

## 综合比较
pred.rp = predict(result.rp, data.test,type='prob')
pred.rf = predict(result.rf,data.test,type='prob')
pred.adaboost = predict(result.adaboost,data.test,type='prob')
# 绘制ROC曲线
opar <- par(mfrow=c(1,3))
plot.roc(data.test$EASY, pred.rp[,2], 
         col = "royalblue2", print.auc=TRUE,
         auc.polygon=TRUE, auc.polygon.col="aliceblue", 
         xlab = "FPR",ylab = "TPR", main = "决策树 ROC曲线",cex.lab=1.2)  

plot.roc(data.test$EASY, pred.rf[,2], 
         col = "royalblue2", print.auc=TRUE,
         auc.polygon=TRUE, auc.polygon.col="aliceblue", 
         xlab = "FPR",ylab = "TPR", main = "随机森林 ROC曲线",cex.lab=1.2)  

plot.roc(data.test$EASY, pred.adaboost$prob[,2], 
         col = "royalblue2", print.auc=TRUE,
         auc.polygon=TRUE, auc.polygon.col="aliceblue", 
         xlab = "FPR",ylab = "TPR", main = "adaboost ROC曲线",cex.lab=1.2) 

# 另一种方式
roc.rp <- roc(data.test$EASY, pred.rp[,2])
roc.rf <- roc(data.test$EASY, pred.rf[,2])
roc.adaboost <- roc(data.test$EASY, pred.adaboost$prob[,2])
ggroc(roc.rf)
ggroc(roc.rf)
ggroc(roc.adaboost)
