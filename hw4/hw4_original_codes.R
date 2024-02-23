
# Q1

# 导入数据
data = read.csv("D:/大三上学习资料/统计（机器）学习/hw4/files/Data_Cleaning.csv", 
                 header = T, stringsAsFactors = F) 
# 排序，便于查看
data = data[order(data$index_origin),]  

# 仅保留录取与拒绝两种结果所对应的数据记录，并将AD和offer归为一类
data = data[which(data$offertype!='WaitingList'),]
data[which(data$offertype %in% c('AD无奖', 'AD小奖','Offer')),]$offertype = '录取'
data[which(data$offertype %in% c('Rej')),]$offertype = '拒绝'

# 对申请学校进行简单修正
data[which(data$college_apply %in% c('Texas A', 'M University')),]$college_apply = 'Texas A&M University'
data[which(data$college_apply %in% c('Washington University in St', 'Louis')),]$college_apply = 'Washington University in St. Louis'


# 导入缩写
abb = read.table("D:/大三上学习资料/统计（机器）学习/hw4/files/美国大学缩写汇总.txt", 
                 quote="\"", comment.char="")
colnames(abb) = c('fullName','abbName')
# 将学校缩写替换为全称，不考虑缩写字母的大小写差异
data$college_apply = toupper(data$college_apply) # 全部变成大写字母
abb$abbName = toupper(abb$abbNam)
for(i in 1:dim(data)[1]){
  if(data$college_apply[i]%in%abb$abbName) # 先确认是一个缩写才进行转换
  data$college_apply[i] = abb$fullName[which(abb$abbName==data$college_apply[i])]
}
data$college_apply = factor(data$college_apply)

# 导入QS前百名的美国大学名单
top = read.delim("D:/大三上学习资料/统计（机器）学习/hw4/files/QS大学排名前百（美国）.txt",header=F)
# 将所有申请学校划分为”Top50”和”Others”两类
data$CollegeRankTop50 = data$college_apply %in% top$V1
data[which(data$CollegeRankTop50==T),]$CollegeRankTop50 = 'Top50'
data[which(data$CollegeRankTop50==F),]$CollegeRankTop50 = 'Others'
data$CollegeRankTop50 = factor(data$CollegeRankTop50,levels = c('Top50','Others'))

# 首先把gpa_measure填写错误的数据删掉
data = data[which(data$gpa_measure %in% c(4,4.3,100)),]
# 对绩点进行离散化处理
# 共有 '<=3.4', '3.4-3.55', '3.55-3.7', '>3.7'四类
data$gpa_dis = NA # 新建一列
for(i in 1:dim(data)[1]){
  GPA = 4*data$gpa[i]/data$gpa_measure[i]
  if(is.na(GPA)) {
    data$gpa_dis[i] = NA}
  else if(GPA <= 3.4){
    data$gpa_dis[i]='<=3.4'}
  else if(GPA>3.4 && GPA<=3.55){
    data$gpa_dis[i] = '3.4-3.55'}
  else if(GPA>3.55 && GPA<=3.7){
    data$gpa_dis[i] = '3.55-3.7'}
  else{
    data$gpa_dis[i] = '>3.7'}
}
data$gpa_dis = factor(data$gpa_dis,levels=c('<=3.4', '3.4-3.55', '3.55-3.7', '>3.7'))
# 对托福成绩进行离散化处理
# '<=98', '98-102', '102-106', '>106'
data$toefl = as.numeric(data$toefl)
data$toefl_dis = NA
for(i in 1:length(data$toefl_dis)){
  TOF = data$toefl[i]
  if(is.na(TOF))
    {data$toefl_dis[i]=NA}
  else if(TOF<=98)
    {data$toefl_dis[i]='<=98'}
  else if(TOF>98 && TOF<=102)
    {data$toefl_dis[i]='98-102'}
  else if(TOF>102 && TOF<=106)
    {data$toefl_dis[i]='102-106'}
  else 
    {data$toefl_dis[i]='>106'}
}
data$toefl_dis = factor(data$toefl_dis,levels = c('<=98', '98-102', '102-106', '>106'))

# 删除含有缺失值的数据
data = na.omit(data)

# Q2
# Top 10热门学校申请人数柱状图，并在图中分别展示不同录取情况的人数
library(ggplot2)
t = table(data$college_apply)
t = t[order(t,decreasing = T)]
t = as.data.frame(t[1:10])
colnames(t) = c('college_apply','count')
perf = ggplot(data=t, aes(x=college_apply, y=count))+
  geom_bar(stat="identity",fill="royalblue")+ylab('人数')+xlab('大学')+theme_bw()+
  theme(axis.text.x = element_text(angle=60, hjust=1,size=9),
        plot.title = element_text(color="black", size=15, face="bold",hjust=0.5),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"))+
  geom_text(aes(label = count), vjust = 0,size=3.2)+ggtitle('Top 10 热门学校申请人数')
perf

# 托福成绩与录取结果箱线图
box <- ggplot(data, aes(x = CollegeRankTop50, y = toefl, fill = offertype)) +
    geom_boxplot(show.legend = T, varwidth = T) +theme_light() +
    scale_fill_manual("申请结果", values = c("pink", "royalblue")) +    # 按照申请结果填色
  theme(axis.text.x = element_text(angle=0, hjust=1,size=9),
        plot.title = element_text(color="black", size=15, face="bold",hjust=0.5),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"))+
    scale_y_continuous(limits = c(70, 120),breaks = seq(70, 120, by = 5))+ 
    labs(x = "申请学校的世界排名", y = "托福成绩", title = "托福成绩与申请结果")
box

# 申请结果分布饼图
library(plotly)
t2 = table(data$offertype)
t2 = as.data.frame(t2)
colnames(t2) = c('offertype','count')
plot_ly(t2,values= ~count,labels= ~offertype,type='pie',title='申请结果分布')


# 分析申请人数和申请率变化的关系
# 申请通过率随时间的变化情况

library(dplyr)
library(magrittr)
library(ggplot2)
offer_rate = data%>%group_by(time)%>%summarise(rate=mean(offertype=='录取'))
offer_rate = offer_rate[4:8,] # 删去数据量太少的年份
offer_rate$rate = (offer_rate$rate/(1-offer_rate$rate))  # 转化为优势比
offer_rate$time = offer_rate$time + 2*1e3
p = ggplot(offer_rate,mapping=aes(x=time,y=rate))
of = p + geom_line(col='pink2',linewidth=1)+geom_point(size=2,col='red',alpha=0.6)+theme_light()+
  theme(axis.text.x = element_text(angle=0, hjust=1,size=9),
                     plot.title = element_text(color="black", size=15, face="bold",hjust=0.5),
                     axis.title.x = element_text(color="black", size=12, face="bold"),
                     axis.title.y = element_text(color="black", size=12))+ 
  labs(x = "年份", y = "录取通过率的优势比ODD", title = "录取率优势比ODD随年份的变化")

# 申请人数随年份变化情况
population = data%>%group_by(time)%>%summarise(count=n())
population = population[4:8,]
population$time = population$time + 2*1e3
p2 = ggplot(population,mapping=aes(x=time,y=count))
po = p2 + geom_line(col='lightblue',linewidth=1)+geom_point(size=2,alpha=0.6,col='blue')+theme_light()+
  theme(axis.text.x = element_text(angle=0, hjust=1,size=9),
        plot.title = element_text(color="black", size=15, face="bold",hjust=0.5),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12))+ 
  labs(x = "年份", y = '申请人数', title = "申请人数随年份的变化")

library("cowplot")
plot_grid(of, po, ncol = 1, nrow = 2)


# Q3
# 划分数据集，进行逐步逻辑回归
## 建立逻辑回归模型
# 包含以下自变量
# 申请季节、申请类型、申请学校是否为Top50
# 拓展信息中的所有变量、GPA成绩区间、托福成绩区间
data$offertype = factor(data$offertype)
data$season = factor(data$season)
data$type = factor(data$type,levels=c('MS','PhD','混合'))
data$rl = factor(data$rl)
data$intern = factor(data$intern)
data$research = factor(data$research)
data$paper = factor(data$paper)
data$first = factor(data$first)
data[which(data$first==-1),]$first = NA # 有的first填了-1，删去这些数据
data$sci = factor(data$sci)
data[which(data$sci==-1),]$sci = NA # 有的sci填了-1，删去这些数据
data = na.omit(data)
data$first = factor(data$first)
data$sci = factor(data$sci)
data$exchange = factor(data$exchange)

## 抽取训练集
set.seed(2023)    # 随机数种子
nsample = sample(x = dim(data)[1], size = dim(data)[1]/5, replace = F)
## 重新划分训练集和测试集
train_data = data[-nsample, ]  
test_data = data[nsample, ]
# 建立逻辑回归模型
formula = "offertype ~ season + type +CollegeRankTop50+
rl + intern + research + paper + first + sci + exchange + gpa_dis + toefl_dis"

logisticModel = glm(formula, 
                    family = binomial(link='logit'), data = train_data)  # 逻辑回归
stepModel = step(logisticModel, trace = F)     # AIC准则逐步回归
summary(stepModel)    # 查看回归结果

# Q4
# 对测试集的申请结果进行预测，并利用R包pROC绘制出ROC曲线图
# 进行预测
pred = predict(stepModel, test_data, type="response")
# 绘制ROC曲线
library(pROC)
# 绘制ROC曲线
plot.roc(test_data$offertype, pred, 
         col = "royalblue2", print.auc=TRUE,
         auc.polygon=TRUE, auc.polygon.col="aliceblue", 
         xlab = "FPR",ylab = "TPR", main = "预测ROC曲线",cex.lab=1.2)  

# Q5
# 阈值0.6，计算在测试集上的精准率（precision）和召回率（recall）
# 并在测试集绘制混淆矩阵
library(caret)
# 在测试集上进行预测
pred = predict(stepModel,test_data,type='response') 
confsTable = confusionMatrix(as.factor(pred>0.6),as.factor(test_data$offertype=='录取'))$table
precision = confsTable[2,2]/(confsTable[2,1]+confsTable[2,2])
recall = confsTable[2,2]/(confsTable[1,2]+confsTable[2,2])
# 精准率和召回率
P_R = cbind(precision,recall)
colnames(P_R) = c('精准率','召回率')
print(P_R)

# 混淆矩阵
print(confsTable)


# Q6 绘制成本收益曲线，对该曲线内容进行解读
# 仍然在测试集上绘制成本-收益曲线
alphas = seq(0.1,0.9,by=0.01)
cost = numeric(length(alphas))
recall = cost
for(i in 1:length(alphas)){
  alpha = alphas[i]
  confsTable = confusionMatrix(as.factor(pred>alpha),
                               as.factor(test_data$offertype=='录取'))$table
  # print(confsTable)
  recall[i] = confsTable[2,2]/(confsTable[1,2]+confsTable[2,2])
  cost[i] = (confsTable[2,1]+confsTable[2,2])/sum(confsTable)
}

library(tidyr)
base_recall = cost
temp = as.data.frame(cbind(cost,recall,base_recall))
temp = gather(temp,捕获率类型,recall,recall,base_recall)
temp[which(temp$捕获率类型=='recall'),]$捕获率类型 ='模型捕获率'
temp[which(temp$捕获率类型=='base_recall'),]$捕获率类型 ='基准捕获率'

library(ggplot2)

ggplot(temp,aes(cost,recall,group=捕获率类型,color=捕获率类型,shape=捕获率类型))+
  geom_point(size=2.5,alpha=0.5)+ theme_grey()+
  geom_line(linewidth=0.7)+ labs(x='成本（覆盖率）',y='收益（捕获率）')

# Q7 构造自变量，提升模型预测效果

# 最热门的10个被申请专业，提供可交互页面
library(plotly)
t3 = table(data$major_apply)
t3 = t3[order(t3,decreasing=T)]
t3 = as.data.frame(t3[1:10])
colnames(t3) = c('专业','申请人数')
plot_ly(t3,values= ~申请人数,labels= ~专业,type='pie',title='申请者原专业（前10）')
# 可以看出CS和EE占了一半以上，推测这些来自这些专业的同学可能
# 面临更加激烈的竞争，故新建一个变量
data$CSorEE = data$major_before == 'CS' |  data$major_before == 'EE'
offer_rate2 = data%>%group_by(CSorEE)%>%summarise(rate=mean(offertype=='录取'))
offer_rate2$rate = offer_rate2$rate/(1-offer_rate2$rate)
p = ggplot(offer_rate2,mapping=aes(x=CSorEE,y=rate))
of_CSorEE = p + geom_bar(stat='identity') +geom_col(colour='lightblue',fill='lightblue')+ theme_bw() + ylab('录取率优势比ODD') +
  xlab('是否来自CS或EE')  + 
  theme(axis.text.x = element_text(angle=0, hjust=1,size=9),
        plot.title = element_text(color="black", size=15, face="bold",hjust=0.5))

# 看一下转专业的影响
data$cross = as.logical(data$cross)
offer_rate3 = data%>%group_by(cross)%>%summarise(rate=mean(offertype=='录取'))
offer_rate3$rate = offer_rate3$rate/(1-offer_rate3$rate)
p = ggplot(offer_rate3,mapping=aes(x=cross,y=rate))
of_cross = p + geom_bar(stat='identity') +geom_col(colour='pink',fill='pink')+ theme_bw() + ylab('录取率优势比ODD') +
  xlab('是否转专业')  + 
  theme(axis.text.x = element_text(angle=0, hjust=1,size=9),
        plot.title = element_text(color="black", size=15, face="bold",hjust=0.5))

library("cowplot")
plot_grid(of_CSorEE, of_cross, ncol = 2, nrow = 1)

# 在第三问经过逐步回归筛选后的模型基础上增加新变量，建立新模型
## 抽取训练集
set.seed(2023)    # 随机数种子
nsample = sample(x = dim(data)[1], size = dim(data)[1]/5, replace = F)
## 重新划分训练集和测试集
train_data = data[-nsample, ]  
test_data = data[nsample, ]
# 建立新的逻辑回归模型
formula2 = "offertype ~ season + type +CollegeRankTop50+
  research + paper  + exchange + gpa_dis + toefl_dis + CSorEE + cross"

logisticModel2 = glm(formula2, 
                    family = binomial(link='logit'), data = train_data)  # 逻辑回归
stepModel2 = step(logisticModel2, trace = F)     # AIC准则逐步回归
summary(stepModel2)   # 查看新模型的回归结果

# 对测试集的申请结果进行预测，并利用R包pROC绘制出ROC曲线图
# 进行预测
pred2 = predict(stepModel2, test_data, type="response")
# 绘制ROC曲线
library(pROC)
# 绘制ROC曲线（AUC变大了！）
plot.roc(test_data$offertype, pred2, 
         col = "royalblue2", print.auc=TRUE,
         auc.polygon=TRUE, auc.polygon.col="aliceblue", 
         xlab = "FPR",ylab = "TPR", main = "新模型的预测ROC曲线",cex.lab=1.2) 

# 阈值0.6，计算新模型在测试集上的精准率（precision）和召回率（recall）
# 并在测试集绘制混淆矩阵
library(caret)
# 在测试集上用新模型进行预测
confsTable2 = confusionMatrix(as.factor(pred2>0.6),as.factor(test_data$offertype=='录取'))$table
precision2 = confsTable2[2,2]/(confsTable2[2,1]+confsTable2[2,2])
recall2 = confsTable2[2,2]/(confsTable2[1,2]+confsTable2[2,2])
# 精准率（精准率上升）与召回率（召回率下降）
P_R2 = cbind(precision2,recall2)
colnames(P_R2) = c('精准率','召回率')
print(P_R2)
# 混淆矩阵
print(confsTable2)

