# Q1
options(warn = -1)
library(readxl)
Mydata <- read_excel("D:/大三上学习资料/统计（机器）学习/hw3/Mydata.xlsx")
Mydata$地区 = as.factor(Mydata$地区)
Mydata$公司类别 = as.factor(Mydata$公司类别)
Mydata$公司规模 = as.factor(Mydata$公司规模)
Mydata$行业类别 = as.factor(Mydata$行业类别)
Mydata$经验 = as.numeric(Mydata$经验)
Mydata$学历 = factor(Mydata$学历,levels=c('无','中专','高中','大专','本科','硕士','博士' ))
Mydata$人数 = as.numeric(Mydata$人数)
Mydata$最低薪资 = as.numeric(Mydata$最低薪资)
Mydata$最高薪资 = as.numeric(Mydata$最高薪资)
Mydata$平均薪资 = (Mydata$最低薪资+Mydata$最高薪资)/2

# Q2
ind_freq = table(Mydata$行业类别)
ind_freq = ind_freq[order(ind_freq,decreasing = T)]
barplot(ind_freq[1:5],names.arg = names(ind_freq)[1:5],family='Simsun',
        ylab='频数',xlab='',col=c('gold','grey','grey','grey','grey'),las=2)
title('频次最高的5个行业类别的频数直方图')

# Q3
library(wordcloud2)
wordcloud2(ind_freq[-(1:5)],size=0.25)

# Q4
Mydata$对数平均薪资 = log(Mydata$平均薪资)
summary(Mydata$对数平均薪资)
highest = Mydata[which(Mydata$对数平均薪资 == max(Mydata$对数平均薪资,na.rm=TRUE)),]
lowest = Mydata[which(Mydata$对数平均薪资 == min(Mydata$对数平均薪资,na.rm=TRUE)),]
c(highest$公司名称,highest$职位)
c(lowest$公司名称,lowest$职位)

hist(Mydata$对数平均薪资,xlab='对数平均薪资',ylab='频数',main='对数平均薪资的频数直方图')


# Q5
# png(file='学历对薪资的影响.png', height=900, width=600)
opar <- par(mfrow=c(3,1),mar=c(6,8,1,1))
boxplot(log(平均薪资)~学历,data=Mydata,col=c('gold','grey'),ylab='对数平均薪资',cex.lab=1,cex.axis=1,las=2)
boxplot(log(最高薪资)~学历,data=Mydata,col=c('gold','grey'),ylab='对数最高薪资',cex.lab=1,cex.axis=1,las=2)
boxplot(log(最低薪资)~学历,data=Mydata,col=c('gold','grey'),ylab='对数最低薪资',cex.lab=1,cex.axis=1,las=2)
# dev.off()

# Q6
png(file='软件对薪资的影响.png', height=1800, width=1200)
opar <- par(mfrow=c(4,3),mar=c(5,4,1,1))
software = c('R','SPSS','Excel','Python','MATLAB','Java',
             'SQL','SAS','Stata','EViews','Spark','Hadoop')
for(i in 1:12){
  item = software[i]
  Mydata$new = grepl(item,Mydata$描述,ignore.case = TRUE)
  boxplot(log(平均薪资)~new,data=Mydata,col=c('grey','gold'),
          ylab='对数平均薪资',xlab='',
          main=paste('是否会使用',item),cex.lab=1)
  colnames(Mydata)[colnames(Mydata)=='new'] = item
}
dev.off()

# Q7
Mydata2 = Mydata[!is.na(Mydata$平均薪资),] # 去除无效数据
mod = lm(log(平均薪资)~地区+公司类别+公司规模+学历+经验+R+SPSS+
        Excel+Python+MATLAB+Java+SQL+SAS+Stata+EViews+Spark+Hadoop,
        data=Mydata2)
summary(mod)
library(broom)
res_mod <- as.data.frame(tidy(mod))
res_mod2 <- as.data.frame(glance(mod))
write.csv(res_mod, "res_mod.csv")
write.csv(res_mod2, "res_mod2.csv")
# 进行回归检验
par(mfrow=c(2,2)) 
plot(mod,which=c(1:4))

# Q8
library(DAAG)
rmses = rep(0,50)
for(i in 1:50){
  cat(i,'\r')
  CV = cv.lm(data=Mydata2,form.lm=mod,m=5,
          plotit = F,prinit=F,seed=23*i) # 设定每次划分样本的随机数种子
  rmses[i] = attr(CV,'ms')
}
# 对250个预测误差求平均
mean(rmses)
# 0.18969
