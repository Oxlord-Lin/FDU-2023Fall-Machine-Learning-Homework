library(corrplot)
library(psych)
library(factoextra)

# 1.	读入五大联赛数据集soccer.csv
library(readr)
soccer <- read_csv("soccer.csv")
predictor <- soccer[4:16]

# 2.	绘制自变量的相关系数图并解读
M <- cor(predictor)
par(family='STHeiti')
corrplot::corrplot(M, tl.srt = 60,tl.col = "black")

# 3. 对所有的自变量（除身价外）进行主成分分析，
# 选择主成分个数时使用碎石图的方法，并对结果进行解读；
# （提示：可以使用princomp()函数或psych包的principal()函数）
par(family='STHeiti')
result1 <- scree(predictor, factors = F, pc = T,  
                 main = "主成分分析崖底碎石图")
cumvar <- round(cumsum(result1$pcv)/sum(result1$pcv),2)
cat('前三个主成分累计方差贡献率为：', cumvar[1:3]) # 确定主成分个数为3

# 提取主成分
pc <- principal(predictor, nfactors = 3)
pc



# 4. 计算每一位球员的主成分得分
pc <- principal(predictor, nfactors = 3, scores = TRUE)
# 挑选三个球员
player.4 <- pc$scores[4,]
player.16 <- pc$scores[16,]
player.20 <- pc$scores[20,]

# 5. 使用计算得到的主成分得分，对足球运动员进行K-means聚类，并对结果进行解读
pc.scores <- as.data.frame(pc$scores)
fviz_nbclust(pc.scores, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)
# 最佳聚类个数为4个
km <- kmeans(pc.scores, 4, nstart = 24)
km$size
km$centers
# 聚类可视化
fviz_cluster(km, data = pc.scores[,-1],
             palette = c("blue", "green", "gold", "#FC4E07"),
             ellipse.type = "euclid",
             star.plot = TRUE, 
             repel = TRUE,
             ggtheme = theme_minimal()
)
