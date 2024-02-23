library(readxl)
data <- read_excel("第三题数据.xlsx", col_names = FALSE)
data = t(data)
colnames(data) = c('x','y')
data = as.data.frame(data)
epsilon = 0.1

split <- function(data,i,j,epsilon){ # i是左侧下标，j是右侧下标，level是切分的层次
  if(i==j){ # 如果数据集已经不可再分
    return(NULL)
  }
  L = sum((data[i:j,]$y-mean(data[i:j,]$y))^2)
  if(L<epsilon){ # 如果数据集的损失函数已经足够小
    return(NULL)
  }
  minL = Inf
  minK = i
  # 寻找该子集的最佳切分点
  for(k in seq(i,j-1)){
    # 先计算该子集的损失函数
    sub1 = data[i:k,]
    sub2 = data[(k+1):j,]
    # 损失函数L
    L = sum((sub1$y-mean(sub1$y))^2) + sum((sub2$y-mean(sub2$y))^2) 
    print(k)
    print(L)
    if( L<minL ){
      minL = L
      minK = k
    }
  }
  return(minK)
}

epsilon = 0.5 # 停止界值

# 第一层的分界点
k.1 = split(data,1,10,epsilon)
print(k.1) # 5

# 第二层的分界点
k.2.1 = split(data,1,5,epsilon)
k.2.2 = split(data,6,10,epsilon)
print(c(k.2.1,k.2.2)) # 3,7

# 第三层的分界点
k.3.1 = split(data,1,3,epsilon)
k.3.2 = split(data,4,5,epsilon)
k.3.3 = split(data,6,7,epsilon)
k.3.4 = split(data,8,10,epsilon)
print(c(k.3.1,k.3.2,k.3.3,k.3.4)) # NULL

# 每个叶子节点的损失函数都小于临界值epsilon=0.5，停止迭代
