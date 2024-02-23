# Generate data
set.seed(123)
Ns <- c(200, 500, 800, 1000)
R <- 200
beta_true <- as.matrix(c(0.5, 1.2, -1))
# X <- lapply(N, function(n) cbind(rep(1,n), mvrnorm(n=n,mu=rep(0,2),Sigma=diag(2))))
# Y <- lapply(X, function(x) rbinom(nrow(x), size=1, prob=plogis(X %*% beta_true)))

# Define the log-likelihood function
loglik <- function(beta,X,Y) {
  eta <- X %*% beta
  p <- plogis(eta)
  sum(Y*log(p+1e-13) + (1-Y)*log(1-p+1e-13)) # 防止取对数时出现inf
}

# Define the score function, which is also the gradient
score <- function(beta,X,Y) {
  eta <- X %*% beta
  p <- plogis(eta)
  t(X) %*% (Y - p)
}

# Define the Hessian matrix
hessian <- function(beta,X,Y) {
  eta <- X %*% beta
  p <- plogis(eta)
  W <- diag(as.numeric(p*(1-p)))
  t(X) %*% W %*% X
}

# Implement the Newton-Raphson algorithm
library(MASS)
opar <- par(mfrow=c(2,2))
for(i in 1:length(Ns)){
  N = Ns[i]
  beta_hat_collection = matrix(rep(0,3*R),nrow=3)
  beta_hat = as.matrix(rep(0,3)) # initial point
  for (r in seq_len(R)) {
    x = cbind(rep(1,N),10*rnorm(N),10*rnorm(N))
    prob = plogis(x%*% beta_true)
    y = as.numeric(prob>0.9) # true value of y
    loglik_old <- -Inf
    iter = 0
    while (TRUE) {
      score_new <- score(beta_hat,x,y)
      hessian_new <- hessian(beta_hat,x,y)
      beta_new <- beta_hat + ginv(hessian_new) %*% score_new # pseudo inverse
      
      # loglik_new <- loglik(beta_new,x,y)
      # print(loglik_new)
      
      if ( max(abs(beta_new-beta_hat)) <1e-5 || iter>100 ) break
    
      beta_hat <- beta_new
      iter = iter + 1
      # loglik_old <- loglik_new
    }
    
    beta_hat_collection[1:3,r] = beta_hat
  }
  boxplot(t(beta_hat_collection),main=paste('N=',N))
  }


