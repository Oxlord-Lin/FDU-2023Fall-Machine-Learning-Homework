lgst = function(y,X,beta){
  -(t(y)%*%X%*%beta - sum(log(1+exp(X%*%beta))) )
}

Ns = c(200,500,800,1000)
R = 200
beta_true = c(0.5, 1.2, -1) 
beta_true = as.matrix(beta_true)
for (i in 1:length(Ns)){
  N = Ns[i]
  res_collection = as.matrix(rep(0,3*200),nrow=3); 
    for (r in 1:R) { 
    X1 = 100*as.matrix(rnorm(N));
    X2 = 100*as.matrix(rnorm(N));
    ones = as.matrix(rep(1,N))
    X = cbind(ones,X1,X2)
    y = exp(X %*% beta_true)/(1+exp(X%*%beta_true))
    y[which(y>0.5),] = 1
    y[which(y<=0.5),] = 0 
    beta = c(0,0,0) 
    beta = as.matrix(beta)
    iter = 0;
    while (iter <= 100){ # Newton-Raphson 
      print(iter)
      t = exp(X%*%beta);
      p = t/(1+t)
      W = diag(as.numeric(p*(1-p)))
      H = t(X)%*%W%*%X
      g = -t(X)%*%(y-p)
      beta_new = beta - solve(H)%*%g;
      print(lgst(y,X,beta_new))
      Sys.sleep(1)
      if (norm(beta_new-beta,'M') < 1e-5) { 
        break}
      beta = beta_new
      print(beta)
      # iter = iter + 1
      # Sys.sleep(1)
    }
    res_collection[,r] = beta_new - beta_true;}
  
  boxplot(t(res_collection))
}