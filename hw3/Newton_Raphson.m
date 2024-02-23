Ns = [200,500,800,1000];
R = 200;
beta_true = [0.5, 1.2, -1]'; % 真实的beta值
for i = 1:length(Ns)
    N = Ns(i);
    res_collection = []; % 用于收集每一轮得到的beta估计值的残差
    for r = 1:R % 进行R轮估计
        X1 = randn(N,1);
        X2 = randn(N,1);
        X = [ones(N,1),X1,X2];
        y = exp(X*beta_true)./(1+exp(X*beta_true));
        y(y>0.5) = 1;   y(y<=0.5) = 0; % 真实的y值
        beta = [0,0,0]'; % 初始的beta值
        iter = 0;
        while iter <= 100 % Newton-Raphson 迭代
            t = exp(X*beta);
            p = t./(1+t);
            W = diag(p.*(1-p));
            beta_new = beta + (X'*W*X)\(X'*(y-p));
            if norm(beta_new-beta,"inf") <1e-5 % 用无穷范数刻画新旧beta的差异
                break
            else
                beta = beta_new;
            end
            iter = iter + 1;
        end
        res_collection = [res_collection, beta_new - beta_true];
    end
    figure()
    boxplot(res_collection')
end