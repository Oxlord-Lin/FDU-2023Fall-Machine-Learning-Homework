% Add a column of ones to the predictor matrix
N = 10;
X1 = randn(N,1);
X2 = randn(N,1);
X = [ones(N,1) ,X1,X2];
beta_true = [0.5, 1.2, -1]';
y = exp(X*beta_true)./(1+exp(X*beta_true));
y(y>0.5) = 1;   y(y<=0.5) = 0; % 真实的y值

b = zeros(size(X,2),1);

% Set the maximum number of iterations
max_iter = 100;

% Set the convergence threshold
tol = 1e-5;

% Initialize the iteration counter and the change in coefficients
iter = 0;
delta_b = Inf;

% Run the Newton-Raphson algorithm until convergence or maximum iterations are reached
while (norm(delta_b) > tol) && (iter < max_iter)
    % Compute the predicted probabilities
    p = 1./(1 + exp(-X*b));
    
    % Compute the gradient and Hessian matrix
    grad = X'*(y - p);
    H = X'*diag(p.*(1-p))*X;
    
    % Update the coefficients
    delta_b = H\grad;
    b = b + delta_b;
    
    % Update the iteration counter
    iter = iter + 1;
end

% Display the estimated coefficients
disp(b);


%% 
% Load sample data
load fisheriris

% Convert species names to numeric values
y = grp2idx(species);

% Add a column of ones to the predictor matrix
X = [ones(size(meas,1),1) meas];

% Initialize coefficients to zero
b = zeros(size(X,2),1);

% Set the maximum number of iterations
max_iter = 100;

% Set the convergence threshold
tol = 1e-3;

% Initialize the iteration counter and the change in coefficients
iter = 0;
delta_b = 100;

% Run the Newton-Raphson algorithm until convergence or maximum iterations are reached
while (norm(delta_b) > tol) && (iter < max_iter)
    % Compute the predicted probabilities
    p = 1./(1 + exp(-X*b));
    
    % Compute the gradient and Hessian matrix
    grad = X'*(y - p);
    H = X'*diag(p.*(1-p))*X;
    
    % Update the coefficients
    delta_b = H\grad
    b = b + delta_b;
    
    % Update the iteration counter
    iter = iter + 1;
end

% Display the estimated coefficients
disp(b);