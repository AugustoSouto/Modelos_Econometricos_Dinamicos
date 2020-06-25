% Example 1, Class 3: Time Series.

tic;
clear;
close all;
clc;

%% Set the seed.
rng(0);

%% Parameters.

T=1250;                 % sample size.
BIP=250;                % burn in period.
q=floor(0.75*T^(1/3));  % maximum number of lags in Newey-West.
c=1;                    % centered or not?
u=randn(T,1);           % innovations.
x=zeros(T,1);           % data.
th=0.0;                 % theta.
u0=randn;               % first u.

%% Initialize x.
x(1,1) =  (randn + th*u0)/sqrt(1+th^2);
for t=2:T
   x(t,1) = (u(t,1) + th*u(t-1,1))/sqrt(1+th^2);
end
x=x(BIP+1:end,:);

%% Estimator of the mean.
mu_hat = mean(x,1); % sum(x)/length(x); % mean(x)

%% Calculate standard errors.

% 1.) Non-robust standard error (assuming that x is iid).
se_nr_1 =  (length(x))^(-1)*((x - mu_hat)'*(x - mu_hat));
se_nr_2 = mean((x-mu_hat).^2);

% 2.) Newey-West correction.
% a.) Compute directly here using loops.
se_nw_1 = se_nr_1;                               % Initialize robust non paramtric se
for j = 1:q
    se_nw_1 = se_nw_1 + 2*(1-(j/(q+1)))*( (x(j+1:length(x))-mu_hat)'*(x(1:length(x)-j)-mu_hat))/(length(x)-j);
end

% b.) Write a function that I could use to calculate Newey-West outside of
% the main code.
se_nw_2 = newey(x,q,c);

% 3.) Estimator when we know that the true DGP is an MA(1).
gamma1hat =  (x(2:length(x))-mu_hat)'*(x(1:length(x)-1)-mu_hat)/(length(x)-1);
se_ma_1 =  1 + 2*gamma1hat;
se_ma_2 =  1 + 2*th;
 
toc;