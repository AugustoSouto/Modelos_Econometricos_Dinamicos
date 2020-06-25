
% Critical values of the augmented Dickey Fuller test.

tic;
clear all;
close all;
clc;

% Set the seed.
rng(0);

% Set alphas.
alpha = 0.01:0.01:0.99;
T = [10 25 50 100 250 500 1000 2500 5000 10000 100000];
la  = length(alpha);
lt = length(T);
S =  1000000;

% Initialize.
critval=zeros(la,lt);
DFstats = zeros(S,1);

for t=1:lt
    disp(t)
    for s=1:S
        % Compute the critical values here.
        T1 =  T(t);
        u =  randn([T1,1]);
        W = 1/sqrt(T1)*cumsum(u); %1 sobre sqrt es para que quede con la misma varianza
        DFstats(s,1) = (W(T1)^2 -1)/(2*sqrt(mean(W.^2)));
    end
        critval(:,t) =  quantile(DFstats,alpha);
end

save('critval.mat','critval')


toc;