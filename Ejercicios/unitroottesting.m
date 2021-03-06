% This example is for unit root testing using the Dickey Fuller test.
tic;

clear all;
close all;
clc;

% Set the seed.
rng(0)

% Parameters.
S=1000;                         % number of simulations.
T=2500;                         % length of T.
BIP=500;                        % burn in period of the time series.
rho=1;                          % AR(1) lag.
theta=[-0.99, -0.95, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 0.95, 0.99];  % MA(1) lag.
N1 =  length(theta);            % length of theta.
sig=1;                          % Standard deviation of the white noise.
mu=0;                           % Mean of the white noise.    
u = normrnd(mu,sig,[T,S]);      % White noise.
maxlags=15;
    
% Initialize vectors.
xsims=zeros(T,S);               % Store the time series here.
adfregs=zeros(N1,S);
adfstats=zeros(N1,S);           % ADF test statistics.    
lags=zeros(N1,S);
IC=zeros(N1,S);
pval=zeros(N1,S);

% Load critical values.
load('critval.mat')
Ts = [10 25 50 100 250 500 1000 2500 5000 10000 100000];
Cvs = 0.01:0.01:0.99;

for i=1:N1
    disp(i)
    for s=1:S
    % Simulate the time series process.
    for tt=2:T
       xsims(tt,s) = rho*xsims(tt-1,s) + u(tt,s) - theta(i)*u(tt-1,s); 
    end
    xtemp =  xsims(BIP+1:T,s);
    
    % From simulating the time series process, I can then perform the
    % augmented Dickey Fuller test.
    [adfregs(i,s),adfstats(i,s),lags(i,s)]=adfautolag(xtemp,maxlags,1);
    
    % Now, calculate the associated p values and critical values.
    T1 = length(xtemp);
    
    if T1>max(Ts)
        first_col = size(critval,2);
        second_col = first_col;
    elseif T1<min(Ts)
        first_col = size(critval,2);
        second_col = first_col;
    else
        first_col = max(find(Ts<=T1));
        second_col = min(find(Ts>=T1));
    end
    
    if first_col ~= second_col
        w = (Ts(second_col)-T)/(Ts(second_col)-Ts(first_col)); %interpola el valor de la matriz, usando ambos valores
    else
        w=0;
    end
    
    Cv1=critval(:,first_col);
    Cv2=critval(:,second_col);
    Cv=w*(Cv1)+(1-w)*Cv2;
    
    first = max(find(adfstats(i,s)>Cv));
    last = min(find(adfstats(i,s)<Cv));
    
    if isempty(last)
        pval(i,s)=1;
    elseif isempty(first)
        pval(i,s)=0;
    else
        if first~=last
            wlast=1-(Cv(last)-adfstats(1,s))/(Cv(last)-Cv(first));
        else
            wlast=1;
        end
        pval(i,s)=(1-wlast)*Cvs(first)+wlast*Cvs(last);
    end
    end
end

% Compute the mean and the standard deviation of the coefficient in the ADF
% test, and the p-values.
mean(pval,2) %hace la media de la columna (dimension 2 de la matriz)
mean(adfregs,2)
std(adfregs,[],2)
mean(lags,2)
std(lags,[],2)

% Plot one realization of the time series.
figure(1)
t1=1:1:length(xsims);
plot(t1,xsims(:,end));
xlabel('t')
ylabel('y_{t}')
title('ARMA(1,1), y_{t}=y_{t-1}+u_{t}-u_{t-1}')

nlags=10;
acf1=autocorrfunc(xsims(:,end),nlags);

figure(2)
tt=0:nlags;
bar(tt,[1; acf1]);
xlabel('lag');
ylabel('ACF');
title('ARMA(1,1), y_{t}=y_{t-1}+u_{t}-u_{t-1}')
set(gca,'xlim',[0 nlags]);
set(gca,'ylim',[0 1]);
set(gca,'xtick',(0:5:nlags)');
set(gca,'ytick',(0:0.5:1)');

toc;