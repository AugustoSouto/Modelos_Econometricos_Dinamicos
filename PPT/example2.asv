% Example 2, Class 3. Time Series.

tic;
clear;
close all;
clc;

%% Set the seed.
rng(0);

%% Parameters.
T=1250;                                 % sample size.
BIP=250;                                % burn in period.
T1=T-BIP;
S=1000;                                 % number of simulations.
theta=linspace(-.99,.99,100)';          % grid of theta. 
N=length(theta);                        % length of theta.
x=zeros(T,1);                           % Initialize x.
q=(T-BIP)^(1/3);                        % maximum number of lags in Newey-West.
c=1;                                    % centered or not?
u0=randn;                               % initial x.

%% Initialize the vectors here.
mc_mu=zeros(S,N);
se_nr=zeros(S,N);
se_nw=zeros(S,N);
se_ma=zeros(S,N);
mc_var=zeros(1,N);

%% Do the loop.
for i=1:N 
    disp(i)
    for s=1:S
        u = randn(T,1);
        %% Generate the data.
        x(1,1) =  (randn + theta(i,1)*u0)/sqrt(1+theta(i,1)^2);
        for t=2:T
            x(t,1) = (u(t,1) + theta(i,1)*u(t-1,1))/sqrt(1+theta(i,1)^2);
        end
        x=x(BIP+1:end,:);
        
        %% After generating the data, calculate the mean.
        mu_hat =  mean(x);
        mc_mu(s,i) = mu_hat;
        
        %% Calculate the standard errors.
        
        % Non-robust standard errors.
        se_nr(s,i) = (T1)^(-1)*((x - mu_hat)'*(x-mu_hat));        
     
        % Newey West.
        se_nw(s,i) = newey(x,q,c);
        
        % MA(1).
        gamma1hat =  (x(2:T1)-mu_hat)'*(x(1:T1-1)-mu_hat)/(T1-1);
        se_ma(s,i) = 1 + 2*gamma1hat;
    end
    
    mc_var1 = var(mc_mu(:,i));
    mc_var(1,i)=mc_var1*T1;
    
end
 
% Plot against theta.
figure(1)
plot(theta,mc_var,'LineWidth',1.5,'LineStyle','--','Color',[0 0 0])
hold on % lines en R
plot(theta,mean(se_nr,1),'LineWidth',1.5,'LineStyle','-','Color',[0 0.5 0])
hold on
plot(theta,mean(se_nw,1),'LineWidth',1.5,'LineStyle','-.','Color',[0.8 0.2 0.2])
hold on
plot(theta,mean(se_ma,1),'LineWidth',1.5,'LineStyle',':','Color',[0.2 0.6 0.6])
hold on
plot(theta,1+ 2*(theta./((1+theta.^2))),'LineWidth',0.5,'LineStyle','none','Marker','+','Color',[0.5 0.5 0.5])
hold off
xlabel('\theta') %insertar letras griegas requiere poner la barra
ylabel('SE') %
legend('MC','Naive','NW','MA(1)','Asymptotic','Location','Southeast')