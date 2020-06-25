% This code estimates an MA(1) by maximum likelihood and PML.
tic;

clear all;
close all;
clc;

%% Set random seed.
rng(0)

%% Parameters.
ngrid=11;                                   % Number of values of theta to be considered.
nsim=100;                                   % Number of Monte Carlo repetitions.
T=1500;                                     % sample size.
BIP=500;                                    % burn in period.
theta=linspace(-0.99,0.99,ngrid);             % parameter. linspace pone ngrid numeros -0.99 y 0.99
lb=-0.99;
ub=0.99;
options = optimset('display','off','TolX',1e-15,'Algorithm','interior-point');

%% Initialize the vectors to save the parameter estimates in the Monte Carlo repetitions.
th_ma1=zeros(nsim,1);
th_ar1=zeros(nsim,1);
asym_se_ma1=zeros(nsim,1);

%% Initialize vectors to save the sample mean of the estimates.
m_mle=zeros(ngrid,1);
m_ar1=zeros(ngrid,1);

%% Initalize vectors to save the standard errors.
v_mle=zeros(ngrid,1);
v_mle_a=zeros(ngrid,1);
v_ar1=zeros(ngrid,1);

%% Simulations.

for i=1:ngrid
    disp(i)
    for j=1:nsim
        
        % Generate x.
        u = randn(T,1);
        x = zeros(T,1);
        
        x(1,1) = sqrt(1+theta(i)^2)*u(1,1);
        
        for t=2:T
            x(t,1) = u(t,1) + theta(i)*u(t-1,1);
        end
        
        x=x(BIP+1:T,1);
        T1 = length(x);
        
        % AR(1) estimates.
        rho = (x(2:T1,1)'*x(1:T1-1,1))/(x(1:T1-1,1)'*x(1:T1-1,1));
        th_ar1(j,1) = implied_ma(rho);
        
        % MA(1) estimates.
        [th_ma1(j,1),~,~]=fmincon(@(th_ma1)loglikema1(th_ma1,x),0,[],[],[],[],lb,ub,[],options);
        
        % Information matrix.
        asym_se_ma1(j,1) = 1/(T1*informat1(th_ma1(j,1),x));

    end
    
    % reescala la varianza al tamaño de la muestra, ver bien donde lo hace 
    
    
    % Save the sample mean of the estimates.
    m_ar1(i,1) = mean(th_ar1);
    v_ar1(i,1) = std(th_ar1);
    
    % Save standard error.
    m_mle(i,1) = mean(th_ma1);
    v_mle(i,1) = std(th_ma1);
    
    % Save the asymptotic standard error.
    v_mle_a(i,1) = sqrt(mean(asym_se_ma1));
    
end

%% Graphs.

figure(1)
plot(theta,theta,'LineStyle',':','LineWidth',1,'Color',[0.8 0.2 0.2])
hold on
plot(theta,m_mle,'LineStyle','-','LineWidth',1,'Color',[0.2 0.2 0.6])
hold on
plot(theta,m_ar1,'LineStyle','--','LineWidth',1,'Color',[0 0.5 0])
hold off
xmin=min(theta);
xmax=max(theta);
xlim([xmin xmax])
xlabel('\theta')
ylabel('Mean')
legend('True value','ML','PML','Location','Southeast')

figure(2)
plot(theta,v_mle_a,'LineStyle',':','LineWidth',1,'Color',[0.8 0.2 0.2])
hold on
plot(theta,v_mle,'LineStyle','-','LineWidth',1,'Color',[0.2 0.2 0.6])
hold on
plot(theta,v_ar1,'LineStyle','--','LineWidth',1,'Color',[0 0.5 0])
hold off
xmin=min(theta);
xmax=max(theta);
xlim([xmin xmax])
xlabel('\theta')
ylabel('SD')
legend('True value','ML','PML','Location','Southeast')

toc;