% These codes are for the specification tests.

clear all; %control c para el codigo corriendo-----clear borra en memoria
close all;
clc;

tic; % tiempo
%% Exercise 1

%generate the test mean vectors, outside the loop
meanlr= zeros(3,1);
meanlm = zeros(3,1);
meanwald =  zeros(3,1);

%% Now, compute the proportions of times that the 90 percent quantile is exceeded.
crit =  chi2inv(0.9,1);

%generate the test cdf 90 percent value
lr90 = zeros(3,1);
lm90 = zeros(3,1);
wald90 = zeros(3,1);

%now, define the three different 
rho_values = {0,0.025,0.2};

for num = 1:3


%% Set the seed.
rng(0)

%% Set the parameters.
T=1500;                 % time series length.
BIP=500;                % burn in period.
S=500;                 % number of MC simulations.

% Some of the parameters.
rho=rho_values{num};
beta=0.5;
alpha=0.3;
sv=1;
sw=1;

% Store all results.
lr=zeros(1,S);          % LR test statistic.
wald=zeros(1,S);        % Wald test statistic.
lm=zeros(1,S);          % LM test statistic.

%% Simulation of the model.
u = zeros(T,S);
ymat = zeros(T,S);
xmat = zeros(T,S);

mu0=0.0;

u0 = normrnd(mu0,(1-rho^2)^(-1)*sw);    % Initial value for u.
v = normrnd(mu0,sv,[T,S]);
w = normrnd(mu0,sw,[T,S]);
u(1,1) = u0;

for s=1:S
    %% Generate u and y.
    for t=2:T
       u(t,s) = rho*u(t-1,s) + w(t,s);
       ymat(t,s) =  alpha*beta*ymat(t-1,s) + beta*v(t,s) + u(t,s);
    end
end

% Generate x.
xmat(2:T,:) = alpha*ymat(1:T-1,:) + v(2:T,:);

% I only want to take the ones from BIP+1 to T.
ymat=ymat(BIP+1:T,:);
xmat=xmat(BIP+1:T,:);

%% Estimation of the model.

% Get the length of the time series to be the new time series length.
Tmat=length(xmat);
npar=5;
th0=zeros(npar,S); th1=zeros(npar,S); M1=zeros(1,S); M0=zeros(1,S);

for s=1:S
    disp(s) %pagina 15, son esas ecuaciones
    % Estimates under the null hypothesis H0.
    beta0  = (ymat(2:Tmat,s)'*xmat(2:Tmat,s))/(xmat(2:Tmat,s)'*xmat(2:Tmat,s));
    alpha0 = (xmat(2:Tmat,s)'*ymat(1:Tmat-1,s))/(ymat(1:Tmat-1,s)'*ymat(1:Tmat-1,s));
    sw20 =  (ymat(2:Tmat,s)- beta0*xmat(2:Tmat,s))'*(ymat(2:Tmat,s)- beta0*xmat(2:Tmat,s))/(Tmat-1);
    sv20 =  (xmat(2:Tmat,s)- alpha0*ymat(1:Tmat-1,s))'*(xmat(2:Tmat,s)- alpha0*ymat(1:Tmat-1,s))/(Tmat-1);
    th0(:,s) =  [beta0 alpha0 sw20 sv20 0];
    
    % Estimates under the alternative hypothesis H1.
    [th1(:,s),~] = concllik(ymat(:,s),xmat(:,s));
    
    % Tests.
    
    % LR.
    M0(:,s) = loglikeunrestricted(0,ymat(:,s),xmat(:,s));
    M1(:,s) = loglikeunrestricted(th1(5,s),ymat(:,s),xmat(:,s));
    lr(:,s) = 2*(M1(:,s)-M0(:,s));
    
    % LM.
    scr0 =  scr1(0,ymat(:,s),xmat(:,s));
    info0 = informat1(0,ymat(:,s),xmat(:,s));
    lm(:,s) =  (scr0'*inv(info0)*scr0)/Tmat;

    % Wald.
    info1=informat1(th1(5,s),ymat(:,s),xmat(:,s));
    stderr2 = diag(info1^(-1))/Tmat;
    wald(:,s) = (th1(5,s)/sqrt(stderr2(5)))^2;
    
end

%% Print the means.


meanlr(num, 1) = mean(lr);
meanlm(num, 1) = mean(lm);
meanwald(num, 1) =  mean(wald);

meanlr(num, 1)
meanlm(num, 1)
meanwald(num, 1)

%% Now, compute the proportions of times that the 90 percent quantile is exceeded.
crit =  chi2inv(0.9,1);

lr90(num, 1) = mean(lr>crit); % computa el porc de rechazo mean(trues/tot)
lm90(num, 1) = mean(lm>crit);
wald90(num, 1) = mean(wald>crit);

lr90(num, 1)
lm90(num, 1)
wald90(num, 1)

%% Calculate p-value discrepancies. pag 23

epv = linspace(0,0.999,100);
res = zeros(3,length(epv));

%use several thresholds to test how many times the test statistic is
%greater than the chi squared. calculate the mean and p values
%the test is above its 

for i=1:length(epv)
   chi =  chi2inv(epv(i),1);
   ulr = (lr > chi);
   ulm = (lm > chi);
   uwald = (wald > chi);
   res(1,i) = mean(ulr);
   res(2,i) = mean(ulm);
   res(3,i) = mean(uwald);  
end

%% Plot.

figure(1)
plot(1-epv,1-epv,'LineStyle','-.','LineWidth',2,'Color',[0 0 0])
hold on
plot(1-epv,res(1,:),'LineStyle','--','LineWidth',2,'Color',[0.8 0.2 0.2])
hold on
plot(1-epv,res(2,:),'LineStyle','--','LineWidth',2,'Color',[0.2 0.2 0.6])
hold on
plot(1-epv,res(3,:),'LineStyle','--','LineWidth',2,'Color',[0 0.5 0])
hold off
xlabel('Nominal p-value')
ylabel('Empirical p-value')
title('Size plots')
legend('45 degree line','LR','LM','Wald','Location','Southeast')

figure(2)
plot(1-epv,zeros(length(epv),1),'LineStyle','-.','LineWidth',2,'Color',[0 0 0])
hold on
plot(1-epv,res(1,:)-(1-epv),'LineStyle','--','LineWidth',2,'Color',[0.8 0.2 0.2])
hold on
plot(1-epv,res(2,:)-(1-epv),'LineStyle','--','LineWidth',2,'Color',[0.2 0.2 0.6])
hold on
plot(1-epv,res(3,:)-(1-epv),'LineStyle','--','LineWidth',2,'Color',[0 0.5 0])
hold off
xlabel('Nominal p-value')
ylabel('p-value discrepancy')
title('Discrepancy plots')
legend('45 degree line','LR','LM','Wald','Location','Southeast')

%% Kernel density of the rho's.
nbins=20;
figure(3)
histfit(th1(5,:),nbins)
xlabel('\rho')
ylabel('density')

end

%% EJERCICIO 2

clear all

%MARKOV CHAIN PARAMETERS
Te={100, 1000, 10000};
pe={0.2,0.95 };
qe={0.6,0.95 };

mu=zeros(3,2);
sdev=zeros(3,2);


for iter1=1:3
    for iter2=1:2
%%%%%%%%%%%%%%%%%%%%%%%%
p=pe{iter2}
q=qe{iter2}


BIP=50;
T=Te{iter1}+BIP;
S=500;
%%%%%%%%%%%%%%%%%%%%%%%

%GENERATE TIME SERIES MATRIX 
y=zeros(T,S);

%DRAW THE FIRST VALUE FOR EACH SIMULATION 
y(1,:)=randi([0,1],1,S);

%DRAW RANDOM NUMBERS TO SIMULATE MARKOV CHAIN VIA MONTE CARLO
draws=rand(T-1,S);

%% GENERATE THE TRANSITIONS WITH THE RANDOM NUMBERS

for time=2:T
    for sim=1:S
        if y(time-1,sim)==0 && draws(time-1,sim)<p
            y(time,S)=0;
        end
        if y(time-1,sim)==0 && draws(time-1,sim)>=p
            y(time,sim)=1;
        end
        if y(time-1,sim)==1 && draws(time-1,sim)<q
            y(time,sim)=1;
        end
        if y(time-1,sim)==1 && draws(time-1,sim)>=q
            y(time,sim)=0;
        end
    end
end

%%
%DELETE BURN IN PERIOD SAMPLED OBSERVATIONS
y=y(BIP+1:T,:) 

% A) COMPUTE THE MEAN FOR EACH SIMULATION
%binary markov chain
mean(y)
%MEAN ACROSS ALL SIMULATIONS
mu(iter1, iter2)=mean(mean(y));
%MEAN STANDARD DEVIATION
sdev(iter1,iter2)=std(mean(y));
 
histogram(mean(y))

    end
end

%% NOW, LETS COMPUTE THE NORMAL DIST MPLE

%proposed distribution sd=1, then variance is also =1



%% EXERCISE NUMBER 3



toc;

