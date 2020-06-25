% This is the Bayesian example for the Time Series Econometrics class.

tic; 

clear all;
close all;
clc;

rng(0);

% Parameters.
T=1100;
BIP=1000;
alpha=0.5;
tau=1;
sig=1/tau;

% True data generating process.
x=zeros(T,1);
x(1) = sqrt(sig^2/(1-alpha^2))*randn;
for tt=2:T
    x(tt,1)=alpha*x(tt-1,1)+sig*randn;
end
x=x(BIP+1:T);
T=length(x);

% Parameters of the priors.
alpha0=-0.5;
nu0=1.5;
gam0=1;

% Auxiliary parameters.
z = (x(1:T-1)'*x(1:T-1));
alphahat =  (x(2:T,1)'*x(1:T-1,1))/z;
s = (x(2:T,1) -alphahat*x(1:T-1,1))'*(x(2:end,1) -alphahat*x(1:T-1,1));

% Parameters of the posterior of tau.
gam_post =  gam0+0.5*s+0.5*z*(alphahat-alpha0)^2/(1+z);
nu_post = (T+2*nu0)/2;

% Parameters of the posterior of alpha.
s2_bar =  1/(z+1);
a_bar = (alphahat*z + alpha0)*s2_bar;
scale = (s2_bar/(T+2*nu0))*2*gam_post;
 
% Generate density.

% Grid of x values for alpha and tau.
n=10000;
alphagrid= linspace(-1,1,n);
taugrid=linspace(1e-3,4,n);

% Prior densities.
prior_alpha =  sqrt(gam0/nu0)^(-1)*tpdf((alphagrid - alpha0)./sqrt(gam0/nu0),2*nu0);
prior_tau = gampdf(taugrid,nu0,1/gam0);

% Posterior densities.
post_alpha = sqrt(scale)^(-1)*tpdf((alphagrid-a_bar)./sqrt(scale),T+2*nu0);
post_tau = gampdf(taugrid,nu_post,1/gam_post);

% Predictive density.
nsim=10000;
tau_post_simul =  gamrnd(nu_post,1/gam_post,[nsim,1]);
alpha_post_simul =  a_bar +sqrt(s2_bar./tau_post_simul).*randn(nsim,1);
xpredict = alpha_post_simul*x(end)+ sqrt(1./tau_post_simul).*randn(nsim,1);

%% Plot results.

figure(1)
plot(alphagrid,prior_alpha,'LineStyle','--','LineWidth',2,'Color',[0.8 0.2 0.2])
hold on
plot(alphagrid,post_alpha,'LineStyle','-','LineWidth',2,'Color',[0.2 0.2 0.6])
hold off
legend('Prior', 'Posterior', 'Location','Northeast')

figure(2)
plot(taugrid,prior_tau,'LineStyle','--','LineWidth',2,'Color',[0.8 0.2 0.2])
hold on
plot(taugrid,post_tau,'LineStyle','-','LineWidth',2,'Color',[0.2 0.2 0.6])
hold off
legend('Prior', 'Posterior', 'Location','Northeast')

figure(3)
nbins=100;
histfit(xpredict,nbins)

T1=11000;
xtrue=zeros(T1,1);
xtrue(1) = sqrt(sig^2/(1-alpha^2))*randn;
for tt=2:T1
    xtrue(tt,1)=alpha*xtrue(tt-1,1)+sig*randn;
end
xtrue=xtrue(BIP+1:end,1);



[ff1,x1]=ksdensity(xpredict,'NumPoints',10000);
[ff2,x2]=ksdensity(xtrue,'NumPoints',10000);

figure(4)
plot(x1,ff1,'LineStyle','--','LineWidth',2,'Color',[0.8 0.2 0.2])
hold on
plot(x2,ff2,'LineStyle','-','LineWidth',2,'Color',[0.2 0.2 0.6])
hold off
legend('predicted', 'true', 'Location','Northeast')


toc;