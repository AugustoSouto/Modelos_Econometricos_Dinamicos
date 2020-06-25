function [adfcoef,adfstat,lags]=adfautolag(y,maxlags,crit)

% Dickey Fuller and augmented DF with automatic lag selection (without the
% deterministic trend or the constant.)

% crit =1 : Akaike information criterion.
% crit =2 : Bayesian information criterion.

% Set-up.
ydiff=diff(y);
[~,ydifflags]=newlagmatrix(ydiff,maxlags);
T=length(y);
Y=y(maxlags+2:T);
tau=length(Y);

i=0;
X=y(maxlags+1:T-1);
rho=X\Y;
% Compute the errors.
e =  Y-X*rho;
s2(i+1) =e'*e/tau;
K(i+1)=size(X,2);

% Loop.
for i=1:maxlags
   X=[y(maxlags+1:T-1) ydifflags(:,1:i)];
   rho = X\Y;
   % Compute the errors.
e =  Y-X*rho;
s2(i+1) =e'*e/tau;
K(i+1)=size(X,2);    
end

% Criteria.
if crit==1
    ICs = log(s2) + 2*K/tau;
else
    ICs = log(s2) + K*log(tau)/tau;
end
[~,lags] =  min(ICs);
lags = lags - 1;

% Once I have chosen the number of lags, do the test again.
ydiff=diff(y);
[ydiffcurr,ydifflags]=newlagmatrix(ydiff,lags);
T=length(y);
Y=y(lags+2:T);
tau=length(Y);

X=[y(lags+1:T-1), ydifflags];
rho = X\ydiffcurr;
% Compute the errors.
e = ydiffcurr-X*rho;

% Compute the covariance matrix of the estimated parameters.
s2 = e'*e/(tau-size(X,2));
Uinv =  inv(diag([T T^(0.5)*ones(1,lags)]));
sel = [1 zeros(1,lags)];
sigp = s2*sel*(Uinv*(X'*X)*Uinv)^(-1)*sel';
adfstat = tau*rho(1)/sqrt(sigp);
adfcoef = rho(1);

function [y,x]=newlagmatrix(x,nlags)

% This function is used in order to create a new lag matrix for the
% augmented DF regression.
T1=length(x);

if nlags>0
    nlags=nlags+1;
    newX=[x;zeros(nlags,1)];
    lagmatrix=repmat(newX,nlags,1);
    lagmatrix=reshape(lagmatrix(1:size(lagmatrix,1)-nlags),T1+nlags-1,nlags);
    lagmatrix=lagmatrix(nlags:T1,:);
    y=lagmatrix(:,1);
    x=lagmatrix(:,2:nlags);
else
        y=x;
        x=[];
end
end
end