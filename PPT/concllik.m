function [theta,M]=concllik(y,x)

T=length(x);
options = optimset('display','off','TolX',1e-15,'Algorithm','interior-point');

% y's and x's to calculate beta's, etc.

yt=y(2:T,1);
y1=y(1:T-1,1);
xt=x(2:T,1);
x1=x(1:T-1,1);
theta=zeros(5,1);
b=0.999;

% Estimate rho of the unrestricted model.
L = @(a) (-1)*loglikeunrestricted(a,y,x); % queda asi porque en matlab
                                         % se minimiza y queremos max el
                                         % likelihood 
                                         
[rho,M] = fmincon(L,0,[],[],[],[],-b,b,[],options);
M = -M;

beta = ((yt-rho*y1)'*(xt-rho*x1))/((xt-rho*x1)'*(xt-rho*x1));
alpha = (xt'*y1)/(y1'*y1);
u = y - beta*x;
uhat  = u(2:T,1); 
uhat1 = u(1:T-1,1);
sw2 = (uhat - rho*uhat1)'*(uhat - rho*uhat1)/(T-1);
sv2 = (xt - alpha*y1)'*(xt - alpha*y1)/(T-1);

theta(1)=beta;
theta(2)=alpha;
theta(3)=sw2;
theta(4)=sv2;
theta(5)=rho;


end

