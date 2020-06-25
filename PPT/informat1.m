function [out]=informat1(rho,y,x)

T=length(x);

% y and x.
yt=y(2:T,1);
y1t=y(1:T-1,1);
xt=x(2:T,1);
x1t=x(1:T-1,1);

% beta, alpha, u, sw22, sv2.
beta = ((yt-rho*y1t)'*(xt-rho*x1t))/((xt-rho*x1t)'*(xt-rho*x1t));
alpha = (xt'*y1t)/(y1t'*y1t);

u = y-beta*x;
ut = u(2:T,1);
u1t= u(1:T-1,1);

sw2=((ut-rho*u1t)'*(ut-rho*u1t))/(T-1);
sv2=((xt-alpha*y1t)'*(xt-alpha*y1t))/(T-1);

% Score.
s_beta = (1/sw2)*(ut-rho*u1t).*(xt-rho*x1t);
s_rho =  (1/sw2)*(ut-rho*u1t).*u1t;
s_sw = (-0.5/sw2) + ((ut-rho*u1t).*(ut-rho*u1t))/(2*sw2^2);
s_alpha =  ((xt-alpha*y1t).*y1t)/sv2;
s_sv = (-0.5/sv2) + ((xt-alpha*y1t).*(xt-alpha*y1t))/(2*sv2^2);

mat =  [s_beta,s_alpha,s_sw,s_sv,s_rho];
var_mat = cov(mat);
mean_mat =  mean(mat,1);

out = var_mat + (mean_mat'*mean_mat);