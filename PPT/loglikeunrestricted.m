function L=loglikeunrestricted(rho,y,x)


T = length(x);
xt = x(2:T,1);
yt = y(2:T,1);
x1 = x(1:T-1,1);
y1 = y(1:T-1,1);


beta = ((yt-rho*y1)'*(xt-rho*x1))/((xt-rho*x1)'*(xt-rho*x1));
alpha = (xt'*y1)/(y1'*y1);
u = y - beta*x;
uhat = u(2:T,1); 
uhat1= u(1:T-1,1);

sw2 =(uhat - rho*uhat1)'*(uhat - rho*uhat1)/(T-1);
sv2 = (xt - alpha*y1)'*(xt - alpha*y1)/(T-1);

L = -(T-1)/2*log(2*pi*sw2) - (yt - beta*xt - rho*(y1 - beta*x1))'*(yt - beta*xt - rho*(y1 - beta*x1))/(2*sw2);
L = L -(T-1)/2*log(2*pi*sv2) - (xt-alpha*y1)'*(xt-alpha*y1)/(2*sv2);

end