
clear all
sim=500;
T=100;
u=normrnd(0,1,[T,sim]);
x=zeros(T,sim,11);

th_ma1=zeros(sim,1);
lb=-0.99;
ub=0.99;
options = optimset('display','off','TolX',1e-15,'Algorithm','interior-point');


for i =1:sim
x(1,i,:)=u(1,i);
end

rho=linspace(-0.5,0.5,11);

for rh=1:11
for i=1:sim
for t=2:T
            x(t,i,rh) = rho(rh)*x(t-1,i) + u(t,i);
end
end
end

rho_ml=zeros( sim, 11);

for rh=1:11
 for j=1:sim
rho_ml(j,rh) = (x(2:T,j,rh)'*x(1:T-1,j,rh))/(x(1:T-1,j,rh)'*x(1:T-1,j,rh));
 end
end


for i=1:11
    disp(i)
    for j=1:sim                        % MA(1) estimates.
        [th_ma1(j,1),~,~]=fmincon(@(th_ma1)loglikema1(th_ma1,x),0,[],[],[],[],lb,ub,[],options);        
    end
end
