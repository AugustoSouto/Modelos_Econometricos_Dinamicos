function [out] =  loglikema1(theta,x)

% This function calculates the log likelihood of an MA(1).

s2=1;

log_like=0;
T = length(x);

% First observation.
log_like = log_like - 0.5*log(2*pi*s2) - 0.5*(x(1,1)-0)^2/s2;
ut=x(1,1);

for t=2:T
   % Compute the log likelihood.
   log_like = log_like - 0.5*log(2*pi*s2) - 0.5*(x(t,1) - theta*ut)^2/s2;
   % Compute ut.
   ut = x(t,1) - theta*ut;
end

out = -log_like;