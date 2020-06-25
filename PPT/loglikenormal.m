function [out]=loglikenormal(theta,x)

loglike = 0.5*log(2*pi) - 0.5*((x-theta)'*(x-theta));
out = - sum(loglike);