function [out]=implied_ma(rho) % esto es lo que hay en la diapo 6 del lab

if rho <=-0.5
    out = -1;
elseif rho >=0.5
    out =  1;
elseif (rho>-0.5 && rho<=0)
    out = (0.5/rho) + sqrt((0.5/rho)^2 -1);
elseif (rho>0 && rho < 0.5)
    out  = (0.5/rho) - sqrt((0.5/rho)^2 -1);
end