
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
%markov chain mu is sum of 1 divided by length of the chain
%the normal dist mu is estimated using max likelihood, which also
%uses the mean as a estimator
y=y(BIP+1:T,:) 

% COMPUTE THE MEAN FOR EACH SIMULATION
%binary markov chain
mean(y)

%MEAN ACROSS ALL SIMULATIONS
mu(iter1, iter2)=mean(mean(y));
%MEAN STANDARD DEVIATION
sdev(iter1,iter2)=std(mean(y));
 
histogram(mean(y))

    end
end

mu %in first column, mean estimates for p=0.2 and q=0.6, in the second mean estimates for p=q=0.95
   %the rows are the chain length, with first row using t=100, second t=1000 and last row uses t=10000

sdev %sample variance of mu estimator is reduced as t-->infinity.

% so, mu tends to 2/3 in te first chain parametrization and to 1/2 in the
% second parametrization.

%proposed distribution sd=1, then variance is also =1
toc;