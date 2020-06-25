function [output]=newey(x,q,cen_nw)

t=size(x,1);

meanx=mean(x,1);
if cen_nw==1
    u=bsxfun(@minus,x,meanx);
else
    u=x;
end
s=u'*u/t;

i=1;
while i<=q
    c=u(i+1:t,:)'*u(1:t-i,:)/t;
    s=s+(1-(i/(q+1)))*(c+c');
    i=i+1;
end

output=s;

end