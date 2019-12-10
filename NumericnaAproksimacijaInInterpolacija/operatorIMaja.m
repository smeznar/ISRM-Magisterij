function [outf] = operatorIMaja(f,p,n)
    outf = @(x) 0;
    for i = 2:n-1
        outf = @(x) outf(x) + f(p(i)).*(((x-p(i-1))./(p(i)-p(i-1))).*((p(i-1)<=x) & (x<p(i))));
        outf = @(x) outf(x) + f(p(i)).*(((p(i+1)-x)./(p(i+1)-p(i))).*((p(i)<=x) & (x<=p(i+1))));
    end
    
    outf = @(x) outf(x) +f(p(1)).*(((p(2)-x)./(p(2)-p(1))).*((p(1)<=x) & (x<p(2))));
    outf = @(x) outf(x) +f(p(n)).*(((x-p(n-1))./(p(n)-p(n-1))).*((p(n-1)<x) & (x<=p(n))));

end