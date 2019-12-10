function [out] = BersteinPolinom(f,n)
out = @(x) 0;
for i=0:n
    out = @(x) out(x)+(nchoosek(n,i).*(x.^i).*(1-x).^(n-i))*(f(i/n));
end
end

