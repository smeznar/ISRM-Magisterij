function [outF] = OperatorI(f,n,x)
fl = floor(x*n)./n;
ce = ceil(x*n)./n;
mask = (fl == ce);
delta = 1/n;
outF = mask.*f(x)+(1-mask).*(f(fl).*((ce-x)./delta)+f(ce).*((x-fl)/delta));
end

