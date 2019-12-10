function [x] = findMax(r,a,b)
ls = linspace(a,b,1001);
[~,M] = max(abs(r(ls)));
x = ls(M);
end

