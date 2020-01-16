function [coefs] = Remes(f,a,b,start,bf,steps)
    for i = 1:steps
        coefs = FindGeneralPolinome(f,bf,start);
        r = @(x) f(x);
        for j=1:length(bf)
            h = bf{j};
            r = @(x) r(x)-coefs(j).*h(x);
        end
        x = findMax(r,a,b);
        start = SwitchPoints(r,start,x);
    end
end

