function [outP] = SwitchPoints(f,points,x)
    if x < points(1)
        if sign(f(x))==sign(f(points(1)))
            outP = [x,points(2:end)];
        else
            outP = [x,points(1:end-1)];
        end
    elseif x > points(end)
        if sign(f(x))==sign(f(points(1)))
            outP = [points(1:end-1),x];
        else
            outP = [points(2:end),x];
        end
    else
        under = points(points<x);
        over = points(points>x);
        if sign(f(x))==sign(f(over(1)))
            outP = [under,x,over(2:end)];
        else
            outP = [under(1:end-1),x,over];
        end
    end
end

