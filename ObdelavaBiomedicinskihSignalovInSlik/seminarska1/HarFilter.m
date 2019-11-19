function [hF] = HarFilter(Signal, Fs)
    %m = mean(Signal);
    b1 = floor(Fs*0.025);
    b2 = floor(Fs*0.06);
    c = (2*(b2-b1))/(2*b1+1);
    sig = [(zeros(1,b2)),Signal,(zeros(1,b2))];
    sp = 0;
    for i=1:(2*b2+1)
        if (abs(b2+1-i) > b1)
            sp = sp - sig (1,i);
        else
            sp = sp + c*sig(1,i);
        end
    end
    hF = zeros(1,size(Signal,2));
    hF (1,1) = sp;
    for i=(b2+2):(b2+size(Signal,2))
        curr = i-b2;
        hF (1, curr) = hF (1, curr-1);
        hF (1, curr) = hF (1, curr) + sig(1,i-b2-1);
        hF (1, curr) = hF (1, curr) - (c+1)*sig(1,i-b1-1);
        hF (1, curr) = hF (1, curr) + (c+1)*sig(1,i+b1);
        hF (1, curr) = hF (1, curr) - sig(1,i+b2);
    end
end
