function [hF] = HarFilter(Signal, Fs)
    b1 = floor(Fs*0.025);
    b2 = floor(Fs*0.06);
    c = (2*(b2-b1))/(2*b1+1);
    a = [1,-1];
    b = [-1,zeros(1,b2-b1-1),c+1,zeros(1,2*b1),-c-1,zeros(1,b2-b1-1),1];
    sig1 = filter(b,a,[Signal, zeros(1,b2)]);
    hF = sig1(b2+1:(size(sig1,2)));
end
