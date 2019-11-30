function [samples, score] = FindCandidates(Signal, Filtered, Fs)
    c = 0.55;
    r = floor(0.2*Fs);
    interval = 2*r;
    sig = 2*Signal - circshift(Signal,1)-circshift(Signal, -1);
    score = abs(Filtered.*(Signal+c*sig));
    hF =[zeros(1, r),score ,zeros(1, r)];
    maxes = zeros(1,length(Signal));
    for i = 1:length(Signal)
        [ ~ , ind] = max (hF(i:(i+interval)));
        maxes(i) = i+ind;
    end
    [gc, gn] = groupcounts(maxes');
    samples = gn(gc>r/2)'-(r+1);
end