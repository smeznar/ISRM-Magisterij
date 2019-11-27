function [samples, score] = FindCandidates(Signal, Filtered, Fs)
    c = 0.55;
    interval = floor(0.2*Fs);
    sig = [0,2*Signal,0] - [Signal,0,0]-[0,0,Signal];
    score = abs(Filtered.*(Signal+c*sig(1,2:(size(sig,2))-1)));
    hF =[zeros(1, interval),score ,zeros(1, interval)];
    maxes = zeros(1,size(Signal, 2));
    for i = (interval+1):(interval+size(Signal,2))
        [ val , ind] = max (hF((i-interval):(i+interval)));
        maxes(1,i-interval) = i-2*interval+ind-1;
    end
    [gc, gn] = groupcounts(maxes');
    samples = gn(gc>interval/2)';
    %for i = 1:length(samples)
    %   mask = samples(abs(samples-samples(i))<interval);
    %   [a,b] = max(hF(mask));
    %   samples(i) = mask(b);
    %end
    %samples = unique(samples);
    %scatter(samples, zeros(1,size(samples,2)));
    %hold on; plot(Signal);
end