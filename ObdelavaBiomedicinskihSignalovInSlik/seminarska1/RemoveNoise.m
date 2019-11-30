function [idx] = RemoveNoise(Candidates,Signal, Fs)
    r = floor(Fs*0.1);
    interval = 2*r;
    B = floor(Fs*0.025);
    sig = [zeros(1,r),Signal,zeros(1,r)];
    sigDif = abs([zeros(1,B),Signal,zeros(1,B+1)]-[zeros(1,B+1),Signal,zeros(1,B)]);
    counter = 1;
    samples = false(1,length(Candidates));
    for i = Candidates
       u1 = max(sig(1,i:(i+interval))) - min(sig(1,i:(i+interval)));
       u2 = sum(sigDif(1,i:(i+2*B)));
       if u1/u2 > 0.25
           samples(counter) = 1;
       end
       counter = counter+1;
    end
    idx = Candidates(samples);
end

