function [samples] = RemoveNoise(Candidates,Signal, Fs)
    interval = floor(Fs*0.1);
    B = floor(Fs*0.025);
    sig = [zeros(1,interval),Signal,zeros(1,interval)];
    sigDif = abs([zeros(1,B),Signal,zeros(1,B+1)]-[zeros(1,B+1),Signal,zeros(1,B)]);
    counter = 1;
    samples = [];
    for i = Candidates
       u1 = max(sig(1,i:(i+2*interval))) - min(sig(1,i:(i+2*interval)));
       u2 = sum(sigDif(1,i:(i+2*B)));
       if u1/u2 > 0.3
           samples(1,counter) = i;
           counter = counter+1;
       end
    end
end

