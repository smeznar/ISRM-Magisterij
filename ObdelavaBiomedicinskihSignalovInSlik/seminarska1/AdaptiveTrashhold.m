function [samples] = AdaptiveTrashhold(Candidates, Score, Fs)
    W1T = -100000;
    beta1 = 0.6;
    beta2 = 0.5;
    timediff = Fs*10;
    startCandidates = Candidates(Candidates<=timediff);
    startScore = Score(startCandidates);
    startSorted = sort(startScore, 'descend');
    startTresh = startSorted(5)*0.75;
    samples = startCandidates(startScore>startTresh);
    for i=Candidates(Candidates>timediff)
       points = Candidates((0<(i-Candidates)) & ((i-Candidates)<=timediff));
       sp = sort(Score(points), 'descend');
       W1 = W1T+sp(5);
       ftr = filter([1,-1],1,points(length(points)-5:length(points)));
       ftr = ftr(2:length(ftr));
       Ie = sum(ftr.*[3/25,4/25,1/5,6/25,7/25]);
       W2 = beta1+beta2*abs((i-samples(length(samples)))/Ie - round((i-samples(length(samples)))/Ie));
       if abs(Score(i))>W1*W2
           samples(length(samples)+1) = i;
       end
    end
    offset = [samples,0]-[samples(1,1),samples];
    sortedOffset = sort(offset,'descend');
    maxOffset = 2*mean(sortedOffset(1:(length(offset)-2)));
    for i = 1:(length(offset))
        if offset(1,i)>maxOffset
           cands = Candidates(Candidates>samples(i-1) & Candidates<samples(i)); 
           ns = sum(Score(samples([i-1,i])))/3.5;
           cands = cands(Score(cands)>ns);
           samples = [samples,cands];
        end
    end
    samples = sort(samples);
end