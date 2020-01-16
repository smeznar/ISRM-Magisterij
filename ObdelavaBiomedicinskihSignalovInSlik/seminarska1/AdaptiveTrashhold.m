function [idx] = AdaptiveTrashhold(Candidates, Score)
    W1T = -100000;
    beta1 = 0.6;
    beta2 = 0.5;
    startScore = Score(Candidates(1:25));
    startSorted = sort(startScore, 'descend');
    startTresh = startSorted(5)*0.75;
    samples = false(1,length(Candidates));
    samples(1:25) = startScore>startTresh;
    s = sum(samples(1:25));
    lastn = Candidates(logical(samples));
    lastn = lastn(max(1,length(lastn)-5):end);
    last = lastn(end);
    lastn = (lastn - circshift(lastn,1));
    lastn = lastn(2:end);
    for i=1:(length(Candidates)-25)
        points = Candidates(i:i+24);
        currInd = i + 25;
        point = Candidates(currInd);
        sp = sort(Score(points), 'descend');
        if (s>5)
            W1 = sp(5) + W1T;
        else
            W1 = sp(5) + 3*W1T;
        end
        Ie = sum(lastn)/5;
        e = (point-last)/Ie;
        W2 = beta1+beta2*abs(e - round(e));
        if abs(Score(point))>W1*W2
            samples(currInd) = 1;
            lastn = circshift(lastn, -1);
            lastn(end) = point-last;
            last = point;
        end
        s = s - samples(i)+samples(i+25);
    end
    ids = Candidates(samples);
    offset = ids-circshift(ids,1);
    sortedOffset = sort(offset,'descend');
    maxOffset = 2*mean(sortedOffset(1:(length(offset)-2)));
    for i = 1:(length(offset))
        if offset(1,i)>maxOffset
           cands = Candidates(Candidates>samples(i-1) & Candidates<samples(i)); 
           ns = median(Score(samples([i-1,i])));
           cands = cands(Score(cands)>ns);
           samples(cands) = 1;
        end
    end
    idx = Candidates(samples);
end