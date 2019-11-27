function [idx] = QRSDetect(fileName, Fs)
  S = load(fileName);
  sig = S.val(1,:);
  lwFreq = 1.6 / Fs;
  n = 800;
  b = fir1(n,lwFreq,'high');
  sig = filter(b,1,[sig,zeros(1,n/2)]);
  sig = sig((n/2 + 1):size(sig,2));
  filtered = HarFilter (sig, Fs);
  [candidates, score] = FindCandidates(sig, filtered, Fs);
  candidates = AdaptiveTrashhold(candidates,score, Fs);
  %scatter(candidates(candidates < 10000),zeros(1,size(candidates(candidates < 10000),2)))
  idx = RemoveNoise(candidates,sig,Fs);
end
