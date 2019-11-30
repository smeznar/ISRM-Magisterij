function [idx] = QRSDetect(fileName, Fs)
  S = load(fileName);
  sig = S.val(1,:);
  lwFreq = 1.6 / Fs;
  n = 400;
  b = fir1(2*n,lwFreq,'high');
  sig = filter(b,1,[sig,zeros(1,n)]);
  sig = sig((n + 1):size(sig,2));
  filtered = HarFilter (sig, Fs);
  [candidates, score] = FindCandidates(sig, filtered, Fs);
  candidates = AdaptiveTrashhold(candidates,score, Fs);
  idx = RemoveNoise(candidates,sig,Fs);
end
