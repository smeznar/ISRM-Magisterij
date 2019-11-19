function [idx] = QRSDetect(fileName,m, normCnst)
  S = load(fileName);
  sig = S.val(1,:);
  filtered = HarFilter (sig, 250);
  [candidates, score] = FindCandidates(sig, filtered, 250);
  candidates
  idx = [1,1];
end
