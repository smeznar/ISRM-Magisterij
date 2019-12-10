% record s20501, a 1 minute segment of the first signal (lead 0) 
% from 00:02:00 to 00:03:00 with wfdb2mat -r s20501 -f 120 -t 180
load('s20501m.mat');
sig1 = val(1,:); % sig1 is a column vector
sig1 = sig1'; % but to flip the signal using flipud we will need a row vector

Fs = 250;
Nr = length(sig1);
f = [0:(Fs/Nr):Fs];

lw = 1/(Fs/2);
[b,a] = butter(4,lw,'high');

figure;
% Note non-linear phase
freqz(b,a);

% Forward filter
fsig1 = filter(b,a,sig1);
% Flip the result for backward filtering
ffsig1 = flipud(fsig1);
% And now filter again (backward)
ffsig1 = filter(b,a,ffsig1);
% Re-flip the signal (return to forward)
ffsig1 = flipud(ffsig1);

figure;
hold on; 
plot(sig1,'b'); 
plot(fsig1,'r');
plot(ffsig1,'g');
legend('Unfiltered','Forward','Forward+Backward');
hold off;

% See amplitude spectrum of unfiltered and filtered signal
S = fft(sig1);
F = fft(ffsig1);

idx = 1:1000;

figure;
subplot(2,1,1);
plot(f(idx),abs(S(idx)));

subplot(2,1,2);
plot(f(idx),abs(F(idx)));


