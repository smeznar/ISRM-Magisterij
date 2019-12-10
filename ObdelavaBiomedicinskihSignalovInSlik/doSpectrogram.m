function doSpectrogram(nF) %nF should be a number of a power of two 

Fs = 8000;	% Set the assumed sampling frequency
Nr = 3000;	% Desired signal length in samples (3000 samples, i.e. 3000/8000 seconds)
f1 = 500; 	% Set frequences of all four sinusoids in the signal (500, 800, 1500, 3400 Hz)
f2 = 800;
f3 = 1500;
f4 = 3400;

t = (0:Nr-1)/Fs;		% Vector of times
f = ((0:Nr-1)/Nr)*Fs;	% Vector of frequencies

x1 = cos(2*pi*f1*t);	% First, second, third, and fourth sinusoid
x2 = cos(2*pi*f2*t);
x3 = cos(2*pi*f3*t);
x4 = cos(2*pi*f4*t);

% Now sum all of the signals, however, add x3 only during the first third
% of the total signal and x4 only in the interval from 2/3 to 3/4.
x = x1 + x2;
x(1:Nr/3) = x(1:Nr/3) + x3(1:Nr/3);
x(2*Nr/3:3*Nr/4)=x(2*Nr/3:3*Nr/4)+x4(2*Nr/3:3*Nr/4);

% --- USING INTEGRATED FUNCTIONS ---
% nF/s - overlap - experiment with s=0, nF/4, nF/2, and nF-1
overlap = nF/2;
[B,fr,tm] = spectrogram(x,hanning(nF),overlap,nF,Fs); % FOR MATLAB
%[B,fr,tm] = specgram(x,nF,Fs,hanning(nF),overlap); % FOR OCTAVE (but requires signal package)

% Now draw the spectrogram using logarithmic scale, note that 10*log10(B.^2/nF) = 10*2*log10(abs(B)/nF),
% i.e. plot the power spectrum calculated from the SFFT (short-time FFT)
figure;
imagesc(tm,fr,20*log10(abs(B)/nF));
set(gca,'YDir','normal');
xlabel('Time');
ylabel('Frequency');
title('Spectrogram');

% --- MANUAL METHOD ---
overlap = nF/2; % First set the desired overlap between two consecutive sub-intervals
step = nF - overlap; % Given the overlap, we can calculate the step in time for each next sub-interval
offset = (1:step:length(x)-nF); % Compute the offsets (or starting index) of sub-intervals for the whole signal

% Zero-out the matrix that will contain the final SFFT (short-time FFT) results (i.e. fft for each sub-interval)
Specs = zeros(nF, length(offset));
% Set the window function vector (duration of nF samples, nF=width of each sub-interval)
window = hanning(nF)'; 

% For all sub-intervals (remember, we split the signal into sub-intervals knowing their length,
% total signal duration in samples, and desired overlap in samples)
for i=1:length(offset)
	% Multiply all sub-intervals with the window function
	Specs(1:nF,i) = x(offset(i):offset(i)+nF-1).*window;
end
% Now compute the SFFT for all sub-intervals (for whole matrix, but instead this 
% could also be done individually for each sub-interval inside the above for loop)
Specs = fft(Specs);
Specs = Specs(1:nF/2+1,:); % Use only the first half of the result (symmetric)
fm = (0:nF/2)*Fs/nF; % Construct the frequency vector for Y axis
% t was already set at the beginning

% Draw the spectrogram that was manually computed without using the integrated Matlab/Octave functions
figure;
imagesc(t, fm, 20*log10(abs(Specs)/nF));
set(gca,'YDir','normal');
xlabel('Time');
ylabel('Frequency');
title('Spectrogram 2');

% Now experiment with different window durations to see the effect of using shorter
% or longer fft lengths, and also experiment with different overlaps
end









