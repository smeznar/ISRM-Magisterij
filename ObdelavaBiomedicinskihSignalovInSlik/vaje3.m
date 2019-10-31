%% Intro
clear
Fs = 100;
f1 = 20;
f2 = 7;
L = 512;
t = (0:(L-1))*(1/Fs);
t(1)
t(2)
x1 = cos(2*pi*t*f1);
x2 = cos(2*pi*t*f2);
plot(x1)
plot(x1(1:50))
plot(x2(1:50))
x = x1 + x2;
plot(x(1:50))
Y = fft(x);
length(x)
length(Y)
plot(Y)
Y(1)
Y(2)
plot(real(Y))
plot(imag(Y))
plot(abs(Y))
f = (0:(L-1))/L*Fs;
plot(f, abs(Y))
plot(f, angle(Y))
plot(f, 1/L*abs(Y/L).^2)
showSpecs(x, 100)
%% Load Signals
load('s20011m.mat')
sig = val(1,:);
plot(sig)
showSpecs(sig, 250)

%% Filter
a = 1;
b = 1/8*ones(1,8);
fsig = filter(b, a, sig);
plot(sig); hold on; plot(fsig);

d = [1,0,0,0];
h = filter(b,a,d);

d = [1, zeros(1,15)];
h = filter(b,a,d);
showSpecs(h, 250);
H = fft(h, 512);
showSpecsN(h, 250, 512);

%% Example 1
a = 1;
b = [-0.2,-0.2,0.8, -0.2,-0.2];
d = [1 zeros(1,511)];
h = filter(b,a,d);
h(1:10);
showSpecs(h, 250);
freqz(b,a);
[Hw, fw] = freqz(b, a);
plot(fw, abs(Hw));

fsig = filter(b,a,sig);
plot(sig); hold on; plot(fsig);

%% Example 2
alpha = 0.75;
a = [1, -alpha];
b = 1;

d = [1, zeros(1,15)];
h = filter(b, a, d);

showSpecsN(h, 250, 256);

fsig = filter(b, a, sig);
plot(sig); hold on; plot(fsig/4);
