function transferCharacteristic6(Fs,k,m,n)

% Transfer characteristic:
% 
% H(z) = (1-z^-k)^m * (1-z^-n)/(1-z^-1)

Nr = 1000;
f = (0:Fs/(Nr-1):Fs);
half = (1:Nr/2);

fn1 = (1-exp(-1*i*2*pi*(f./Fs)*k)).^m; % H1(z)=(1-z^-k)^m
fn2 = (1-exp(-1*i*2*pi*(f./Fs)*n)) ./ (1-exp(-1*i*2*pi*(f./Fs))); % H2(z)=(1-z^-n)/(1-z^-1)

fn3 = fn1.*fn2;

subplot(3,1,1);
plot(f(half), abs(fn1(half)));
subplot(3,1,2);
plot(f(half), abs(fn2(half)));
subplot(3,1,3);
plot(f(half), abs(fn3(half)));

end